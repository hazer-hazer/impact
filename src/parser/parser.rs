use crate::{
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    pp::PP,
    session::{OkStageResult, Session, Stage, StageResult},
    span::span::{Ident, Kw, Span, WithSpan},
    ast::{
        expr::{Expr, ExprKind, InfixOpKind, Lit, PrefixOpKind},
        stmt::{Stmt, StmtKind},
        ErrorNode, AST, N, PR,
    },
};

use super::{
    token::{Infix, Prefix, Punct, Token, TokenCmp, TokenKind, TokenStream},
};

pub struct Parser {
    sess: Session,
    pos: usize,
    tokens: TokenStream,
    msg: MessageStorage,
}

impl MessageHolder for Parser {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl Parser {
    pub fn new(sess: Session, tokens: TokenStream) -> Self {
        Self {
            sess,
            tokens,
            pos: 0,
            msg: MessageStorage::default(),
        }
    }

    fn eof(&self) -> bool {
        TokenCmp::Eof == self.peek()
    }

    fn peek_tok_at(&self, pos: usize) -> Token {
        self.tokens[pos]
    }

    fn peek_tok(&self) -> Token {
        self.peek_tok_at(self.pos)
    }

    fn peek_at(&self, pos: usize) -> TokenKind {
        self.peek_tok_at(pos).kind
    }

    fn peek(&self) -> TokenKind {
        self.peek_at(self.pos)
    }

    fn span(&self) -> Span {
        self.peek_tok().span
    }

    fn is(&self, cmp: TokenCmp) -> bool {
        cmp == self.peek()
    }

    fn advance_offset_tok(&mut self, offset: usize) -> Token {
        let last = self.pos;
        self.pos += offset;
        self.peek_tok_at(last)
    }

    fn advance_tok(&mut self) -> Token {
        self.advance_offset_tok(1)
    }

    fn advance_offset(&mut self, offset: usize) -> TokenKind {
        self.advance_offset_tok(offset).kind
    }

    fn advance(&mut self) -> TokenKind {
        self.advance_offset(1)
    }

    fn expected_pr<'a, T>(&'a mut self, entity: Option<PR<N<T>>>, expected: &str) -> PR<N<T>>
    where
        T: PP<'a> + WithSpan,
    {
        if let Some(entity) = entity {
            entity
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!(
                    "Expected {}, got {}",
                    expected,
                    self.peek().ppfmt(&self.sess)
                ))
                .emit(self);
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expected<'a, T>(&'a mut self, entity: Option<T>, expected: &str) -> PR<T>
    where
        T: PP<'a> + WithSpan,
    {
        if let Some(entity) = entity {
            Ok(entity)
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!(
                    "Expected {}, got {}",
                    expected,
                    self.peek().ppfmt(&self.sess)
                ))
                .emit(self);
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expect<'a, T>(&'a mut self, entity: Option<T>, expected: &str)
    where
        T: PP<'a> + WithSpan,
    {
        if entity.is_none() {
            MessageBuilder::error()
                .span(self.span())
                .text(format!(
                    "Expected {}, got {}",
                    expected,
                    self.peek().ppfmt(&self.sess)
                ))
                .emit(self);
        }
    }

    // Sadly, cause of problems with exclusive borrowing,
    // it is simpler to make predicate implicitly check for peek in implementation points
    fn skip_if<F>(&mut self, pred: F) -> Option<Token>
    where
        F: Fn(TokenKind) -> bool,
    {
        if pred(self.peek()) {
            Some(self.advance_tok())
        } else {
            None
        }
    }

    fn skip_many_if<F>(&mut self, pred: F) -> Option<Vec<Token>>
    where
        F: Fn(TokenKind) -> bool,
    {
        let mut tokens = Vec::<Token>::new();

        while pred(self.peek()) {
            tokens.push(self.advance_tok());
        }

        if tokens.is_empty() {
            None
        } else {
            Some(tokens)
        }
    }

    fn skip(&mut self, cmp: TokenCmp) -> Option<Token> {
        self.skip_if(|kind| cmp == kind)
    }

    fn skip_many(&mut self, cmp: TokenCmp) -> Option<Vec<Token>> {
        self.skip_many_if(|kind| cmp == kind)
    }

    fn skip_any(&mut self, cmp: &[TokenCmp]) -> Option<Token> {
        self.skip_if(|kind| cmp.iter().any(|cmp| *cmp == kind))
    }

    fn skip_nls(&mut self) -> bool {
        let mut nl = false;
        while let Some(_) = self.skip_if(|kind| kind == TokenKind::Nl) {
            nl = true;
        }
        nl
    }

    fn skip_prefix(&mut self, prefix: Prefix) -> Option<Token> {
        self.skip_if(|kind| TokenCmp::Prefix(prefix) == kind)
    }

    fn skip_kw(&mut self, kw: Kw) -> Option<Token> {
        self.skip_if(|kind| TokenCmp::Kw(kw) == kind)
    }

    fn skip_punct(&mut self, punct: Punct) -> Option<Token> {
        self.skip_if(|kind| TokenCmp::Punct(punct) == kind)
    }

    fn parse_multiple(&mut self, cmp: TokenCmp) -> Vec<Token> {
        let mut items: Vec<Token> = Default::default();

        while !self.eof() {
            if self.is(cmp) {
                items.push(self.advance_tok());
            } else {
                break;
            }
        }

        items
    }

    fn parse_delim<T, P>(
        &mut self,
        begin: TokenCmp,
        delim: TokenCmp,
        end: TokenCmp,
        mut parser: P,
    ) -> Vec<T>
    where
        P: FnMut() -> T,
    {
        let skip = self.skip(begin);
        self.expect(skip, format!("{}", begin).as_str());

        let mut els = Vec::<T>::new();

        let mut first = true;
        while !self.eof() {
            if end == self.peek() {
                break;
            }

            if first {
                first = false;
            } else {
                let skip = self.skip(delim);
                self.expect(skip, format!("{} delimiter", delim).as_str());
            }

            if end == self.peek() {
                // TODO: Trailing?
                break;
            }

            els.push(parser());
        }

        let skip = self.skip(end);
        self.expect(skip, format!("{}", end).as_str());

        els
    }

    fn parse_ident(&mut self, expected: &str) -> PR<Ident> {
        let skip = self.skip(TokenCmp::Ident).map(|tok| Ident::from_token(tok));
        self.expected(skip, expected)
    }

    fn parse_stmt(&mut self) -> PR<N<Stmt>> {
        if let Some(expr) = self.parse_expr() {
            Ok(Box::new(Stmt::new(expr.span(), StmtKind::Expr(expr))))
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!(
                    "Unexpected token {}",
                    self.peek().ppfmt(&self.sess)
                ))
                .emit(self);
            Err(ErrorNode::new(self.advance_tok().span))
        }
    }

    fn parse_expr(&mut self) -> Option<PR<N<Expr>>> {
        if TokenCmp::Kw(Kw::Let) == self.peek() {
            return Some(self.parse_let());
        }

        self.parse_prec(0)
    }

    fn parse_let(&mut self) -> PR<N<Expr>> {
        let lo = self.span();

        let skip = self.skip_kw(Kw::Let);
        self.expect(skip, "`let` keyword");

        let name = self.parse_ident("variable name");

        let value = self.parse_expr();
        let value = self.expected_pr(value, "value");

        let body = self.parse_expr();
        let body = self.expected_pr(body, "body");

        // There might be a better way to slice vector
        Ok(Box::new(Expr::new(
            lo.to(self.span()),
            ExprKind::Let(name, value, body),
        )))
    }

    fn parse_prec(&mut self, prec: u8) -> Option<PR<N<Expr>>> {
        const PREC_TABLE: &[&[TokenCmp]] = &[
            &[TokenCmp::Infix(Infix::Plus), TokenCmp::Infix(Infix::Minus)],
            &[
                TokenCmp::Infix(Infix::Mul),
                TokenCmp::Infix(Infix::Div),
                TokenCmp::Infix(Infix::Mod),
            ],
        ];

        if prec as usize == PREC_TABLE.len() {
            return self.parse_prefix();
        }

        let lo = self.span();

        let mut lhs = self.parse_prec(prec + 1)?;

        while let Some(op) = self.skip_any(&PREC_TABLE[prec as usize]) {
            let rhs = self.parse_prec(prec + 1);

            if let Some(rhs) = rhs {
                lhs = Ok(Box::new(Expr::new(
                    lo.to(self.span()),
                    ExprKind::Infix(lhs, InfixOpKind::from_tok(op), rhs),
                )));
            } else {
                break;
            }
        }

        Some(lhs)
    }

    fn parse_prefix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();
        if let Some(op) = self.skip(TokenCmp::SomePrefix) {
            let rhs = self.parse_postfix();

            let rhs = if let Some(rhs) = rhs {
                rhs
            } else {
                MessageBuilder::error()
                    .span(self.span())
                    .text(format!(
                        "Expected expression after {} operator",
                        op.kind.ppfmt(&self.sess)
                    ))
                    .emit(self);
                Err(ErrorNode::new(self.span()))
            };

            Some(Ok(Box::new(Expr::new(
                op.span.to(lo),
                ExprKind::Prefix(PrefixOpKind::from_tok(&op), rhs),
            ))))
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let lhs = self.parse_primary();

        if lhs.is_none() {
            return None;
        }

        let mut args: Vec<PR<N<Expr>>> = Vec::default();
        while let Some(expr) = self.parse_primary() {
            args.push(expr);
        }

        if args.is_empty() {
            lhs
        } else if let Some(lhs) = lhs {
            Some(Ok(Box::new(Expr::new(
                lo.to(self.span()),
                ExprKind::App(lhs, args),
            ))))
        } else {
            unreachable!()
        }
    }

    fn parse_primary(&mut self) -> Option<PR<N<Expr>>> {
        let Token { kind, span } = self.peek_tok();

        if TokenCmp::Nl == self.peek() {
            return self.parse_block();
        }

        if TokenCmp::Punct(Punct::Backslash) == self.peek() {
            return self.parse_abs();
        }

        let kind = match kind {
            TokenKind::Bool(val) => Some(Ok(ExprKind::Lit(Lit::Bool(val)))),
            TokenKind::Int(val) => Some(Ok(ExprKind::Lit(Lit::Int(val)))),
            TokenKind::String(sym) => Some(Ok(ExprKind::Lit(Lit::String(sym)))),
            TokenKind::Ident(sym) => Some(Ok(ExprKind::Ident(Ident::new(span, sym)))),

            // Error token is an error on lexing stage
            //  so don't emit one more error for it, just add error stub
            TokenKind::Error(_) => Some(Err(ErrorNode::new(span))),

            _ => None,
        };

        if let Some(_) = kind {
            self.advance();
        }

        kind.map(|k| k.map(|k| Box::new(Expr::new(span, k))))
    }

    fn parse_block(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let mut stmts = vec![];

        if self.skip_nls() {
            if self.eof() {
                return None;
            }

            let skip = self.skip(TokenCmp::Indent);
            self.expect(skip, "indent");

            let mut first = true;
            while !self.eof() {
                if self.eof() || TokenCmp::Dedent == self.peek() {
                    break;
                }

                if first {
                    first = false;
                } else {
                    let skip = self.skip(TokenCmp::Nl);
                    self.expect(skip, "line break");
                }

                if self.eof() || TokenCmp::Dedent == self.peek() {
                    break;
                }

                stmts.push(self.parse_stmt());
            }

            if !self.eof() {
                let skip = self.skip(TokenCmp::Dedent);
                self.expect(skip, "dedent");
            }
        } else {
            stmts = vec![self.parse_stmt()];
        }

        Some(Ok(Box::new(Expr::new(
            lo.to(self.span()),
            ExprKind::Block(stmts),
        ))))
    }

    fn parse_abs(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        self.skip(TokenCmp::Punct(Punct::Backslash));

        let param = self.parse_ident("parameter name");

        self.skip(TokenCmp::Punct(Punct::Arrow));

        let body = self.parse_expr();
        let body = self.expected_pr(body, "lambda body");

        Some(Ok(Box::new(Expr::new(
            lo.to(self.span()),
            ExprKind::Abs(param, body),
        ))))
    }

    fn parse(&mut self) -> AST {
        let mut stmts = vec![];

        self.skip_nls();
        while !self.eof() {
            stmts.push(self.parse_stmt());
        }

        AST::new(stmts)
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(mut self) -> StageResult<AST> {
        let ast = self.parse();
        StageResult::new(self.sess, ast, self.msg)
    }

    fn run_and_unwrap(self) -> OkStageResult<AST> {
        self.run().unwrap()
    }
}

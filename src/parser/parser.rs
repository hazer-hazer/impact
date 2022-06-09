use crate::{
    message::{
        message::{Message, MessageBuilder, MessageHolder, MessageStorage},
        MessageEmitter,
    },
    pp::PP,
    session::{Session, Stage, StageResult},
    span::span::{Ident, Kw, Span, WithSpan},
};

use super::{
    ast::{
        expr::{Expr, ExprKind, InfixOpKind, Lit, PrefixOpKind},
        stmt::{LetStmt, StmtKind},
        ErrorNode, AST, N, PR,
    },
    token::{Infix, Prefix, Punct, Token, TokenCmp, TokenKind, TokenStream},
};

struct Parser {
    sess: Session,
    pos: usize,
    tokens: TokenStream,
    msg: MessageStorage,
    prec_table: Vec<Vec<TokenCmp>>,
}

impl MessageHolder for Parser {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl Parser {
    pub fn new(sess: Session, tokens: TokenStream) -> Self {
        let prec_table = vec![
            vec![TokenCmp::Infix(Infix::Plus), TokenCmp::Infix(Infix::Minus)],
            vec![
                TokenCmp::Infix(Infix::Mul),
                TokenCmp::Infix(Infix::Div),
                TokenCmp::Infix(Infix::Mod),
            ],
        ];

        Self {
            sess,
            tokens,
            pos: 0,
            msg: MessageStorage::default(),
            prec_table,
        }
    }

    fn eof(&self) -> bool {
        TokenCmp::Eof == *self.peek()
    }

    fn peek_tok_at(&self, pos: usize) -> &Token {
        &self.tokens[pos]
    }

    fn peek_tok(&self) -> &Token {
        self.peek_tok_at(self.pos)
    }

    fn peek_at(&self, pos: usize) -> &TokenKind {
        &self.peek_tok_at(pos).kind
    }

    fn peek(&self) -> &TokenKind {
        self.peek_at(self.pos)
    }

    fn span(&self) -> Span {
        self.peek_tok().span
    }

    fn is(&self, cmp: TokenCmp) -> bool {
        cmp == *self.peek()
    }

    fn advance_offset_tok(&mut self, offset: usize) -> &Token {
        let last = self.pos;
        self.pos += offset;
        self.peek_tok_at(last)
    }

    fn advance_tok(&mut self) -> &Token {
        self.advance_offset_tok(1)
    }

    fn advance_offset(&mut self, offset: usize) -> &TokenKind {
        &self.advance_offset_tok(offset).kind
    }

    fn advance(&mut self) -> &TokenKind {
        &self.advance_offset(1)
    }

    fn expected_pr<'a, T>(&'a mut self, entity: Option<PR<N<T>>>, expected: &str) -> PR<N<T>>
    where
    T: PP<'a> + WithSpan
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

    fn expected<'a, T>(&'a mut self, entity: Option<&'a T>, expected: &str) -> PR<&'a T>
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

    // Sadly, cause of problems with exclusive borrowing,
    // it is simpler to make predicate implicitly check for peek in implementation points
    fn skip_if<F>(&mut self, pred: F) -> Option<&Token>
    where
        F: Fn(&TokenKind) -> bool,
    {
        if pred(self.peek()) {
            Some(self.advance_tok())
        } else {
            None
        }
    }

    fn skip(&mut self, cmp: TokenCmp) -> Option<&Token> {
        self.skip_if(|kind| cmp == *kind)
    }

    fn skip_any(&mut self, cmp: &[TokenCmp]) -> Option<&Token> {
        self.skip_if(|kind| cmp.iter().any(|cmp| *cmp == *kind))
    }

    fn skip_prefix(&mut self, prefix: Prefix) -> Option<&Token> {
        self.skip_if(|kind| TokenCmp::Prefix(prefix) == *kind)
    }

    fn skip_kw(&mut self, kw: Kw) -> Option<&Token> {
        self.skip_if(|kind| TokenCmp::Kw(kw) == *kind)
    }

    fn skip_punct(&mut self, punct: Punct) -> Option<&Token> {
        self.skip_if(|kind| TokenCmp::Punct(punct) == *kind)
    }

    fn parse_multiple(&mut self, cmp: TokenCmp) -> Vec<Token> {
        let items: Vec<Token> = Default::default();

        while !self.eof() {
            if self.is(cmp) {
                items.push(*self.peek_tok().clone());
            }
        }

        items
    }

    fn parse_let(&mut self) -> StmtKind {
        self.expected(self.skip_kw(Kw::Let), "`let` keyword");

        let idents = self
            .parse_multiple(TokenCmp::Ident)
            .iter()
            .map(|tok| Ident::from_token(*tok))
            .collect::<Vec<_>>();

        let (name, params) = match idents.as_slice() {
            [] => {
                MessageBuilder::error()
                    .span(self.span())
                    .text("Expected variable name and optional list of parameters".to_string())
                    .emit(self);
                (Err(ErrorNode::new(self.span())), vec![])
            }
            [name] => (Ok(*name), vec![]),
            [name, params @ ..] => (Ok(*name), params.to_vec()),
        };

        self.expected(self.skip_punct(Punct::Assign), "");

        let value = self.expected_pr(
            self.parse_expr(),
            format!(
                "{}",
                match (name, params.len()) {
                    (Ok(_), 0) => "variable value",
                    (Ok(_), x) if x > 1 => "function body",
                    (Err(_), _) => "function body or variable value",
                    _ => unreachable!(),
                }
            )
            .as_str(),
        );

        // There might be a better way to slice vector
        StmtKind::Let(LetStmt::new(name, params, value))
    }

    fn parse_stmt(&mut self) {
        if self.peek().is_kw(&self.sess, Kw::Let) {
            self.parse_let();
        }
    }

    fn parse_expr(&mut self) -> Option<PR<N<Expr>>> {
        self.parse_prec(0)
    }

    fn parse_prec(&mut self, prec: u8) -> Option<PR<N<Expr>>> {
        if prec as usize >= self.prec_table.len() {
            return self.parse_prefix();
        }

        let lo = self.span();

        let mut lhs = self.parse_prec(prec + 1)?;

        while let Some(op) = self.skip_any(&self.prec_table[prec as usize]) {
            let rhs = self.parse_prec(prec + 1);

            if let Some(rhs) = rhs {
                lhs = Ok(Box::new(Expr::new(
                    lo.to(self.span()),
                    ExprKind::Infix(lhs, InfixOpKind::from_tok(*op), rhs),
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
            let rhs = self.parse_primary();

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
                ExprKind::Prefix(PrefixOpKind::from_tok(*op), rhs),
            ))))
        } else {
            None
        }
    }

    fn parse_postfix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let lhs = self.parse_primary();

        while let Some(expr) = self.parse_expr() {}

        lhs
    }

    fn parse_primary(&mut self) -> Option<PR<N<Expr>>> {
        let Token { kind, span } = self.advance_tok();

        let kind = match kind {
            TokenKind::Bool(val) => Some(Ok(ExprKind::Lit(Lit::Bool(*val)))),
            TokenKind::Int(val) => Some(Ok(ExprKind::Lit(Lit::Int(*val)))),
            TokenKind::String(sym) => Some(Ok(ExprKind::Lit(Lit::String(*sym)))),
            TokenKind::Ident(sym) => Some(Ok(ExprKind::Ident(Ident::new(*span, *sym)))),

            // Error token is an error on lexing stage
            //  so don't emit one more error for it, just add error stub
            TokenKind::Error(_) => Some(Err(ErrorNode::new(*span))),

            _ => None,
            // TODO: Move to top-level check
            // _ => {
            //     MessageBuilder::error()
            //         .span(*span)
            //         .text(format!("Unexpected token {}", kind.ppfmt(&self.sess)))
            //         .emit(self);
            //     Err(ErrorNode::new(*span))
            // }
        };

        kind.map(|k| k.map(|k| Box::new(Expr::new(*span, k))))
    }

    fn parse(&mut self) -> AST {
        AST::new(vec![])
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(mut self) -> StageResult<AST> {
        let ast = self.parse();
        StageResult::new(self.sess, ast, self.msg)
    }

    fn run_and_unwrap(
        self,
        emitter: &mut impl MessageEmitter,
    ) -> crate::session::OkStageResult<AST> {
        self.run().unwrap(emitter)
    }
}

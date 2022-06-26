use crate::{
    ast::{
        expr::{Expr, ExprKind, InfixOpKind, Lit, PrefixOpKind},
        item::{Item, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        ErrorNode, NodeId, AST, N, PR,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Kw, Span, WithSpan},
};

use super::token::{Infix, Prefix, Punct, Token, TokenCmp, TokenKind, TokenStream};

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

macro_rules! parse_block_common {
    ($self: ident, $parse: ident, $parse_inline: expr) => {{
        let mut entities = vec![];

        if $self.skip_nls() && !$self.eof() && $self.is(TokenCmp::Indent) && !$self.eof() {
            $self.just_skip(TokenCmp::Indent);

            while !$self.is(TokenCmp::Dedent) {
                if $self.eof() {
                    break;
                }

                entities.push($self.$parse());
            }

            if !$self.eof() {
                $self.just_skip(TokenCmp::Dedent);
            }
        } else if $parse_inline {
            entities.push($self.$parse());
        }

        entities
    }};
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

    fn next_node_id(&mut self) -> NodeId {
        self.sess.next_node_id()
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

    fn just_skip(&mut self, cmp: TokenCmp) {
        self.expect(cmp)
            .expect(format!("[BUG] Failed to just skip expected token {}", cmp).as_str())
    }

    fn unexpected_token(&mut self) -> ErrorNode {
        MessageBuilder::error()
            .span(self.span())
            .text(format!("Unexpected token {}", self.peek()))
            .emit(self);
        ErrorNode::new(self.advance_tok().span)
    }

    fn expect(&mut self, cmp: TokenCmp) -> PR<()> {
        if self.is(cmp) {
            self.advance();
            Ok(())
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!("Expected {}, got {}", cmp, self.peek()))
                .emit(self);
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expect_semi(&mut self) -> PR<()> {
        // If we encountered EOF don't skip it
        if self.eof() {
            Ok(())
        } else {
            self.expect(TokenCmp::Nls)
        }
    }

    fn expect_kw(&mut self, kw: Kw) -> PR<()> {
        self.expect(TokenCmp::Kw(kw))
    }

    fn expect_punct(&mut self, punct: Punct) -> PR<()> {
        self.expect(TokenCmp::Punct(punct))
    }

    fn expected_pr<'a, T>(&'a mut self, entity: Option<PR<N<T>>>, expected: &str) -> PR<N<T>>
    where
        T: WithSpan,
    {
        if let Some(entity) = entity {
            entity
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!("Expected {}, got {}", expected, self.peek()))
                .emit(self);
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expected<'a, T>(&'a mut self, entity: Option<T>, expected: &str) -> PR<T>
    where
        T: WithSpan,
    {
        if let Some(entity) = entity {
            Ok(entity)
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!("Expected {}, got {}", expected, self.peek()))
                .emit(self);
            Err(ErrorNode::new(self.span()))
        }
    }

    fn try_recover_any<T>(&mut self, entity: Option<PR<N<T>>>, expected: &str) -> PR<N<T>>
    where
        T: WithSpan,
    {
        if let Some(entity) = entity {
            entity
        } else {
            match self.parse_stmt() {
                Ok(stmt) => {
                    MessageBuilder::error()
                        .span(self.span())
                        .text(format!("Expected {}, got {}", expected, stmt))
                        .emit(self);
                    Err(ErrorNode::new(stmt.span()))
                }
                Err(err) => {
                    MessageBuilder::error()
                        .span(self.span())
                        .text(format!(
                            "Unexpected token {}, where {} expected",
                            self.peek(),
                            expected
                        ))
                        .emit(self);
                    Err(ErrorNode::new(self.advance_tok().span()))
                }
            }
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

    fn parse_ident(&mut self, expected: &str) -> PR<Ident> {
        let skip = self.skip(TokenCmp::Ident).map(|tok| Ident::from_token(tok));
        self.expected(skip, expected)
    }

    // Statements //
    fn parse_stmt(&mut self) -> PR<N<Stmt>> {
        if let Some(item) = self.parse_opt_item() {
            let span = item.span();
            Ok(Box::new(Stmt::new(
                self.next_node_id(),
                StmtKind::Item(item),
                span,
            )))
        } else if let Some(expr) = self.parse_opt_expr() {
            let span = expr.span();
            self.expect_semi()?;
            Ok(Box::new(Stmt::new(
                self.next_node_id(),
                StmtKind::Expr(expr),
                span,
            )))
        } else {
            Err(self.unexpected_token())
        }
    }

    // Items //
    fn parse_item(&mut self) -> PR<N<Item>> {
        let item = self.parse_opt_item();
    }

    fn parse_opt_item(&mut self) -> Option<PR<N<Item>>> {
        if self.is(TokenCmp::Kw(Kw::Mod)) {
            Some(self.parse_mod_item())
        } else if self.is(TokenCmp::Kw(Kw::Type)) {
            Some(self.parse_type_item())
        } else {
            None
        }
    }

    fn parse_mod_item(&mut self) -> PR<N<Item>> {
        let lo = self.span();

        self.expect_kw(Kw::Mod)?;

        let name = self.parse_ident("module name");

        let items = parse_block_common!(self, parse_item, false);

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Mod(name, items),
            lo.to(self.span()),
        )))
    }

    fn parse_type_item(&mut self) -> PR<N<Item>> {
        let lo = self.span();

        self.expect_kw(Kw::Type)?;

        let name = self.parse_ident("type name");

        self.expect_punct(Punct::Assign)?;

        let ty = self.parse_ty();

        self.expect_semi()?;

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Type(name, ty),
            lo.to(self.span()),
        )))
    }

    // Expressions //
    fn parse_expr(&mut self) -> PR<N<Expr>> {
        let expr = self.parse_opt_expr();
        self.expected_pr(expr, "expression")
    }

    fn parse_opt_expr(&mut self) -> Option<PR<N<Expr>>> {
        if self.is(TokenCmp::Kw(Kw::Let)) {
            return Some(self.parse_let());
        }

        self.parse_prec(0)
    }

    fn parse_let(&mut self) -> PR<N<Expr>> {
        let lo = self.span();

        self.expect_kw(Kw::Let)?;

        let name = self.parse_ident("variable name");

        self.expect_punct(Punct::Assign)?;

        let value = self.parse_expr();

        self.expect_kw(Kw::In)?;

        let body = self.parse_expr();

        // There might be a better way to slice vector
        Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Let(name, value, body),
            lo.to(self.span()),
        )))
    }

    fn parse_prec(&mut self, prec: u8) -> Option<PR<N<Expr>>> {
        const PREC_TABLE: &[&[TokenCmp]] = &[
            &[TokenCmp::Punct(Punct::Colon)],
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
            if op.kind == TokenKind::Punct(Punct::Colon) {
                // Parse ascription (type expression)
                let ty = self.parse_ty();
                lhs = Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::Ty(lhs, ty),
                    lo.to(self.span()),
                )));

                // TODO: Allow ascription of ascription?
                break;
            } else {
                let rhs = self.parse_prec(prec + 1);

                if let Some(rhs) = rhs {
                    lhs = Ok(Box::new(Expr::new(
                        self.next_node_id(),
                        ExprKind::Infix(lhs, InfixOpKind::from_tok(op), rhs),
                        lo.to(self.span()),
                    )));
                } else {
                    break;
                }
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
                    .text(format!("Expected expression after {} operator", op.kind))
                    .emit(self);
                Err(ErrorNode::new(self.span()))
            };

            Some(Ok(Box::new(Expr::new(
                self.next_node_id(),
                ExprKind::Prefix(PrefixOpKind::from_tok(&op), rhs),
                op.span.to(lo),
            ))))
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let lhs = self.parse_primary();

        if let Some(lhs) = lhs {
            let arg = self.parse_primary();

            if let Some(arg) = arg {
                Some(Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::App(lhs, arg),
                    lo.to(self.span()),
                ))))
            } else {
                Some(lhs)
            }
        } else {
            lhs
        }

        // let mut args: Vec<PR<N<Expr>>> = Vec::default();
        // while let Some(expr) = self.parse_primary() {
        //     args.push(expr);
        // }
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

        kind.map(|k| k.map(|k| Box::new(Expr::new(self.next_node_id(), k, span))))
    }

    fn parse_block(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let stmts = parse_block_common!(self, parse_stmt, true);

        Some(Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Block(stmts),
            lo.to(self.span()),
        ))))
    }

    fn parse_abs(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        self.skip(TokenCmp::Punct(Punct::Backslash));

        let param = self.parse_ident("parameter name");

        self.skip(TokenCmp::Punct(Punct::Arrow));

        let body = self.parse_expr();

        Some(Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Abs(param, body),
            lo.to(self.span()),
        ))))
    }

    // Types //
    fn parse_ty(&mut self) -> PR<N<Ty>> {
        let ty = self.parse_opt_ty();
        self.expected_pr(ty, "type")
    }

    fn parse_opt_ty(&mut self) -> Option<PR<N<Ty>>> {
        let lo = self.span();

        let kind = match self.peek() {
            TokenKind::Punct(Punct::LParen) => {
                self.advance();
                let inner = self.parse_opt_ty();
                self.skip(TokenCmp::Punct(Punct::RParen));

                match inner {
                    Some(inner) => TyKind::Paren(inner),
                    None => TyKind::Unit,
                }
            }

            // TODO: Literal types
            TokenKind::Ident(_) => TyKind::Var(self.parse_ident("type name")),

            _ => {
                return None;
            }
        };

        let ty = Ok(Box::new(Ty::new(
            self.next_node_id(),
            kind,
            lo.to(self.span()),
        )));

        if self.skip_punct(Punct::Arrow).is_some() {
            let return_ty = self.parse_ty();
            Some(Ok(Box::new(Ty::new(
                self.next_node_id(),
                TyKind::Func(ty, return_ty),
                lo.to(self.span()),
            ))))
        } else {
            Some(ty)
        }
    }

    fn parse(&mut self) -> AST {
        let mut items = vec![];

        self.skip_nls();
        while !self.eof() {
            verbose(format!("Parse {:?}", self.peek()));
            items.push(self.parse_item());
        }

        AST::new(items)
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(mut self) -> StageOutput<AST> {
        let ast = self.parse();
        StageOutput::new(self.sess, ast, self.msg)
    }
}

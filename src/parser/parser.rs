use std::fmt::Display;

use crate::{
    ast::{
        expr::{Block, Expr, ExprKind, InfixOpKind, Lit, PrefixOpKind},
        item::{Item, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        ErrorNode, NodeId, NodeKindStr, Path, AST, N, PR,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Kw, Span, WithSpan},
};

use super::token::{Infix, Punct, Token, TokenCmp, TokenKind, TokenStream};

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
    ($self: ident, $parse: ident) => {{
        let mut entities = vec![];

        $self.expect(TokenCmp::Nl)?;
        $self.expect(TokenCmp::Indent)?;

        let mut first = true;
        while !$self.is(TokenCmp::Dedent) {
            if $self.eof() {
                break;
            }

            if first {
                first = false;
            } else {
                $self.expect_semis()?;
            }

            if $self.eof() {
                break;
            }

            entities.push($self.$parse());
        }

        if !$self.eof() {
            $self.just_skip(TokenCmp::Dedent);
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

    fn prev_tok(&self) -> Token {
        self.peek_tok_at(self.pos - 1)
    }

    fn prev_tok_span(&self) -> Span {
        self.prev_tok().span()
    }

    fn close_span(&self, span: Span) -> Span {
        span.to(self.prev_tok_span())
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

    fn backtrack(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn is(&self, cmp: TokenCmp) -> bool {
        cmp == self.peek()
    }

    fn next_is(&self, cmp: TokenCmp) -> bool {
        self.tokens
            .0
            .get(self.pos + 1)
            .map_or(false, |t| cmp == t.kind)
    }

    fn lookup_after_many1(&mut self, after: TokenCmp, cmp: TokenCmp) -> bool {
        let start = self.pos;
        while self.is(after) {
            self.advance();
        }

        let mut got = false;
        if self.pos != start && self.is(cmp) {
            got = true;
        }

        self.backtrack(start);

        got
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

    fn expected_error<E, G>(&mut self, expected: E, got: G) -> PR<()>
    where
        E: Display,
        G: Display,
    {
        MessageBuilder::error()
            .span(self.span())
            .text(format!("Expected {}, got {}", expected, got))
            .emit_single_label(self);
        Err(ErrorNode::new(self.span()))
    }

    fn unexpected_token(&mut self) -> ErrorNode {
        MessageBuilder::error()
            .span(self.span())
            .text(format!("Unexpected token {}", self.peek()))
            .label(self.span(), format!("Unexpected {}", self.peek()))
            .emit(self);
        ErrorNode::new(self.advance_tok().span)
    }

    fn expect(&mut self, cmp: TokenCmp) -> PR<()> {
        if self.is(cmp) {
            self.advance();
            Ok(())
        } else {
            self.expected_error(cmp, self.peek())?;
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expect_semis(&mut self) -> PR<()> {
        // If we encountered EOF don't skip it
        // Note: Keep order
        if self.eof() || self.skip_opt_nls() {
            Ok(())
        } else {
            self.expected_error("semi", self.peek())
        }
    }

    fn expect_kw(&mut self, kw: Kw) -> PR<()> {
        self.expect(TokenCmp::Kw(kw))
    }

    fn expect_punct(&mut self, punct: Punct) -> PR<()> {
        self.expect(TokenCmp::Punct(punct))
    }

    fn expected<'a, T>(&'a mut self, entity: Option<T>, expected: &str) -> PR<T>
    where
        T: WithSpan,
    {
        if let Some(entity) = entity {
            Ok(entity)
        } else {
            self.expected_error(expected, self.peek())?;
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
                        .span(stmt.span())
                        .text(format!("Expected {}, got {}", expected, stmt.kind_str()))
                        .label(stmt.span(), format!("Unexpected {}", stmt.kind_str()))
                        .emit(self);
                    Err(ErrorNode::new(stmt.span()))
                }
                Err(err) => Err(err),
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

    fn skip(&mut self, cmp: TokenCmp) -> Option<Token> {
        self.skip_if(|kind| cmp == kind)
    }

    fn skip_any(&mut self, cmp: &[TokenCmp]) -> Option<Token> {
        self.skip_if(|kind| cmp.iter().any(|cmp| *cmp == kind))
    }

    fn skip_punct(&mut self, punct: Punct) -> Option<Token> {
        self.skip_if(|kind| TokenCmp::Punct(punct) == kind)
    }

    fn just_skip(&mut self, cmp: TokenCmp) {
        self.expect(cmp)
            .expect(format!("[BUG] Failed to just skip expected token {}", cmp).as_str())
    }

    fn skip_opt_nls(&mut self) -> bool {
        self.skip_many(TokenCmp::Nl)
    }

    fn skip_many(&mut self, cmp: TokenCmp) -> bool {
        let mut got = false;
        while let Some(_) = self.skip(cmp) {
            got = true;
        }
        got
    }

    fn parse_many(&mut self, cmp: TokenCmp) -> PR<Vec<PR<Token>>> {
        let mut tokens = vec![];
        while self.is(cmp) {
            tokens.push(Ok(self.advance_tok()));
        }
        Ok(tokens)
    }

    fn parse_many1(&mut self, cmp: TokenCmp) -> PR<Vec<PR<Token>>> {
        let tokens = self.parse_many(cmp)?;
        if tokens.is_empty() {
            Err(ErrorNode::new(self.span()))
        } else {
            Ok(tokens)
        }
    }

    fn parse_ident(&mut self, expected: &str) -> PR<Ident> {
        let skip = self.skip(TokenCmp::Ident).map(|tok| Ident::from_token(tok));
        self.expected(skip, expected)
    }

    // Statements //
    fn parse_stmt(&mut self) -> PR<N<Stmt>> {
        verbose!("Parse stmt {}", self.peek());

        if let Some(item) = self.parse_opt_item() {
            let span = item.span();
            Ok(Box::new(Stmt::new(
                self.next_node_id(),
                StmtKind::Item(item),
                span,
            )))
        } else if let Some(expr) = self.parse_opt_expr() {
            self.expect_semis()?;

            let span = expr.span();
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
        self.skip_opt_nls();

        let item = self.parse_opt_item();
        self.expect_semis()?;

        self.try_recover_any(item, "item")
    }

    fn parse_opt_item(&mut self) -> Option<PR<N<Item>>> {
        if self.is(TokenCmp::Kw(Kw::Mod)) {
            Some(self.parse_mod_item())
        } else if self.is(TokenCmp::Kw(Kw::Type)) {
            Some(self.parse_type_item())
        } else if self.lookup_after_many1(TokenCmp::Ident, TokenCmp::Punct(Punct::Assign)) {
            Some(self.parse_decl_item())
        } else {
            None
        }
    }

    fn parse_mod_item(&mut self) -> PR<N<Item>> {
        verbose!("Parse mod {}", self.peek());

        let lo = self.span();

        self.expect_kw(Kw::Mod)?;

        let name = self.parse_ident("module name");

        let items = parse_block_common!(self, parse_item);

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Mod(name, items),
            self.close_span(lo),
        )))
    }

    fn parse_type_item(&mut self) -> PR<N<Item>> {
        verbose!("Parse type alias {}", self.peek());

        let lo = self.span();

        self.expect_kw(Kw::Type)?;

        let name = self.parse_ident("type name");

        self.expect_punct(Punct::Assign)?;

        let ty = self.parse_ty();

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Type(name, ty),
            self.close_span(lo),
        )))
    }

    fn parse_decl_item(&mut self) -> PR<N<Item>> {
        verbose!("Parse decl {} at {}", self.peek(), self.span());

        let lo = self.span();

        // TODO: Rewrite this scary hell
        let mut idents = self
            .parse_many1(TokenCmp::Ident)?
            .iter()
            .map(|t| {
                t.clone().map(|t| match t.kind {
                    TokenKind::Ident(_) => Ident::from_token(t),
                    _ => unreachable!(),
                })
            })
            .collect::<Vec<_>>();

        let name = idents.remove(0);
        let params = idents;

        self.skip_punct(Punct::Assign);

        let body = self.parse_body();

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Decl(name, params, body),
            self.close_span(lo),
        )))
    }

    // Expressions //
    fn parse_expr(&mut self) -> PR<N<Expr>> {
        verbose!("Parse expr {}", self.peek());

        let expr = self.parse_opt_expr();
        self.try_recover_any(expr, "expression")
    }

    fn parse_opt_expr(&mut self) -> Option<PR<N<Expr>>> {
        if self.is(TokenCmp::Kw(Kw::Let)) {
            return Some(self.parse_let());
        }

        self.parse_prec(0)
    }

    fn parse_let(&mut self) -> PR<N<Expr>> {
        verbose!("Parse let {}", self.peek());

        let lo = self.span();

        self.expect_kw(Kw::Let)?;

        let block = self.parse_block();

        // There might be a better way to slice vector
        Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Let(block),
            self.close_span(lo),
        )))
    }

    fn parse_block_expr(&mut self) -> PR<N<Expr>> {
        verbose!("Parse block expr {}", self.peek());

        let lo = self.span();
        let block = self.parse_block();

        return Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Block(block),
            self.close_span(lo),
        )));
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
            verbose!("Parse prec {}", prec);

            if op.kind == TokenKind::Punct(Punct::Colon) {
                // Parse ascription (type expression)
                let ty = self.parse_ty();
                lhs = Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::Ty(lhs, ty),
                    self.close_span(lo),
                )));

                // TODO: Allow ascription of ascription?
                break;
            } else {
                let rhs = self.parse_prec(prec + 1);

                if let Some(rhs) = rhs {
                    lhs = Ok(Box::new(Expr::new(
                        self.next_node_id(),
                        ExprKind::Infix(lhs, InfixOpKind::from_tok(op), rhs),
                        self.close_span(lo),
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
            verbose!("Parse prefix {}", self.peek());

            let rhs = self.parse_postfix();

            let rhs = if let Some(rhs) = rhs {
                rhs
            } else {
                verbose!("[parse_prefix] expected expression error {}", self.peek());
                MessageBuilder::error()
                    .span(self.span())
                    .text(format!("Expected expression after {} operator", op.kind))
                    .emit_single_label(self);
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
            let arg = self.parse_postfix();

            if let Some(arg) = arg {
                Some(Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::App(lhs, arg),
                    self.close_span(lo),
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
        verbose!("Parse primary {}", self.peek());

        let Token { kind, span } = self.peek_tok();

        if self.is(TokenCmp::Punct(Punct::Backslash)) {
            return self.parse_abs();
        }

        let (kind, advance) = match kind {
            TokenKind::Bool(val) => (Some(Ok(ExprKind::Lit(Lit::Bool(val)))), true),
            TokenKind::Int(val, kind) => (Some(Ok(ExprKind::Lit(Lit::Int(val, kind)))), true),
            TokenKind::String(sym) => (Some(Ok(ExprKind::Lit(Lit::String(sym)))), true),
            TokenKind::Ident(_) => (
                Some(Ok(ExprKind::Path(
                    self.parse_path("[BUG] First identifier in path expression"),
                ))),
                false,
            ),

            // Error token is an error on lexing stage
            //  so don't emit one more error for it, just add error stub
            TokenKind::Error(_) => (Some(Err(ErrorNode::new(span))), false),

            _ => (None, false),
        };

        if advance {
            self.advance();
        }

        kind.map(|k| k.map(|k| Box::new(Expr::new(self.next_node_id(), k, span))))
    }

    fn parse_path(&mut self, expected: &str) -> PR<Path> {
        verbose!("Parse path {}", self.peek());

        // If no first identifier present then it's "expected path" error, not "expected identifier"
        let mut segments = vec![self.parse_ident(expected)?];
        while self.skip(TokenCmp::Punct(Punct::Dot)).is_some() {
            segments.push(self.parse_ident("path segment (identifier)")?);
        }

        Ok(Path::new(self.next_node_id(), segments))
    }

    fn parse_block(&mut self) -> PR<Block> {
        verbose!("Parse block {}", self.peek());

        let lo = self.span();

        let stmts = parse_block_common!(self, parse_stmt);

        Ok(Block::new(self.next_node_id(), stmts, self.close_span(lo)))
    }

    fn parse_body(&mut self) -> PR<N<Expr>> {
        verbose!("Parse body {}", self.peek());

        if self.is(TokenCmp::Nl) && self.next_is(TokenCmp::Indent) {
            return self.parse_block_expr();
        }

        return self.parse_expr();
    }

    fn parse_abs(&mut self) -> Option<PR<N<Expr>>> {
        verbose!("Parse abs {}", self.peek());

        let lo = self.span();

        self.skip(TokenCmp::Punct(Punct::Backslash));

        let param = self.parse_ident("parameter name");

        self.skip(TokenCmp::Punct(Punct::Arrow));

        let body = self.parse_expr();

        Some(Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Abs(param, body),
            self.close_span(lo),
        ))))
    }

    // Types //
    fn parse_ty(&mut self) -> PR<N<Ty>> {
        let ty = self.parse_opt_ty();
        self.try_recover_any(ty, "type")
    }

    fn parse_opt_ty(&mut self) -> Option<PR<N<Ty>>> {
        verbose!("Parse opt ty {}", self.peek());

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

            TokenKind::Ident(_) => TyKind::Path(self.parse_path("type path")),

            _ => {
                return None;
            }
        };

        let ty = Ok(Box::new(Ty::new(
            self.next_node_id(),
            kind,
            self.close_span(lo),
        )));

        if self.skip_punct(Punct::Arrow).is_some() {
            let return_ty = self.parse_ty();
            Some(Ok(Box::new(Ty::new(
                self.next_node_id(),
                TyKind::Func(ty, return_ty),
                self.close_span(lo),
            ))))
        } else {
            Some(ty)
        }
    }

    fn parse(&mut self) -> AST {
        let mut items = vec![];

        while !self.eof() {
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

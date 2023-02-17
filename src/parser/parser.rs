use std::fmt::{Debug, Display};

use crate::{
    ast::{
        expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, TyExpr},
        is_block_ended,
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind, TyPath},
        ErrorNode, IsBlockEnded, NodeId, NodeKindStr, Path, PathSeg, AST, N, PR,
    },
    cli::color::{Color, Colorize},
    interface::writer::{out, outln},
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Kw, Span, WithSpan},
};

use super::token::{Op, Punct, Token, TokenCmp, TokenKind, TokenStream};

#[derive(Debug, PartialEq)]
enum ParseEntryKind {
    Opt,
    Expect,
    ExpectWrapper,
    Wrapper,
    ExpectToken,
    RecoverWrapper,
    LateCheck,
}

impl Display for ParseEntryKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseEntryKind::Opt => write!(f, "optional"),
            ParseEntryKind::Expect => write!(f, "expected"),
            ParseEntryKind::ExpectWrapper => write!(f, "expected wrapper"),
            ParseEntryKind::Wrapper => write!(f, "wrapper"),
            ParseEntryKind::ExpectToken => write!(f, "expected token"),
            ParseEntryKind::RecoverWrapper => write!(f, "recovering wrapper"),
            ParseEntryKind::LateCheck => write!(f, "late check"),
        }
    }
}

impl ParseEntryKind {
    fn is_recovering(&self) -> bool {
        match self {
            ParseEntryKind::Opt
            | ParseEntryKind::Expect
            | ParseEntryKind::ExpectWrapper
            | ParseEntryKind::ExpectToken
            | ParseEntryKind::Wrapper
            | ParseEntryKind::LateCheck => false,
            ParseEntryKind::RecoverWrapper => true,
        }
    }
}

#[derive(Debug)]
struct ParseEntry {
    parent: Option<usize>,
    id: usize,
    kind: ParseEntryKind,
    children: Vec<usize>,
    name: String,
    start: Token,
    end: Option<Token>,
    failed: bool,
}

#[derive(Clone, Copy)]
struct ParseEntryPrinter<'a> {
    parent_prefix: &'a str,
    is_last: bool,
    continuous: bool,
    recovering: bool,
}

pub struct Parser {
    sess: Session,
    pos: usize,
    tokens: TokenStream,
    msg: MessageStorage,
    parse_entries: Vec<ParseEntry>,
    parse_entry: Option<usize>,
}

impl<'ast> MessageHolder for Parser {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

macro_rules! parse_block_common {
    ($self: ident, $parse: ident) => {{
        let mut entities = vec![];

        $self.just_skip(TokenCmp::BlockStart);

        while !$self.eof() && !$self.is(TokenCmp::BlockEnd) {
            entities.push($self.$parse());
        }

        if !$self.eof() {
            $self.just_skip(TokenCmp::BlockEnd);
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
            parse_entries: Vec::new(),
            parse_entry: None,
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

    fn close_span(&self, lo: Span) -> Span {
        lo.to(self.prev_tok_span())
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

    fn next_is(&self, cmp: TokenCmp) -> bool {
        self.tokens
            .0
            .get(self.pos + 1)
            .map_or(false, |t| cmp == t.kind)
    }

    fn lookup_after_many1(&mut self, after: TokenCmp, cmp: TokenCmp) -> bool {
        let mut cur_pos = self.pos;
        let start = self.pos;

        while let Some(tok) = self.tokens.0.get(cur_pos) {
            if tok.kind != after {
                break;
            }
            cur_pos += 1;
        }

        if cur_pos > start {
            if let Some(tok) = self.tokens.0.get(cur_pos) {
                return tok.kind == cmp;
            }
        }

        false
    }

    fn advance_offset_tok(&mut self, offset: usize) -> Token {
        if self.eof() {
            panic!("Advanced to EOF");
        }
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
            .origin(file!(), line!())
            .emit_single_label(self);
        Err(ErrorNode::new(self.span()))
    }

    fn unexpected_token(&mut self) -> ErrorNode {
        MessageBuilder::error()
            .span(self.span())
            .text(format!("Unexpected token {}", self.peek()))
            .label(self.span(), format!("Unexpected {}", self.peek()))
            .origin(file!(), line!())
            .emit(self);
        let tok = self.advance_tok();
        ErrorNode::new_parsed(tok)
    }

    fn expect(&mut self, cmp: TokenCmp) -> PR<()> {
        self.mark_expect_token(cmp);
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
        if self.eof() || self.is(TokenCmp::BlockEnd) || self.skip_opt_nls() {
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

    fn expect_op(&mut self, op: Op) -> PR<()> {
        self.expect(TokenCmp::Op(op))
    }

    fn expected<'a, T>(&'a mut self, entity: Option<T>, expected: &str) -> PR<T> {
        if let Some(entity) = entity {
            Ok(entity)
        } else {
            self.expected_error(expected, self.peek())?;
            Err(ErrorNode::new(self.span()))
        }
    }

    fn expected_pr<'a, T>(&'a mut self, pr: Option<PR<T>>, expected: &str) -> PR<T> {
        if let Some(pr) = pr {
            pr
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
            let pe = self.enter_entity(ParseEntryKind::RecoverWrapper, "recover any");
            let stmt = self.parse_stmt();
            match stmt {
                Ok(stmt) => {
                    MessageBuilder::error()
                        .span(stmt.span())
                        .text(format!("Expected {}, got {}", expected, stmt.kind_str()))
                        .label(stmt.span(), format!("Unexpected {}", stmt.kind_str()))
                        .origin(file!(), line!())
                        .emit(self);
                    self.exit_parsed_entity(pe);
                    Err(ErrorNode::new_parsed(stmt))
                },
                Err(err) => {
                    self._exit_entity(pe, true);
                    Err(err)
                },
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

    fn parse_ident_decl_name(&mut self, expected: &str) -> PR<Ident> {
        let skip = self
            .skip_any(&[TokenCmp::DeclName])
            .map(|tok| Ident::from_token(tok));
        self.expected(skip, expected)
    }

    // Statements //
    fn parse_stmt(&mut self) -> PR<N<Stmt>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "statement");

        let stmt = self._parse_stmt();

        self.exit_entity(pe, &stmt);

        stmt
    }

    fn _parse_stmt(&mut self) -> PR<N<Stmt>> {
        if let Some(item) = self.parse_opt_item() {
            let span = item.span();

            if !is_block_ended!(item) {
                self.expect_semis()?;
            }

            Ok(Box::new(Stmt::new(
                self.next_node_id(),
                StmtKind::Item(item),
                span,
            )))
        } else if let Some(expr) = self.parse_opt_expr() {
            if !is_block_ended!(expr) {
                self.expect_semis()?;
            }

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
        let pe = self.enter_entity(ParseEntryKind::Expect, "item");

        let item = self.parse_opt_item();

        self.exit_expected_entity(pe, &item);

        self.try_recover_any(item, "item")
    }

    fn parse_item_semi(&mut self) -> PR<N<Item>> {
        let pe = self.enter_entity(ParseEntryKind::Wrapper, "item with semi");

        let item = self.parse_item();
        self.expect_semis()?;

        self.exit_entity(pe, &item);

        item
    }

    fn parse_opt_item(&mut self) -> Option<PR<N<Item>>> {
        let pe = self.enter_entity(ParseEntryKind::Opt, "item");

        if self.is(TokenCmp::Kw(Kw::Mod)) {
            let mod_item = self.parse_mod_item();

            self.exit_entity(pe, &mod_item);

            Some(mod_item)
        } else if self.is(TokenCmp::Kw(Kw::Type)) {
            let ty_item = self.parse_ty_item();

            self.exit_entity(pe, &ty_item);

            Some(ty_item)
        } else if self.lookup_after_many1(TokenCmp::DeclName, TokenCmp::Op(Op::Assign)) {
            let decl = self.parse_decl_item();

            self.exit_entity(pe, &decl);

            Some(decl)
        } else {
            self.exit_parsed_entity(pe);

            None
        }
    }

    fn parse_mod_item(&mut self) -> PR<N<Item>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "module");

        let lo = self.span();

        self.expect_kw(Kw::Mod)?;

        let name = self.parse_ident("module name");

        let items = parse_block_common!(self, parse_item_semi);

        self.exit_parsed_entity(pe);

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Mod(name, items),
            self.close_span(lo),
        )))
    }

    fn parse_ty_item(&mut self) -> PR<N<Item>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "type alias");

        let lo = self.span();

        self.expect_kw(Kw::Type)?;

        // Note: `parse_ident_in_path` is used to allow `type () = ()`
        let name = self.parse_ident_decl_name("type name");

        self.expect_op(Op::Assign)?;

        let ty = self.parse_ty();

        self.exit_parsed_entity(pe);

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Type(name, ty),
            self.close_span(lo),
        )))
    }

    fn parse_decl_item(&mut self) -> PR<N<Item>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "declaration");

        let lo = self.span();

        // `parse_ident_in_path` is used to allow `() = ()`
        let name = self.parse_ident_decl_name("function name");

        let mut params = vec![];
        while !self.eof() {
            if self.is(TokenCmp::Op(Op::Assign)) {
                break;
            }
            params.push(self.parse_pat("function parameter (pattern)"));
        }

        self.expect(TokenCmp::Op(Op::Assign))?;

        let body = self.parse_body();

        self.exit_parsed_entity(pe);

        Ok(Box::new(Item::new(
            self.next_node_id(),
            ItemKind::Decl(name, params, body),
            self.close_span(lo),
        )))
    }

    // Patterns //
    fn parse_pat(&mut self, expected: &str) -> PR<Pat> {
        let lo = self.span();

        let kind = self.parse_pat_kind(expected)?;

        Ok(Pat::new(self.next_node_id(), kind, self.close_span(lo)))
    }

    fn parse_pat_kind(&mut self, expected: &str) -> PR<PatKind> {
        if self.is(TokenCmp::Ident) {
            Ok(PatKind::Ident(self.parse_ident("[bug] ident pattern")))
        } else if self.skip(TokenCmp::Punct(Punct::LParen)).is_some() {
            self.skip_opt_nls();
            // TODO: Tuple pattern
            self.expect(TokenCmp::Punct(Punct::RParen))?;
            Ok(PatKind::Unit)
        } else if self.skip(TokenCmp::Kw(Kw::Unit)).is_some() {
            Ok(PatKind::Unit)
        } else {
            self.expected(None, expected)
        }
    }

    // Expressions //
    fn parse_expr(&mut self) -> PR<N<Expr>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "expression");

        let expr = self.parse_opt_expr();

        self.exit_expected_entity(pe, &expr);

        self.try_recover_any(expr, "expression")
    }

    fn parse_opt_expr(&mut self) -> Option<PR<N<Expr>>> {
        let pe = self.enter_entity(ParseEntryKind::Opt, "expression");

        let expr = if self.is(TokenCmp::Kw(Kw::Let)) {
            Some(self.parse_let())
        } else {
            self.parse_infix()
        };

        self.exit_parsed_entity(pe);

        expr
    }

    fn parse_let(&mut self) -> PR<N<Expr>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "let expression");

        let lo = self.span();

        self.expect_kw(Kw::Let)?;

        let block = self.parse_block();

        self.exit_parsed_entity(pe);

        // There might be a better way to slice vector
        Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Let(block),
            self.close_span(lo),
        )))
    }

    fn parse_block_expr(&mut self) -> PR<N<Expr>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "block expression");

        let lo = self.span();
        let block = self.parse_block();

        self.exit_parsed_entity(pe);

        return Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Block(block),
            self.close_span(lo),
        )));
    }

    fn parse_infix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let lhs = self.parse_prefix();

        if let Some(lhs_expr) = lhs {
            if let Some(op) = self.skip(TokenCmp::InfixOp) {
                let rhs = self.parse_infix();
                let rhs = self.expected_pr(rhs, "right-hand side in infix expression");

                return Some(Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::Infix(Infix {
                        lhs: lhs_expr,
                        op: PathExpr(Ok(Path::new_infix_op(self.next_node_id(), op))),
                        rhs,
                    }),
                    self.close_span(lo),
                ))));
            } else {
                return Some(lhs_expr);
            }
        } else {
            return self.parse_prefix();
        }
    }

    fn parse_prefix(&mut self) -> Option<PR<N<Expr>>> {
        let _lo = self.span();

        if self.is(TokenCmp::Op(Op::Minus))
            && (self.next_is(TokenCmp::Int) || self.next_is(TokenCmp::Float))
        {
            todo!("Negative literals")
        }

        // if let Some(op) = self.skip(TokenCmp::Kw(Kw::Minus)) {
        //     let rhs = self.parse_postfix();

        //     let rhs = if let Some(rhs) = rhs {
        //         rhs
        //     } else {
        //         MessageBuilder::error()
        //             .span(self.span())
        //             .text(format!("Expected expression after {} operator", op.kind))
        //             .origin(file!(), line!())
        //             .emit_single_label(self);
        //         Err(ErrorNode::new(self.span()))
        //     };

        //     self.mark_late_check("prefix expression");

        //     Some(Ok(Box::new(Expr::new(
        //         self.next_node_id(),
        //         ExprKind::Prefix(Prefix {
        //             op: PrefixOpKind::from_tok(&op),
        //             rhs,
        //         }),
        //         op.span.to(lo),
        //     ))))
        // } else {
        //     self.parse_postfix()
        // }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Option<PR<N<Expr>>> {
        let lo = self.span();

        let lhs = self.parse_primary();

        if let Some(lhs) = lhs {
            // FIXME: Ok precedence?
            if self.skip(TokenCmp::Punct(Punct::Colon)).is_some() {
                let ty = self.parse_ty();

                return Some(Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::Ty(TyExpr { expr: lhs, ty }),
                    self.close_span(lo),
                ))));
            }

            let arg = self.parse_postfix();

            if let Some(arg) = arg {
                self.mark_late_check("function call");

                Some(Ok(Box::new(Expr::new(
                    self.next_node_id(),
                    ExprKind::Call(Call { lhs, arg }),
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
        let lo = self.span();
        let Token { kind, span } = self.peek_tok();

        if self.is(TokenCmp::Punct(Punct::Backslash)) {
            return self.parse_lambda();
        }

        // if self.is(TokenCmp::Punct(Punct::LParen)) {
        //     let lo = self.span();

        //     self.advance();

        //     if self.is(TokenCmp::SomeOp) {
        //         let op_ident = Ident::from_token(self.advance_tok());
        //         return Some(Ok(Box::new(Expr::new(
        //             self.next_node_id(),
        //             ExprKind::Path(PathExpr(Ok())),
        //             self.close_span(lo),
        //         ))));
        //     }
        // }

        let (kind, advance) = if let Some(_) = self.skip(TokenCmp::Punct(Punct::LParen)) {
            let expr = self.parse_expr();

            (Some(Ok(ExprKind::Paren(expr))), false)
        } else {
            match kind {
                TokenKind::Bool(val) => (Some(Ok(ExprKind::Lit(Lit::Bool(val)))), true),
                TokenKind::Int(val, kind) => (Some(Ok(ExprKind::Lit(Lit::Int(val, kind)))), true),
                TokenKind::String(sym) => (Some(Ok(ExprKind::Lit(Lit::String(sym)))), true),

                // FIXME: Add TokenCmp::PathFirst
                tok if tok == TokenCmp::DeclName => (
                    Some(Ok(ExprKind::Path(PathExpr(
                        self.parse_path("[BUG] First identifier in path expression"),
                    )))),
                    false,
                ),

                // Error token is an error on lexing stage
                //  so don't emit one more error for it, just add error stub
                TokenKind::Error(_) => (Some(Err(ErrorNode::new(span))), true),

                _ => (None, false),
            }
        };

        if advance {
            self.advance();
        }

        if let Some(kind) = &kind {
            self.mark_late_check(&format!(
                "primary expression `{}`",
                kind.as_ref()
                    .map_or("[ERROR]".to_string(), |expr| expr.to_string())
            ));
        }

        kind.map(|k| k.map(|k| Box::new(Expr::new(self.next_node_id(), k, self.close_span(lo)))))
    }

    fn parse_path(&mut self, _expected: &str) -> PR<Path> {
        let lo = self.span();

        let pe = self.enter_entity(ParseEntryKind::Expect, "path");

        // If no first identifier present then it's "expected path" error, not "expected identifier"
        let mut segments = vec![self.parse_path_seg()];
        while self.skip(TokenCmp::Punct(Punct::Dot)).is_some() {
            segments.push(self.parse_path_seg());
        }

        self.exit_parsed_entity(pe);

        Ok(Path::new(
            self.next_node_id(),
            segments,
            self.close_span(lo),
        ))
    }

    fn parse_path_seg(&mut self) -> PathSeg {
        let lo = self.span();
        // FIXME: PascalCase type ident, not any ident??
        PathSeg::new(
            self.parse_ident_decl_name("path segment"),
            self.close_span(lo),
        )
    }

    fn parse_block(&mut self) -> PR<Block> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "block");

        let lo = self.span();

        let stmts = parse_block_common!(self, parse_stmt);

        self.exit_parsed_entity(pe);

        Ok(Block::new(self.next_node_id(), stmts, self.close_span(lo)))
    }

    fn parse_body(&mut self) -> PR<N<Expr>> {
        let pe = self.enter_entity(ParseEntryKind::Wrapper, "body");

        let body = if self.is(TokenCmp::BlockStart) {
            let body = self.parse_block_expr();
            body
        } else {
            let body = self.parse_expr();
            body
        };

        self.exit_parsed_entity(pe);

        body
    }

    fn parse_lambda(&mut self) -> Option<PR<N<Expr>>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "lambda");

        let lo = self.span();

        self.skip(TokenCmp::Punct(Punct::Backslash));

        let param = self.parse_pat("lambda parameter (pattern)");

        self.skip(TokenCmp::Punct(Punct::Arrow));

        let body = self.parse_body();

        self.exit_parsed_entity(pe);

        Some(Ok(Box::new(Expr::new(
            self.next_node_id(),
            ExprKind::Lambda(Lambda { param, body }),
            self.close_span(lo),
        ))))
    }

    // Types //
    fn parse_ty(&mut self) -> PR<N<Ty>> {
        let pe = self.enter_entity(ParseEntryKind::Expect, "type");

        let ty = self.parse_opt_ty();
        let ty = self.try_recover_any(ty, "type");

        self.exit_parsed_entity(pe);

        ty
    }

    fn parse_opt_ty(&mut self) -> Option<PR<N<Ty>>> {
        let pe = self.enter_entity(ParseEntryKind::Opt, "type");

        let lo = self.span();

        let kind = match self.peek() {
            TokenKind::Punct(Punct::LParen) => {
                self.advance();
                let inner = self.parse_opt_ty();
                self.skip(TokenCmp::Punct(Punct::RParen));

                match inner {
                    Some(inner) => TyKind::Paren(inner),
                    None => {
                        return None;

                        // Note: Unit type as path is handled in lexer with Kw::Unit,
                        //  so here we just failed to parse a type
                        // let span = self.close_span(lo);
                        // TyKind::Path(TyPath(Ok(Path::new(
                        //     self.next_node_id(),
                        //     vec![PathSeg::new(Ok(Ident::new(span, "()".intern())), span)],
                        //     span,
                        // ))))
                    },
                }
            },

            // `Kw::Unit` to allow `()` in type path
            TokenKind::Kw(Kw::Unit) | TokenKind::Ident(_) => {
                TyKind::Path(TyPath(self.parse_path("type path")))
            },

            _ => {
                return None;
            },
        };

        let ty = Ok(Box::new(Ty::new(
            self.next_node_id(),
            kind,
            self.close_span(lo),
        )));

        if self.skip_punct(Punct::Arrow).is_some() {
            let return_ty = self.parse_ty();

            self.exit_parsed_entity(pe);

            Some(Ok(Box::new(Ty::new(
                self.next_node_id(),
                TyKind::Func(ty, return_ty),
                self.close_span(lo),
            ))))
        } else {
            self.exit_parsed_entity(pe);

            Some(ty)
        }
    }

    fn parse<'ast>(&mut self) -> AST {
        let mut items = vec![];

        let pe = self.enter_entity(ParseEntryKind::Expect, "top-level item list");

        self.skip_opt_nls();

        while !self.eof() {
            items.push(self.parse_item_semi());
        }
        self.exit_parsed_entity(pe);

        self.print_parse_entries();

        AST::new(items)
    }

    // Debug //
    fn enter_entity(&mut self, kind: ParseEntryKind, name: &str) -> Option<usize> {
        if !self.sess.config().parser_debug() {
            return None;
        }

        // verbose!("Enter {} {}", kind, name);

        let id = self.parse_entries.len();

        self.parse_entries.push(ParseEntry {
            parent: self.parse_entry,
            id,
            kind,
            name: name.to_string(),
            start: self.peek_tok(),
            end: None,
            children: vec![],
            failed: false,
        });

        // Add entry to parent
        if let Some(entry) = self.parse_entry {
            self.parse_entries.get_mut(entry).unwrap().children.push(id);
        }

        self.parse_entry = Some(id);

        Some(id)
    }

    fn mark_expect_token(&mut self, cmp: TokenCmp) {
        let id = self.enter_entity(ParseEntryKind::ExpectToken, &cmp.to_string());

        let failed = !self.is(cmp);

        self._exit_entity(id, failed);
    }

    fn mark_late_check(&mut self, name: &str) {
        let id = self.enter_entity(ParseEntryKind::LateCheck, name);

        self.exit_parsed_entity(id);
    }

    fn exit_expected_entity<T, E>(&mut self, id: Option<usize>, entity: &Option<Result<T, E>>) {
        self._exit_entity(
            id,
            match entity {
                Some(pr) => pr.is_err(),
                None => true,
            },
        )
    }

    fn exit_parsed_entity(&mut self, id: Option<usize>) {
        self._exit_entity(id, false)
    }

    fn exit_entity<T, E>(&mut self, id: Option<usize>, pr: &Result<T, E>) {
        self._exit_entity(id, pr.is_err());
    }

    fn _exit_entity(&mut self, id: Option<usize>, failed: bool) {
        if !self.sess.config().parser_debug() {
            return;
        }

        let end = Some(self.peek_tok());
        let entry = self.parse_entries.get_mut(id.unwrap()).unwrap();

        entry.end = end;

        if failed {
            entry.failed = true;
        }

        let mut cur_id = id;

        while let Some(id) = cur_id {
            let pe = self.parse_entries.get(id).unwrap();

            // verbose!("Exit {} {}", pe.kind, pe.name);

            cur_id = pe.parent;

            if pe.id == id {
                break;
            }

            let entry = self.parse_entries.get_mut(id).unwrap();
            entry.failed = true;
            entry.end = end;
        }

        self.parse_entry = cur_id;
    }

    fn _parse_entry(&self) -> Option<&ParseEntry> {
        self.parse_entries.get(self.parse_entry.unwrap())
    }

    fn print_parse_entries(&mut self) {
        if !self.sess.config().parser_debug() {
            return;
        }

        self.print_parse_entry(
            self.parse_entries.first().unwrap().id,
            ParseEntryPrinter {
                parent_prefix: "",
                is_last: true,
                continuous: false,
                recovering: false,
            },
        );
    }

    fn print_parse_entry(&mut self, index: usize, printer: ParseEntryPrinter) {
        let entry = self.parse_entries.get(index).unwrap();
        let failed = entry.failed
            || entry.children.len() == 1
                && self.parse_entries[entry.children[0]].kind.is_recovering();
        let recovering = printer.recovering || entry.kind.is_recovering();

        const PREFIXES: [&[&str]; 3] = [
            // Prefix
            &["├──", "│  "],
            // Last
            &["╰──", "   "],
            // Postfix
            &["──", "┬─"],
        ];

        const RECOVER_PREFIXES: [&[&str]; 3] = [
            // Prefix
            &["╠══", "║  "],
            // Last
            &["╚══", "   "],
            // Postfix
            &["══", "╦═"],
        ];

        const FIRST_RECOVER_PREFIXES: [&[&str]; 3] = [
            // Prefix
            &["╞══", "│  "],
            // Last
            &["╘══", "   "],
            // Postfix
            &["══", "╦═"],
        ];

        const FAILED_PREFIXES: [&[&str]; 3] = [
            // Prefix
            &["├×─", "│  "],
            // Last
            &["╰×─", "   "],
            // Postfix
            &["──", "┬─"],
        ];

        let prefixes = match (entry.kind.is_recovering(), printer.recovering, failed) {
            (_, _, true) => FAILED_PREFIXES,
            (true, true, _) => RECOVER_PREFIXES,
            (false, true, _) => RECOVER_PREFIXES,
            (false, false, _) => PREFIXES,
            (true, false, _) => FIRST_RECOVER_PREFIXES,
        };

        let main_prefixes = prefixes[if printer.is_last { 1 } else { 0 }];

        let prefix_color = if failed {
            Color::Red
        } else if recovering {
            Color::Yellow
        } else {
            Color::Green
        };

        let prefix = if printer.continuous {
            String::new().fg_color(Color::Green)
        } else {
            format!(
                "{}{}{} ",
                printer.parent_prefix,
                main_prefixes[0],
                if entry.children.is_empty() {
                    prefixes[2][0]
                } else {
                    prefixes[2][1]
                }
            )
            .fg_color(prefix_color)
        };

        let wrong_tree = match entry.kind {
            ParseEntryKind::ExpectWrapper
            | ParseEntryKind::Wrapper
            | ParseEntryKind::RecoverWrapper => entry.children.len() > 1,
            ParseEntryKind::Opt | ParseEntryKind::Expect => false,
            ParseEntryKind::ExpectToken | ParseEntryKind::LateCheck => !entry.children.is_empty(),
        };

        let wrong_tree_suffix = if wrong_tree { "[WRONG_TREE] " } else { "" }.bright_red();

        let parent_prefix = format!(
            "{}{}",
            printer.parent_prefix,
            if printer.continuous {
                ""
            } else {
                main_prefixes[1]
            }
        );

        let entry_end = entry
            .end
            .map_or("[]".to_string(), |end| end.kind.to_string());

        let prefix = format!("{}{}", prefix, wrong_tree_suffix);

        let name = entry.name.as_str().white();

        if wrong_tree {
            outln!(
                self.sess.writer,
                "{}{} `{}`-`{}`",
                prefix,
                name,
                entry.start.kind,
                entry_end
            );

            let children = entry.children.clone();
            let mut it = children.iter().peekable();
            while let Some(&child) = it.next() {
                self.print_parse_entry(
                    child,
                    ParseEntryPrinter {
                        parent_prefix: &parent_prefix,
                        is_last: it.peek().is_none(),
                        continuous: false,
                        recovering,
                    },
                );
            }

            return;
        }

        match entry.kind {
            ParseEntryKind::Opt => {
                out!(
                    self.sess.writer,
                    "{}{}? `{}`-`{}` - {} ",
                    prefix,
                    name,
                    entry.start.kind,
                    entry_end,
                    if entry.children.is_empty() {
                        "no".black()
                    } else {
                        "yes".white()
                    }
                );

                if let Some(&child) = entry.children.first() {
                    self.print_parse_entry(
                        child,
                        ParseEntryPrinter {
                            parent_prefix: &parent_prefix,
                            is_last: true,
                            continuous: true,
                            recovering,
                        },
                    );
                } else {
                    self.sess.writer.nl();
                }
            },
            ParseEntryKind::Wrapper
            | ParseEntryKind::ExpectWrapper
            | ParseEntryKind::RecoverWrapper => {
                let expect = entry.kind == ParseEntryKind::ExpectWrapper;

                out!(
                    self.sess.writer,
                    "{}[{}]{} `{}`-`{}` - ",
                    prefix,
                    name,
                    if expect { "!" } else { "" },
                    entry.start.kind,
                    entry_end,
                );

                if let Some(&child) = entry.children.first() {
                    self.print_parse_entry(
                        child,
                        ParseEntryPrinter {
                            parent_prefix: &parent_prefix,
                            is_last: true,
                            continuous: true,
                            recovering,
                        },
                    );
                } else {
                    self.sess.writer.nl();
                }
            },
            ParseEntryKind::Expect => {
                outln!(
                    self.sess.writer,
                    "{}{}! `{}`-`{}`",
                    prefix,
                    name,
                    entry.start.kind,
                    entry_end,
                );

                let children = entry.children.clone();
                let mut it = children.iter().peekable();
                while let Some(&child) = it.next() {
                    self.print_parse_entry(
                        child,
                        ParseEntryPrinter {
                            parent_prefix: &parent_prefix,
                            is_last: it.peek().is_none(),
                            continuous: false,
                            recovering,
                        },
                    );
                }
            },
            ParseEntryKind::ExpectToken | ParseEntryKind::LateCheck => {
                let symbol = match entry.kind {
                    ParseEntryKind::ExpectToken => "!",
                    ParseEntryKind::LateCheck => "",
                    _ => unreachable!(),
                };

                outln!(
                    self.sess.writer,
                    "{}{}{} `{}`-`{}`",
                    prefix,
                    name,
                    symbol,
                    entry.start.kind,
                    entry_end,
                );
            },
        };
    }
}

impl<'ast> Stage<AST> for Parser {
    fn run(mut self) -> StageOutput<AST> {
        let ast: AST = self.parse();
        StageOutput::new(self.sess, ast, self.msg)
    }
}

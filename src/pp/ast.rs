use crate::{
    ast::{
        expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, TyExpr},
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        visitor::walk_pr,
        visitor::AstVisitor,
        ErrorNode, NodeId, Path, WithNodeId, AST, N, PR,
    },
    parser::token::Punct,
    span::span::{Ident, Kw},
};

use super::AstLikePP;

macro_rules! walk_block {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {{
        $self.indent();
        $prs.iter().for_each(|pr| {
            $self.out_indent();
            walk_pr!($self, pr, $ok_visitor);
            $self.nl();
        });
        $self.dedent();
    }};
}

// Would be nice if intersperse was stable
macro_rules! walk_each_pr_delim {
    ($self: ident, $prs: expr, $ok_visitor: ident, $sep: expr) => {
        $prs.iter().enumerate().for_each(|(index, pr)| {
            walk_pr!($self, pr, $ok_visitor);
            if index < $prs.len() - 1 {
                $self.str($sep);
            }
        })
    };
}

impl<'ast> AstVisitor<'ast> for AstLikePP<'ast, ()> {
    fn visit_err(&mut self, err: &'ast ErrorNode) {
        if let Some(parsed) = err.parsed() {
            self.string(parsed);
            self.out.push(' ');
        }

        self.out.push_str("[ERROR]")
    }

    fn visit_ast(&mut self, ast: &'ast AST) {
        walk_each_pr_delim!(self, ast.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => walk_pr!(self, expr, visit_expr),
            StmtKind::Item(item) => walk_pr!(self, item, visit_item),
        }
        self.node_id(stmt);
    }

    // Items //
    fn visit_item(&mut self, item: &'ast Item) {
        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
        }
        self.node_id(item);
    }

    fn visit_type_item(&mut self, name: &'ast PR<Ident>, ty: &'ast PR<N<Ty>>, id: NodeId) {
        self.kw(Kw::Type);
        walk_pr!(self, name, name, id);
        self.punct(Punct::Assign);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &'ast PR<Ident>, items: &'ast Vec<PR<N<Item>>>, id: NodeId) {
        self.kw(Kw::Mod);
        walk_pr!(self, name, name, id);
        self.nl();
        walk_block!(self, items, visit_item);
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, name, id);
        if !params.is_empty() {
            self.sp();
        }
        walk_each_pr_delim!(self, params, visit_pat, " ");
        self.punct(Punct::Assign);
        walk_pr!(self, body, visit_expr);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &'ast Pat) {
        match pat.kind() {
            PatKind::Ident(ident) => walk_pr!(self, ident, name, pat.id()),
        }
        self.node_id(pat);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr.kind() {
            ExprKind::Unit => self.visit_unit_expr(),
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => {
                self.ch('(');
                walk_pr!(self, inner, visit_expr);
                self.ch(')');
            },
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
        }
        self.node_id(expr);
    }

    fn visit_unit_expr(&mut self) {
        self.str("()");
    }

    fn visit_lit_expr(&mut self, lit: &'ast Lit) {
        self.string(lit);
    }

    fn visit_infix_expr(&mut self, infix: &'ast Infix) {
        walk_pr!(self, &infix.lhs, visit_expr);
        self.infix(&infix.op);
        walk_pr!(self, &infix.rhs, visit_expr);
    }

    fn visit_lambda_expr(&mut self, lambda: &'ast Lambda) {
        self.punct(Punct::Backslash);
        walk_pr!(self, &lambda.param, visit_pat);
        self.punct(Punct::Arrow);
        walk_pr!(self, &lambda.body, visit_expr);
    }

    fn visit_app_expr(&mut self, call: &'ast Call) {
        walk_pr!(self, &call.lhs, visit_expr);
        self.sp();
        walk_pr!(self, &call.arg, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &'ast PR<Block>) {
        self.kw(Kw::Let);
        walk_pr!(self, block, visit_block);
    }

    fn visit_type_expr(&mut self, ty_expr: &'ast TyExpr) {
        walk_pr!(self, &ty_expr.expr, visit_expr);
        self.punct(Punct::Colon);
        walk_pr!(self, &ty_expr.ty, visit_ty);
    }

    // Types //
    fn visit_ty(&mut self, ty: &'ast Ty) {
        match ty.kind() {
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
        }
        self.node_id(ty);
    }

    fn visit_unit_ty(&mut self) {
        self.str("()");
    }

    fn visit_func_ty(&mut self, param_ty: &'ast PR<N<Ty>>, return_ty: &'ast PR<N<Ty>>) {
        walk_pr!(self, param_ty, visit_ty);
        self.punct(Punct::Arrow);
        walk_pr!(self, return_ty, visit_ty);
    }

    fn visit_paren_ty(&mut self, inner: &'ast PR<N<Ty>>) {
        self.punct(Punct::LParen);
        walk_pr!(self, inner, visit_ty);
        self.punct(Punct::RParen);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &'ast Ident) {
        self.string(ident);
    }

    fn visit_path(&mut self, path: &'ast Path) {
        self.path(path)
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.nl();
        walk_block!(self, block.stmts(), visit_stmt);
    }
}

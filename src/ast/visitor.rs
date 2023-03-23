use crate::span::span::Ident;

use super::{
    expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, TyExpr},
    item::{Item, ItemKind},
    pat::{Pat, PatKind},
    stmt::{Stmt, StmtKind},
    ty::{Ty, TyKind, TyPath},
    ErrorNode, NodeId, Path, WithNodeId, AST, N, PR,
};

macro_rules! walk_pr {
    ($self: ident, $pr: expr, $ok_visitor: ident) => {
        walk_pr!($self, $pr, $ok_visitor,)
    };

    ($self: ident, $pr: expr, $ok_visitor: ident, $($args: expr),*) => {
        match $pr {
            Ok(ok) => $self.$ok_visitor(ok, $($args),*),
            Err(err) => $self.visit_err(err),
        }
    };
}

pub(crate) use walk_pr;

macro_rules! walk_each_pr {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {
        for pr in $prs {
            match pr {
                Ok(ok) => $self.$ok_visitor(ok),
                Err(err) => $self.visit_err(err),
            }
        }
    };
}

pub(crate) use walk_each_pr;

/// This might be not a right solution, but AstVisitor as HirVisitor is a walker by default,
///  i.e it walks through all nodes as if visitors return unit type.
/// It may cause silent non-implemented problem in visitors with non-unit return type, but it allows
///  me to use visitors both for PP and walking in some stages where we don't need to implement visitor
///  for all nodes.
/// So keep in mind implementing all visitors you need or
pub trait AstVisitor<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode);

    fn visit_ast(&mut self, ast: &'ast AST) {
        walk_each_pr!(self, ast.items(), visit_item);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &'ast PR<N<Expr>>) {
        walk_pr!(self, expr, visit_expr)
    }

    fn visit_item_stmt(&mut self, item: &'ast PR<N<Item>>) {
        walk_pr!(self, item, visit_item)
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
    }

    fn visit_type_item(&mut self, name: &'ast PR<Ident>, ty: &'ast PR<N<Ty>>, _: NodeId) {
        walk_pr!(self, name, visit_ident);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &'ast PR<Ident>, items: &'ast Vec<PR<N<Item>>>, _: NodeId) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, items, visit_item);
    }

    fn visit_decl_name(&mut self, name: &'ast PR<Ident>) {
        walk_pr!(self, name, visit_ident);
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        _: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, params, visit_pat);
        walk_pr!(self, body, visit_expr);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &'ast Pat) {
        match pat.kind() {
            PatKind::Unit => self.visit_unit_pat(),
            PatKind::Ident(ident) => walk_pr!(self, ident, visit_ident_pat),
        }
    }

    fn visit_unit_pat(&mut self) {}

    fn visit_ident_pat(&mut self, ident: &'ast Ident) {
        self.visit_ident(ident);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => walk_pr!(self, inner, visit_expr),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
        }
    }

    fn visit_unit_expr(&mut self) {}

    fn visit_lit_expr(&mut self, _: &'ast Lit) {}

    fn visit_path_expr(&mut self, path: &'ast PathExpr) {
        walk_pr!(self, &path.0, visit_path)
    }

    fn visit_block_expr(&mut self, block: &'ast PR<Block>) {
        walk_pr!(self, block, visit_block)
    }

    fn visit_infix_expr(&mut self, infix: &'ast Infix) {
        walk_pr!(self, &infix.lhs, visit_expr);
        self.visit_path_expr(&infix.op);
        walk_pr!(self, &infix.rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, call: &'ast Call) {
        walk_pr!(self, &call.lhs, visit_expr);
        walk_pr!(self, &call.arg, visit_expr);
    }

    fn visit_lambda_expr(&mut self, lambda: &'ast Lambda) {
        walk_each_pr!(self, &lambda.params, visit_pat);
        walk_pr!(self, &lambda.body, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &'ast PR<Block>) {
        walk_pr!(self, block, visit_block)
    }

    fn visit_type_expr(&mut self, ty_expr: &'ast TyExpr) {
        walk_pr!(self, &ty_expr.expr, visit_expr);
        walk_pr!(self, &ty_expr.ty, visit_ty);
    }

    // Types //
    fn visit_ty(&mut self, ty: &'ast Ty) {
        match ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
            TyKind::App(cons, arg) => self.visit_ty_app(cons, arg),
            TyKind::AppExpr(cons, const_arg) => self.visit_ty_app_expr(cons, const_arg),
        }
    }

    fn visit_unit_ty(&mut self) {}

    fn visit_ty_path(&mut self, path: &'ast TyPath) {
        walk_pr!(self, &path.0, visit_path)
    }

    fn visit_func_ty(&mut self, param_ty: &'ast PR<N<Ty>>, return_ty: &'ast PR<N<Ty>>) {
        walk_pr!(self, param_ty, visit_ty);
        walk_pr!(self, return_ty, visit_ty)
    }

    fn visit_paren_ty(&mut self, inner: &'ast PR<N<Ty>>) {
        walk_pr!(self, inner, visit_ty)
    }

    fn visit_ty_app(&mut self, cons: &'ast PR<N<Ty>>, arg: &'ast PR<N<Ty>>) {
        walk_pr!(self, cons, visit_ty);
        walk_pr!(self, arg, visit_ty);
    }

    fn visit_ty_app_expr(&mut self, cons: &'ast PR<N<Ty>>, const_arg: &'ast PR<N<Expr>>) {
        walk_pr!(self, cons, visit_ty);
        walk_pr!(self, const_arg, visit_expr);
    }

    // Fragments //
    fn visit_ident(&mut self, _: &'ast Ident) {}

    fn visit_path(&mut self, _: &'ast Path) {}

    fn visit_block(&mut self, block: &'ast Block) {
        walk_each_pr!(self, block.stmts(), visit_stmt);
    }
}

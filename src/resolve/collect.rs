use std::collections::HashMap;

use crate::{
    ast::{
        self,
        expr::{Expr, InfixOp, Lit, PrefixOp},
        stmt::ast::stmt::Stmt,
        visitor::{visit_each_pr, visit_pr, AstVisitor},
        N, PR,
    },
    span::span::{Ident, Symbol},
};

use super::def::{DefId, PerNS};

enum ModuleKind {
    File,
    Mod,
}

struct Module {
    kind: ModuleKind,
    per_ns: PerNS<HashMap<Symbol, DefId>>,
}

impl Module {
    fn new(kind: ModuleKind) -> Self {
        Self {
            kind,
            per_ns: Default::default(),
        }
    }
}

struct Scope {
    parent: Option<Box<Scope>>,
    bound_module: Option<Module>,
}

impl Scope {
    fn new(parent: Option<Box<Scope>>, bound_module: Option<Module>) -> Self {
        Self {
            parent,
            bound_module,
        }
    }
}

pub struct DefCollector {
    last_def_index: u32,
}

impl DefCollector {
    fn next_def_id(&mut self) -> DefId {
        let def_id = DefId(self.last_def_index);
        self.last_def_index += 1;
        def_id
    }
}

impl AstVisitor<()> for DefCollector {
    fn visit_err(&self, _: &ast::ErrorNode) {}

    fn visit_ast(&mut self, ast: &ast::AST) {
        visit_each_pr!(self, ast.stmts(), visit_stmt)
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {}

    fn visit_ident(&mut self, ident: &Ident) {}

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, arg, visit_expr);
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) {
        visit_pr!(self, body, visit_expr);
    }

    fn visit_block_expr(&mut self, stmts: &Vec<PR<N<ast::stmt::Stmt>>>) {
        visit_each_pr!(self, stmts, visit_stmt);
    }

    fn visit_let_expr(&mut self, name: &PR<Ident>, value: &PR<N<Expr>>, body: &PR<N<Expr>>) {
        visit_pr!(self, name, visit_ident);
        visit_pr!(self, value, visit_expr);
        visit_pr!(self, body, visit_expr);
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<ast::ty::Ty>>) {
        visit_pr!(self, expr, visit_expr);
        visit_pr!(self, ty, visit_ty);
    }

    fn visit_unit_ty(&mut self) {}

    fn visit_lit_ty(&mut self, lit_ty: &ast::ty::LitTy) {}

    fn visit_ident_expr(&mut self, ident: &Ident) {}

    fn visit_var_ty(&mut self, ident: &PR<Ident>) {
        visit_pr!(self, ident, visit_ident);
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<ast::ty::Ty>>, return_ty: &PR<N<ast::ty::Ty>>) {
        visit_pr!(self, param_ty, visit_ty);
        visit_pr!(self, return_ty, visit_ty);
    }
}

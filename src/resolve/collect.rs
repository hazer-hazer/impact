use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        self,
        expr::{Expr, ExprKind, InfixOp, Lit, PrefixOp},
        ty::Ty,
        visitor::{visit_each_pr, visit_pr, AstVisitor},
        NodeId, N, PR,
    },
    span::span::{Ident, Symbol},
};

use super::def::{DefId, PerNS};

enum ModuleKind {
    Root,
    Block(NodeId),
    Def(DefId),
}

#[derive(Debug, Clone, Copy)]
struct ModuleId(u32);

struct ModuleArena {
    modules: Vec<Module>,
}

impl ModuleArena {
    pub fn alloc(&mut self, module: Module) -> ModuleId {
        let id = ModuleId(self.modules.len() as u32);
        self.modules.push(module);
        id
    }
}

impl std::ops::Index<ModuleId> for ModuleArena {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[index.0 as usize]
    }
}

impl std::ops::IndexMut<ModuleId> for ModuleArena {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self.modules[index.0 as usize]
    }
}

/**
 * Module is a scope where items defined.
 */
struct Module {
    parent: Option<ModuleId>,
    kind: ModuleKind,
    per_ns: PerNS<HashMap<Symbol, DefId>>,
}

impl Module {
    fn root() -> Self {
        Self {
            parent: None,
            kind: ModuleKind::Root,
            per_ns: Default::default(),
        }
    }

    fn new(parent: ModuleId, kind: ModuleKind) -> Self {
        Self {
            parent: Some(parent),
            kind,
            per_ns: Default::default(),
        }
    }
}

pub struct DefCollector {
    last_def_index: u32,
    current_module: ModuleId,
    modules: ModuleArena,
}

impl DefCollector {
    fn next_def_id(&mut self) -> DefId {
        let def_id = DefId(self.last_def_index);
        self.last_def_index += 1;
        def_id
    }

    fn module(&mut self) -> &mut Module {
        &mut self.modules[self.current_module]
    }

    fn enter_module(&mut self, kind: ModuleKind) {
        self.current_module = self.modules.alloc(Module::new(self.current_module, kind));
    }

    fn enter_def_module(&mut self, def_id: DefId) {
        self.enter_module(ModuleKind::Def(def_id))
    }

    fn enter_block_module(&mut self, node_id: NodeId) {
        self.enter_module(ModuleKind::Block(node_id))
    }

    fn exit_module(&mut self) {
        self.current_module = self.modules[self.current_module]
            .parent
            .expect("Tried to exit root module")
    }
}

impl AstVisitor<()> for DefCollector {
    fn visit_err(&self, _: &ast::ErrorNode) {}

    fn visit_ast(&mut self, ast: &ast::AST) {
        visit_each_pr!(self, ast.items(), visit_item)
    }

    // Items //
    fn visit_type_item(&mut self, _: &PR<Ident>, _: &PR<N<Ty>>) {}

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> () {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            ExprKind::Infix(lhs, op, rhs) => self.visit_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.visit_prefix_expr(op, rhs),
            ExprKind::App(lhs, arg) => self.visit_app_expr(lhs, arg),
            ExprKind::Block(stmts) => {
                self.enter_block_module(expr.id());
                self.visit_block_expr(stmts);
                self.exit_module();
            }
            ExprKind::Let(name, value, body) => self.visit_let_expr(name, value, body),
            ExprKind::Abs(param, body) => self.visit_abs_expr(param, body),
            ExprKind::Ty(expr, ty) => self.visit_type_expr(expr, ty),
        }
    }

    fn visit_lit_expr(&mut self, _: &Lit) {}

    fn visit_ident(&mut self, _: &Ident) {}

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, _: &InfixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_prefix_expr(&mut self, _: &PrefixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, arg, visit_expr);
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) {
        visit_pr!(self, param, visit_ident);
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

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) {
        visit_pr!(self, expr, visit_expr);
        visit_pr!(self, ty, visit_ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {}

    fn visit_lit_ty(&mut self, _: &ast::ty::LitTy) {}

    fn visit_ident_expr(&mut self, ident: &Ident) {
        self.visit_ident(ident)
    }

    fn visit_var_ty(&mut self, ident: &PR<Ident>) {
        visit_pr!(self, ident, visit_ident);
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) {
        visit_pr!(self, param_ty, visit_ty);
        visit_pr!(self, return_ty, visit_ty);
    }
}

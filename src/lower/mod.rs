use crate::{
    ast::{
        expr::{Expr, ExprKind, InfixOp, Lit, PrefixOp},
        item::{Item, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::{LitTy, Ty, TyKind},
        AST, N, PR,
    },
    hir::{self, HIR},
    message::message::MessageStorage,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, WithSpan},
};

macro_rules! lower_pr_boxed {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => Box::new($self.$lower(node)),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_pr {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => $self.$lower(node),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_each_pr_boxed {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.iter()
            .map(|pr| lower_pr_boxed!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

macro_rules! lower_each_pr {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.iter()
            .map(|pr| lower_pr!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

pub struct Lower<'a> {
    sess: Session,
    ast: &'a AST,
    msg: MessageStorage,
}

impl<'a> Lower<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            sess,
            ast,
            msg: Default::default(),
        }
    }

    fn lower_ast(&mut self) -> HIR {
        HIR::new(lower_each_pr_boxed!(self, self.ast.items(), lower_item))
    }

    // Statements //
    fn lower_stmt(&mut self, stmt: &Stmt) -> hir::stmt::Stmt {
        match stmt.kind() {
            StmtKind::Expr(expr) => hir::stmt::Stmt::new(
                hir::stmt::StmtKind::Expr(lower_pr!(self, expr, lower_expr)),
                stmt.span(),
            ),
            StmtKind::Item(item) => todo!(),
        }
    }

    // Items //
    fn lower_item(&mut self, item: &Item) -> hir::item::Item {
        let kind = match item.kind() {
            ItemKind::Type(name, ty) => self.lower_type_item(name, ty),
            ItemKind::Mod(name, items) => self.lower_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.lower_decl_item(name, params, body),
        };

        hir::item::Item::new(kind, item.span())
    }

    fn lower_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) -> hir::item::ItemKind {
        todo!()
    }

    fn lower_mod_item(
        &mut self,
        name: &PR<Ident>,
        items: &Vec<PR<N<Item>>>,
    ) -> hir::item::ItemKind {
        hir::item::ItemKind::Mod(
            lower_pr!(self, name, lower_ident),
            lower_each_pr!(self, items, lower_item),
        )
    }

    fn lower_decl_item(
        &mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Ident>>,
        body: &PR<N<Expr>>,
    ) -> hir::item::ItemKind {
        hir::item::ItemKind::Decl(
            lower_pr!(self, name, lower_ident),
            lower_each_pr!(self, params, lower_ident),
            lower_pr!(self, body, lower_expr),
        )
    }

    // Expressions //
    fn lower_expr(&mut self, expr: &Expr) -> hir::expr::Expr {
        let kind = match expr.kind() {
            ExprKind::Lit(lit) => self.lower_lit_expr(lit),
            ExprKind::Ident(ident) => self.lower_ident_expr(ident),
            ExprKind::Infix(lhs, op, rhs) => self.lower_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.lower_prefix_expr(op, rhs),
            ExprKind::Abs(param, body) => self.lower_abs_expr(param, body),
            ExprKind::App(lhs, arg) => self.lower_app_expr(lhs, arg),
            ExprKind::Block(stmts) => self.lower_block_expr(stmts),
            ExprKind::Let(name, value, body) => self.lower_let_expr(name, value, body),
            ExprKind::Ty(expr, ty) => self.lower_ty_expr(expr, ty),
        };

        hir::expr::Expr::new(kind, expr.span())
    }

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lit(*lit)
    }

    fn lower_ident_expr(&mut self, ident: &Ident) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ident(*ident)
    }

    fn lower_infix_expr(
        &mut self,
        lhs: &PR<N<Expr>>,
        op: &InfixOp,
        rhs: &PR<N<Expr>>,
    ) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Infix(
            lower_pr_boxed!(self, lhs, lower_expr),
            op.clone(),
            lower_pr_boxed!(self, rhs, lower_expr),
        )
    }

    fn lower_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Prefix(op.clone(), lower_pr_boxed!(self, rhs, lower_expr))
    }

    fn lower_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Abs(
            lower_pr!(self, param, lower_ident),
            lower_pr_boxed!(self, body, lower_expr),
        )
    }

    fn lower_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::App(
            lower_pr_boxed!(self, lhs, lower_expr),
            lower_pr_boxed!(self, arg, lower_expr),
        )
    }

    fn lower_block_expr(&mut self, stmts: &Vec<PR<N<Stmt>>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Block(lower_each_pr!(self, stmts, lower_stmt))
    }

    fn lower_let_expr(
        &mut self,
        name: &PR<Ident>,
        value: &PR<N<Expr>>,
        body: &PR<N<Expr>>,
    ) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Let(
            lower_pr!(self, name, lower_ident),
            lower_pr_boxed!(self, value, lower_expr),
            lower_pr_boxed!(self, body, lower_expr),
        )
    }

    fn lower_ty_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ty(
            lower_pr_boxed!(self, expr, lower_expr),
            lower_pr!(self, ty, lower_ty),
        )
    }

    // Types //
    fn lower_ty(&mut self, ty: &Ty) -> hir::ty::Ty {
        let kind = match ty.kind() {
            TyKind::Unit => self.lower_unit_ty(),
            TyKind::Lit(lit_ty) => self.lower_lit_ty(lit_ty),
            TyKind::Var(ident) => self.lower_var_ty(ident),
            TyKind::Func(param_ty, return_ty) => self.lower_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => return lower_pr!(self, inner, lower_ty),
        };

        hir::ty::Ty::new(kind, ty.span())
    }

    fn lower_unit_ty(&mut self) -> hir::ty::TyKind {
        hir::ty::TyKind::Unit
    }

    fn lower_lit_ty(&mut self, lit_ty: &LitTy) -> hir::ty::TyKind {
        hir::ty::TyKind::Lit(*lit_ty)
    }

    fn lower_var_ty(&mut self, ident: &PR<Ident>) -> hir::ty::TyKind {
        hir::ty::TyKind::Var(lower_pr!(self, ident, lower_ident))
    }

    fn lower_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> hir::ty::TyKind {
        hir::ty::TyKind::Func(
            lower_pr_boxed!(self, param_ty, lower_ty),
            lower_pr_boxed!(self, return_ty, lower_ty),
        )
    }

    // Fragments //
    fn lower_ident(&mut self, ident: &Ident) -> Ident {
        *ident
    }
}

impl<'a> Stage<HIR> for Lower<'a> {
    fn run(mut self) -> StageOutput<HIR> {
        let hir = self.lower_ast();
        StageOutput::new(self.sess, hir, self.msg)
    }
}

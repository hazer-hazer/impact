use crate::{
    ast::{
        expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
        item::{Item, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        Path, WithNodeId, AST, N, PR,
    },
    hir::{self, HIR},
    message::message::{Message, MessageBuilder, MessageStorage},
    resolve::res::NamePath,
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
            StmtKind::Item(item) => hir::stmt::Stmt::new(
                hir::stmt::StmtKind::Item(lower_pr!(self, item, lower_item)),
                stmt.span(),
            ),
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
        hir::item::ItemKind::Type(
            lower_pr!(self, name, lower_ident),
            lower_pr_boxed!(self, ty, lower_ty),
        )
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
            ExprKind::Unit => hir::expr::ExprKind::Unit,
            ExprKind::Lit(lit) => self.lower_lit_expr(lit),
            ExprKind::Path(path) => self.lower_path_expr(path),
            ExprKind::Block(block) => self.lower_block_expr(block),
            ExprKind::Infix(lhs, op, rhs) => self.lower_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.lower_prefix_expr(op, rhs),
            ExprKind::Abs(param, body) => self.lower_abs_expr(param, body),
            ExprKind::App(lhs, arg) => self.lower_app_expr(lhs, arg),
            ExprKind::Let(block) => self.lower_let_expr(block),
            ExprKind::Ty(expr, ty) => self.lower_ty_expr(expr, ty),
        };

        hir::expr::Expr::new(kind, expr.span())
    }

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lit(*lit)
    }

    fn lower_path_expr(&mut self, path: &PR<Path>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Path(lower_pr!(self, path, lower_path))
    }

    fn lower_block_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Block(lower_pr!(self, block, lower_block))
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

    fn lower_let_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Let(lower_pr!(self, block, lower_block))
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
            TyKind::Path(path) => self.lower_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.lower_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => return lower_pr!(self, inner, lower_ty),
        };

        hir::ty::Ty::new(kind, ty.span())
    }

    fn lower_unit_ty(&mut self) -> hir::ty::TyKind {
        hir::ty::TyKind::Unit
    }

    fn lower_path_ty(&mut self, path: &PR<Path>) -> hir::ty::TyKind {
        hir::ty::TyKind::Path(lower_pr!(self, path, lower_path))
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

    fn lower_path(&mut self, path: &Path) -> hir::Path {
        hir::Path::new(
            self.sess.res.get(NamePath::new(path.id())).unwrap(),
            path.segments().clone(),
        )
    }

    fn lower_block(&mut self, block: &Block) -> hir::expr::Block {
        assert!(!block.stmts().is_empty());

        let stmts = lower_each_pr!(self, block.stmts()[0..block.stmts().len() - 1], lower_stmt);

        let expr = match block.stmts().last().unwrap().as_ref().unwrap().kind() {
            StmtKind::Expr(expr) => Some(lower_pr_boxed!(self, expr, lower_expr)),
            StmtKind::Item(_) => None,
        };

        hir::expr::Block::new(stmts, expr)
    }
}

impl<'a> Stage<HIR> for Lower<'a> {
    fn run(mut self) -> StageOutput<HIR> {
        let hir = self.lower_ast();
        StageOutput::new(self.sess, hir, self.msg)
    }
}

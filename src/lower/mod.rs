use crate::{
    ast::{
        expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
        item::{Item, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        Path, WithNodeId, AST, N, PR,
    },
    hir::{
        self,
        expr::{FuncCall, Infix, Lambda, PathExpr, Prefix, TyExpr},
        item::{Decl, Mod, TypeItem},
        HIR,
    },
    message::message::{Message, MessageBuilder, MessageStorage},
    parser::token::{FloatKind, IntKind},
    resolve::res::NamePath,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, WithSpan},
    typeck::{
        self,
        ty::{DEFAULT_FLOAT_KIND, DEFAULT_INT_KIND},
    },
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
        hir::item::ItemKind::Type(TypeItem {
            name: lower_pr!(self, name, lower_ident),
            ty: lower_pr_boxed!(self, ty, lower_ty),
        })
    }

    fn lower_mod_item(
        &mut self,
        name: &PR<Ident>,
        items: &Vec<PR<N<Item>>>,
    ) -> hir::item::ItemKind {
        hir::item::ItemKind::Mod(Mod {
            name: lower_pr!(self, name, lower_ident),
            items: lower_each_pr!(self, items, lower_item),
        })
    }

    fn lower_decl_item(
        &mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Ident>>,
        body: &PR<N<Expr>>,
    ) -> hir::item::ItemKind {
        hir::item::ItemKind::Decl(Decl {
            name: lower_pr!(self, name, lower_ident),
            params: lower_each_pr!(self, params, lower_ident),
            body: lower_pr!(self, body, lower_expr),
        })
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

    fn lower_int_kind(&mut self, kind: IntKind) -> hir::expr::IntKind {
        match kind {
            IntKind::Unknown => DEFAULT_INT_KIND,
            IntKind::Inferred(id) => todo!(),
            IntKind::U8 => hir::expr::IntKind::U8,
            IntKind::U16 => hir::expr::IntKind::U16,
            IntKind::U32 => hir::expr::IntKind::U32,
            IntKind::U64 => hir::expr::IntKind::U64,
            IntKind::I8 => hir::expr::IntKind::I8,
            IntKind::I16 => hir::expr::IntKind::I16,
            IntKind::I32 => hir::expr::IntKind::I32,
            IntKind::I64 => hir::expr::IntKind::I64,
            IntKind::Uint => hir::expr::IntKind::Uint,
            IntKind::Int => hir::expr::IntKind::Int,
        }
    }

    fn lower_float_kind(&self, kind: FloatKind) -> hir::expr::FloatKind {
        match kind {
            FloatKind::Unknown => DEFAULT_FLOAT_KIND,
            FloatKind::Inferred(id) => todo!(),
            FloatKind::F32 => hir::expr::FloatKind::F32,
            FloatKind::F64 => hir::expr::FloatKind::F64,
        }
    }

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lit(match lit {
            Lit::Bool(val) => hir::expr::Lit::Bool(*val),
            Lit::Int(val, kind) => hir::expr::Lit::Int(*val, self.lower_int_kind(*kind)),
            Lit::Float(val, kind) => hir::expr::Lit::Float(*val, self.lower_float_kind(*kind)),
            Lit::String(val) => hir::expr::Lit::String(*val),
        })
    }

    fn lower_path_expr(&mut self, path: &PR<Path>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Path(PathExpr(lower_pr!(self, path, lower_path)))
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
        hir::expr::ExprKind::Infix(Infix {
            lhs: lower_pr_boxed!(self, lhs, lower_expr),
            op: op.clone(),
            rhs: lower_pr_boxed!(self, rhs, lower_expr),
        })
    }

    fn lower_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Prefix(Prefix {
            op: op.clone(),
            rhs: lower_pr_boxed!(self, rhs, lower_expr),
        })
    }

    fn lower_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lambda(Lambda {
            param: lower_pr!(self, param, lower_ident),
            body: lower_pr_boxed!(self, body, lower_expr),
        })
    }

    fn lower_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Call(FuncCall {
            lhs: lower_pr_boxed!(self, lhs, lower_expr),
            arg: lower_pr_boxed!(self, arg, lower_expr),
        })
    }

    fn lower_let_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Let(lower_pr!(self, block, lower_block))
    }

    fn lower_ty_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ty(TyExpr {
            expr: lower_pr_boxed!(self, expr, lower_expr),
            ty: lower_pr!(self, ty, lower_ty),
        })
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

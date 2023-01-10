use crate::{
    ast::{
        expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, Prefix, TyExpr},
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        Path, PathSeg, WithNodeId, AST, N, PR,
    },
    hir::{
        self,
        item::{Decl, ItemId, Mod, TypeItem},
        Root, HIR,
    },
    message::message::MessageStorage,
    parser::token::{FloatKind, IntKind},
    resolve::res::NamePath,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, WithSpan},
};

macro_rules! lower_pr {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => $self.$lower(node),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_each_pr {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.into_iter()
            .map(|pr| lower_pr!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

macro_rules! lower_each {
    ($self: ident, $nodes: expr, $lower: ident) => {
        $nodes
            .into_iter()
            .map(|pr| $self.$lower(pr))
            .collect::<Vec<_>>()
    };
}

pub struct Lower<'ast> {
    ast: &'ast AST,
    hir: HIR,
    sess: Session,
    msg: MessageStorage,
}

impl<'ast> Lower<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            ast,
            hir: HIR::new(sess.def_table.defs_count()),
            sess,
            msg: Default::default(),
        }
    }

    fn lower_ast(&mut self) {
        let items = lower_each_pr!(self, self.ast.items(), lower_item);
        self.hir.set_root(items);
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
    fn lower_item(&mut self, item: &Item) -> ItemId {
        let kind = match item.kind() {
            ItemKind::Type(name, ty) => self.lower_type_item(name, ty),
            ItemKind::Mod(name, items) => self.lower_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.lower_decl_item(name, params, body),
        };

        let def_id = self.sess.def_table.get_def_id(item.id()).unwrap();

        self.hir
            .add_item(hir::item::Item::new(def_id, kind, item.span()))
    }

    fn lower_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) -> hir::item::ItemKind {
        hir::item::ItemKind::Type(TypeItem {
            name: lower_pr!(self, name, lower_ident),
            ty: lower_pr!(self, ty, lower_ty),
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
        params: &Vec<PR<Pat>>,
        body: &PR<N<Expr>>,
    ) -> hir::item::ItemKind {
        if params.is_empty() {
            hir::item::ItemKind::Decl(Decl {
                name: lower_pr!(self, name, lower_ident),
                value: lower_pr!(self, body, lower_expr),
            })
        } else {
            let mut value = lower_pr!(self, body, lower_expr);
            for param in params.iter().rev() {
                let param = lower_pr!(self, param, lower_pat);
                let span = param.span().to(body.span());
                value = Box::new(hir::expr::Expr::new(
                    hir::expr::ExprKind::Lambda(hir::expr::Lambda { param, body: value }),
                    span,
                ))
            }
            hir::item::ItemKind::Decl(Decl {
                name: lower_pr!(self, name, lower_ident),
                value,
            })
        }
    }

    // Patterns //
    fn lower_pat(&mut self, pat: &Pat) -> hir::pat::Pat {
        let kind = match pat.kind() {
            PatKind::Ident(ident) => hir::pat::PatKind::Ident(lower_pr!(self, ident, lower_ident)),
        };

        hir::pat::Pat::new(kind, pat.span())
    }

    // Expressions //
    fn lower_expr(&mut self, expr: &Expr) -> N<hir::expr::Expr> {
        let kind = match expr.kind() {
            ExprKind::Unit => hir::expr::ExprKind::Unit,
            ExprKind::Lit(lit) => self.lower_lit_expr(lit),
            ExprKind::Path(path) => self.lower_path_expr(path),
            ExprKind::Block(block) => self.lower_block_expr(block),
            ExprKind::Infix(infix) => self.lower_infix_expr(infix),
            ExprKind::Prefix(prefix) => self.lower_prefix_expr(prefix),
            ExprKind::Lambda(lambda) => self.lower_lambda_expr(lambda),
            ExprKind::Call(call) => self.lower_app_expr(call),
            ExprKind::Let(block) => self.lower_let_expr(block),
            ExprKind::Ty(ty_expr) => self.lower_ty_expr(ty_expr),
        };

        Box::new(hir::expr::Expr::new(kind, expr.span()))
    }

    fn lower_int_kind(&mut self, kind: IntKind) -> hir::expr::IntKind {
        match kind {
            IntKind::Unknown => todo!(),
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
            FloatKind::Unknown => todo!(),
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

    fn lower_path_expr(&mut self, path: &PathExpr) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Path(hir::expr::PathExpr(lower_pr!(self, &path.0, lower_path)))
    }

    fn lower_block_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Block(lower_pr!(self, block, lower_block))
    }

    fn lower_infix_expr(&mut self, infix: &Infix) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Infix(hir::expr::Infix {
            lhs: lower_pr!(self, &infix.lhs, lower_expr),
            op: infix.op,
            rhs: lower_pr!(self, &infix.rhs, lower_expr),
        })
    }

    fn lower_prefix_expr(&mut self, prefix: &Prefix) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Prefix(hir::expr::Prefix {
            op: prefix.op,
            rhs: lower_pr!(self, &prefix.rhs, lower_expr),
        })
    }

    fn lower_lambda_expr(&mut self, lambda: &Lambda) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lambda(hir::expr::Lambda {
            param: lower_pr!(self, &lambda.param, lower_pat),
            body: lower_pr!(self, &lambda.body, lower_expr),
        })
    }

    fn lower_app_expr(&mut self, call: &Call) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Call(hir::expr::Call {
            lhs: lower_pr!(self, &call.lhs, lower_expr),
            arg: lower_pr!(self, &call.arg, lower_expr),
        })
    }

    fn lower_let_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Let(lower_pr!(self, block, lower_block))
    }

    fn lower_ty_expr(&mut self, ty_expr: &TyExpr) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ty(hir::expr::TyExpr {
            expr: lower_pr!(self, &ty_expr.expr, lower_expr),
            ty: lower_pr!(self, &ty_expr.ty, lower_ty),
        })
    }

    // Types //
    fn lower_ty(&mut self, ty: &Ty) -> N<hir::ty::Ty> {
        let kind = match ty.kind() {
            TyKind::Unit => self.lower_unit_ty(),
            TyKind::Path(path) => self.lower_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.lower_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => return lower_pr!(self, inner, lower_ty),
        };

        Box::new(hir::ty::Ty::new(kind, ty.span()))
    }

    fn lower_unit_ty(&mut self) -> hir::ty::TyKind {
        hir::ty::TyKind::Unit
    }

    fn lower_path_ty(&mut self, path: &PR<Path>) -> hir::ty::TyKind {
        hir::ty::TyKind::Path(lower_pr!(self, path, lower_path))
    }

    fn lower_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> hir::ty::TyKind {
        hir::ty::TyKind::Func(
            lower_pr!(self, param_ty, lower_ty),
            lower_pr!(self, return_ty, lower_ty),
        )
    }

    // Fragments //
    fn lower_ident(&mut self, ident: &Ident) -> Ident {
        *ident
    }

    fn lower_path(&mut self, path: &Path) -> hir::Path {
        hir::Path::new(
            self.sess.res.get(NamePath::new(path.id())).unwrap(),
            lower_each!(self, path.segments(), lower_path_seg),
            path.span(),
        )
    }

    fn lower_path_seg(&mut self, seg: &PathSeg) -> hir::PathSeg {
        hir::PathSeg::new(self.lower_ident(seg.expect_name()), seg.span())
    }

    fn lower_block(&mut self, block: &Block) -> hir::expr::Block {
        assert!(!block.stmts().is_empty());

        let stmts = block.stmts()[0..block.stmts().len() - 1]
            .iter()
            .map(|stmt| lower_pr!(self, stmt, lower_stmt))
            .collect::<Vec<_>>();

        let expr = match block.stmts().last().unwrap().as_ref().unwrap().kind() {
            StmtKind::Expr(expr) => Some(lower_pr!(self, expr, lower_expr)),
            StmtKind::Item(_) => None,
        };

        hir::expr::Block::new(stmts, expr)
    }
}

impl<'ast> Stage<HIR> for Lower<'ast> {
    fn run(mut self) -> StageOutput<HIR> {
        self.lower_ast();
        StageOutput::new(self.sess, self.hir, self.msg)
    }
}

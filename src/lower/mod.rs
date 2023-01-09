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
        arena::Arena,
        item::{Decl, Mod, TypeItem},
        HIR,
    },
    interface::ctx::GlobalCtx,
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
            .collect::<Vec<&_>>()
    };
}

pub struct Lower<'hir> {
    ctx: GlobalCtx<'hir>,
    arena: &'hir mut Arena<'hir>,
    sess: Session,
    msg: MessageStorage,
}

impl<'hir> Lower<'hir> {
    pub fn new(sess: Session, ctx: GlobalCtx<'hir>) -> Self {
        Self {
            ctx,
            arena: &mut ctx.hir_arena,
            sess,
            msg: Default::default(),
        }
    }

    fn lower_ast(&mut self) -> HIR {
        HIR {}
    }

    // Statements //
    fn lower_stmt(&'hir mut self, stmt: &Stmt) -> &'hir hir::stmt::Stmt<'hir> {
        self.arena.alloc(match stmt.kind() {
            StmtKind::Expr(expr) => hir::stmt::Stmt::new(
                hir::stmt::StmtKind::Expr(lower_pr!(self, expr, lower_expr)),
                stmt.span(),
            ),
            StmtKind::Item(item) => hir::stmt::Stmt::new(
                hir::stmt::StmtKind::Item(lower_pr!(self, item, lower_item)),
                stmt.span(),
            ),
        })
    }

    // Items //
    fn lower_item(&'hir mut self, item: &Item) -> &'hir hir::item::Item<'hir> {
        let kind = match item.kind() {
            ItemKind::Type(name, ty) => self.lower_type_item(name, ty),
            ItemKind::Mod(name, items) => self.lower_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.lower_decl_item(name, params, body),
        };

        let def_id = self.sess.def_table.get_def_id(item.id()).unwrap();

        self.arena
            .alloc(hir::item::Item::new(def_id, kind, item.span()))
    }

    fn lower_type_item(
        &'hir mut self,
        name: &PR<Ident>,
        ty: &PR<N<Ty>>,
    ) -> hir::item::ItemKind<'hir> {
        hir::item::ItemKind::Type(TypeItem {
            name: lower_pr!(self, name, lower_ident),
            ty: lower_pr!(self, ty, lower_ty),
        })
    }

    fn lower_mod_item(
        &'hir mut self,
        name: &PR<Ident>,
        items: &Vec<PR<N<Item>>>,
    ) -> hir::item::ItemKind<'hir> {
        hir::item::ItemKind::Mod(Mod {
            name: lower_pr!(self, name, lower_ident),
            items: self.lower_items(items),
        })
    }

    fn lower_items(&'hir mut self, ast_items: &Vec<PR<N<Item>>>) -> &'hir [&'hir hir::item::Item] {
        let items = vec![];
        for item in ast_items {
            items.push(self.lower_item(&item.unwrap()));
        }
        &items
    }

    fn lower_decl_item(
        &'hir mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Pat>>,
        body: &PR<N<Expr>>,
    ) -> hir::item::ItemKind<'hir> {
        if params.is_empty() {
            hir::item::ItemKind::Decl(Decl {
                name: lower_pr!(self, name, lower_ident),
                value: lower_pr!(self, body, lower_expr),
            })
        } else {
            let mut value = self.lower_expr(&body.unwrap());
            for param in params.iter().rev() {
                let param = self.lower_pat(&param.unwrap());
                value = self.arena.alloc(hir::expr::Expr::new(
                    hir::expr::ExprKind::Lambda(hir::expr::Lambda { param, body: value }),
                    param.span(),
                ))
            }
            hir::item::ItemKind::Decl(Decl {
                name: lower_pr!(self, name, lower_ident),
                value,
            })
        }
    }

    // Patterns //
    fn lower_pat(&'hir mut self, pat: &Pat) -> &'hir hir::pat::Pat<'hir> {
        let kind = match pat.kind() {
            PatKind::Ident(ident) => hir::pat::PatKind::Ident(lower_pr!(self, ident, lower_ident)),
        };

        self.arena.alloc(hir::pat::Pat::new(kind, pat.span()))
    }

    fn lower_pats(&'hir mut self, ast_pats: &Vec<PR<Pat>>) -> &'hir [&'hir hir::pat::Pat<'hir>] {
        let pats = vec![];
        for pat in ast_pats {
            pats.push(self.lower_pat(&pat.unwrap()));
        }
        &pats
    }

    // Expressions //
    fn lower_expr(&'hir mut self, expr: &Expr) -> &'hir hir::expr::Expr<'hir> {
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

        self.arena.alloc(hir::expr::Expr::new(kind, expr.span()))
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

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Lit(match lit {
            Lit::Bool(val) => hir::expr::Lit::Bool(*val),
            Lit::Int(val, kind) => hir::expr::Lit::Int(*val, self.lower_int_kind(*kind)),
            Lit::Float(val, kind) => hir::expr::Lit::Float(*val, self.lower_float_kind(*kind)),
            Lit::String(val) => hir::expr::Lit::String(*val),
        })
    }

    fn lower_path_expr(&mut self, path: &PathExpr) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Path(hir::expr::PathExpr(lower_pr!(self, &path.0, lower_path)))
    }

    fn lower_block_expr(&'hir mut self, block: &PR<Block>) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Block(lower_pr!(self, block, lower_block))
    }

    fn lower_infix_expr(&'hir mut self, infix: &Infix) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Infix(hir::expr::Infix {
            lhs: lower_pr!(self, &infix.lhs, lower_expr),
            op: infix.op,
            rhs: lower_pr!(self, &infix.rhs, lower_expr),
        })
    }

    fn lower_prefix_expr(&'hir mut self, prefix: &Prefix) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Prefix(hir::expr::Prefix {
            op: prefix.op,
            rhs: lower_pr!(self, &prefix.rhs, lower_expr),
        })
    }

    fn lower_lambda_expr(&'hir mut self, lambda: &Lambda) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Lambda(hir::expr::Lambda {
            param: lower_pr!(self, &lambda.param, lower_pat),
            body: lower_pr!(self, &lambda.body, lower_expr),
        })
    }

    fn lower_app_expr(&'hir mut self, call: &Call) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Call(hir::expr::Call {
            lhs: lower_pr!(self, &call.lhs, lower_expr),
            arg: lower_pr!(self, &call.arg, lower_expr),
        })
    }

    fn lower_let_expr(&'hir mut self, block: &PR<Block>) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Let(lower_pr!(self, block, lower_block))
    }

    fn lower_ty_expr(&'hir mut self, ty_expr: &TyExpr) -> hir::expr::ExprKind<'hir> {
        hir::expr::ExprKind::Ty(hir::expr::TyExpr {
            expr: lower_pr!(self, &ty_expr.expr, lower_expr),
            ty: lower_pr!(self, &ty_expr.ty, lower_ty),
        })
    }

    // Types //
    fn lower_ty(&'hir mut self, ty: &Ty) -> &'hir hir::ty::Ty<'hir> {
        let kind = match ty.kind() {
            TyKind::Unit => self.lower_unit_ty(),
            TyKind::Path(path) => self.lower_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.lower_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => return lower_pr!(self, inner, lower_ty),
        };

        self.arena.alloc(hir::ty::Ty::new(kind, ty.span()))
    }

    fn lower_unit_ty(&mut self) -> hir::ty::TyKind<'hir> {
        hir::ty::TyKind::Unit
    }

    fn lower_path_ty(&mut self, path: &PR<Path>) -> hir::ty::TyKind<'hir> {
        hir::ty::TyKind::Path(lower_pr!(self, path, lower_path))
    }

    fn lower_func_ty(
        &'hir mut self,
        param_ty: &PR<N<Ty>>,
        return_ty: &PR<N<Ty>>,
    ) -> hir::ty::TyKind<'hir> {
        hir::ty::TyKind::Func(
            lower_pr!(self, param_ty, lower_ty),
            lower_pr!(self, return_ty, lower_ty),
        )
    }

    // Fragments //
    fn lower_ident(&mut self, ident: &Ident) -> Ident {
        *ident
    }

    fn lower_path(&mut self, path: &Path) -> &'hir hir::Path<'hir> {
        self.arena.alloc(hir::Path::new(
            self.sess.res.get(NamePath::new(path.id())).unwrap(),
            &path
                .segments()
                .iter()
                .map(|seg| self.lower_path_seg(seg))
                .collect::<Vec<_>>(),
            path.span(),
        ))
    }

    fn lower_path_seg(&mut self, seg: &PathSeg) -> &'hir hir::PathSeg {
        self.arena.alloc(hir::PathSeg::new(
            self.lower_ident(seg.expect_name()),
            seg.span(),
        ))
    }

    fn lower_block(&'hir mut self, block: &Block) -> &'hir hir::expr::Block<'hir> {
        assert!(!block.stmts().is_empty());

        let stmts = vec![];
        for stmt in &block.stmts()[0..block.stmts().len() - 1] {
            stmts.push(self.lower_stmt(&stmt.unwrap()));
        }

        let expr = match block.stmts().last().unwrap().as_ref().unwrap().kind() {
            StmtKind::Expr(expr) => Some(lower_pr!(self, expr, lower_expr)),
            StmtKind::Item(_) => None,
        };

        self.arena.alloc(hir::expr::Block::new(&stmts, expr))
    }
}

impl<'hir> Stage<(HIR, GlobalCtx<'hir>)> for Lower<'hir> {
    fn run(mut self) -> StageOutput<(HIR, GlobalCtx<'hir>)> {
        let hir = self.lower_ast();
        StageOutput::new(self.sess, (hir, self.ctx), self.msg)
    }
}

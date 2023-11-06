use super::{AstLikePP, AstPPMode};
use crate::{
    cli::color::Colorize,
    hir::{
        expr::{Arm, Call, ExprKind, Lambda, Lit, TyExpr},
        item::{Adt, ExternItem, Field, GenericParams, ItemId, Mod, TyAlias, ROOT_ITEM_ID},
        pat::PatKind,
        stmt::{Local, StmtKind},
        ty::TyKind,
        visitor::HirVisitor,
        Block, BodyId, BodyOwner, Expr, ExprPath, Map, Pat, Stmt, Ty, TyPath, Variant, HIR,
    },
    parser::token::{Op, Punct},
    resolve::builtin::{TyBuiltin, ValueBuiltin},
    session::{impl_session_holder, Session},
    span::sym::{Ident, Kw},
    typeck::ty,
};

macro_rules! walk_block {
    ($self: ident, $nodes: expr, $visitor: ident, $hir: expr) => {{
        $self.pp.indent();
        $nodes.for_each(|node| {
            $self.pp.out_indent();
            $self.$visitor(node, $hir);
            $self.pp.nl();
        });
        $self.pp.dedent();
    }};
}

macro_rules! walk_each_delim {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr, $hir: expr) => {
        $nodes.enumerate().for_each(|(index, node)| {
            $self.$visitor(node, $hir);
            if index < $nodes.len() - 1 {
                $self.pp.str($sep);
            }
        })
    };

    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes.enumerate().for_each(|(index, node)| {
            $self.$visitor(node);
            if index < $nodes.len() - 1 {
                $self.pp.str($sep);
            }
        })
    };
}

pub(crate) use walk_each_delim;

pub struct HirPP<'a> {
    pub pp: AstLikePP<'a>,
}

impl_session_holder!(HirPP<'a>; pp.sess);

impl<'a> HirPP<'a> {
    pub fn new(sess: &'a Session, mode: AstPPMode) -> Self {
        Self {
            pp: AstLikePP::new(sess, mode),
        }
    }
}

impl<'a> HirVisitor for HirPP<'a> {
    fn visit_hir(&mut self, hir: &HIR) {
        self.pp.line("== HIR ==").item_id(ROOT_ITEM_ID).nl();
        walk_each_delim!(
            self,
            hir.root().items.iter().copied(),
            visit_item_stmt,
            "\n",
            hir
        )
    }

    fn visit_stmt(&mut self, stmt: Stmt, hir: &HIR) {
        let stmt = hir.stmt(stmt);

        match stmt.kind() {
            &StmtKind::Expr(expr) => self.visit_expr_stmt(expr, hir),
            &StmtKind::Item(item) => self.visit_item_stmt(item, hir),
            StmtKind::Local(local) => self.visit_local_stmt(local, hir),
        }
        self.pp.hir_id(stmt);
    }

    fn visit_local_stmt(&mut self, local: &Local, hir: &HIR) {
        self.pp.kw(Kw::Let);
        self.visit_pat(local.pat, hir);
        self.pp.op(Op::Assign);
        self.visit_expr(local.value, hir);
    }

    // Items //
    // fn visit_item(&mut self, id: &ItemId, hir: &HIR) {
    //     let id = *id;
    //     let item = hir.item(id);
    //     match item.kind() {
    //         ItemKind::TyAlias(ty) => self.visit_type_item(item.name(, hir), ty,
    // id, hir),         ItemKind::Mod(m) => self.visit_mod_item(item.name(,
    // hir), m, id, hir),         ItemKind::Value(value) =>
    // self.visit_value_item(item.name(), value, id, hir),
    //         ItemKind::Func(value) => self.visit_func_item(item.name(, hir),
    // value, id, hir),         ItemKind::ExternItem(extern_item) => {
    //             self.visit_extern_item(item.name(), extern_item, id, hir)
    //         },
    //     }
    // }

    fn visit_generic_params(&mut self, generics: &GenericParams, hir: &HIR) {
        if !generics.ty_params.is_empty() {
            self.pp.sp();
        }
        walk_each_delim!(self, generics.ty_params.iter(), visit_ty_param, " ", hir);
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, id: ItemId, hir: &HIR) {
        self.pp.kw(Kw::Type).item_id(id);
        self.visit_ident(name, hir);
        self.pp.ty_anno(id.hir_id()).op(Op::Assign);
        self.visit_ty(ty_item.ty, hir);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, item_id: ItemId, hir: &HIR) {
        self.pp.kw(Kw::Mod).item_id(item_id);
        self.visit_ident(name, hir);
        self.pp.nl();
        walk_block!(self, mod_item.items.iter().copied(), visit_item, hir);
    }

    fn visit_value_item(&mut self, name: Ident, value: BodyId, id: ItemId, hir: &HIR) {
        self.pp
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id())
            .op(Op::Assign);
        self.visit_body(value, BodyOwner::value(id.def_id()), hir);
    }

    fn visit_func_item(&mut self, name: Ident, body: BodyId, id: ItemId, hir: &HIR) {
        self.pp
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id())
            .sp();
        walk_each_delim!(
            self,
            hir.body(body).params.iter().copied(),
            visit_pat,
            " ",
            hir
        );
        self.pp.op(Op::Assign);
        self.visit_body(body, BodyOwner::func(id.def_id()), hir);
    }

    fn visit_adt_item(&mut self, name: Ident, data: &Adt, id: ItemId, hir: &HIR) {
        self.pp
            .kw(Kw::Data)
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id());
        self.pp.op(Op::Assign);
        walk_each_delim!(self, data.variants.iter(), visit_variant, " | ", hir);
    }

    fn visit_variant(&mut self, &variant: &Variant, hir: &HIR) {
        let variant = hir.variant(variant);
        self.pp
            .string(variant.name)
            .def_id(&variant.def_id)
            .ty_anno(variant.id);

        if self.pp.mode == AstPPMode::TyAnno {
            // FIXME: Can be optional?
            let ctor_ty = self.pp.sess.tyctx.def_ty(variant.ctor_def_id).unwrap();
            self.pp
                .string(format!("(constructor: {ctor_ty})").colorize(ty::TyId::color(), None));
        }

        self.pp.sp();
        walk_each_delim!(self, variant.fields.iter(), visit_field, " ", hir);
    }

    fn visit_field(&mut self, field: &Field, hir: &HIR) {
        field.name.as_ref().map(|name| {
            self.pp.string(name.original_string()).punct(Punct::Colon);
        });
        self.visit_ty(field.ty, hir);
        self.pp.ty_anno(field.id);

        if self.pp.mode == AstPPMode::TyAnno {
            if let Some(accessor_def_id) = field.accessor_def_id {
                let accessor_ty = self.pp.sess.tyctx.def_ty(accessor_def_id).unwrap();
                self.pp
                    .string(format!("(accessor: {accessor_ty})").colorize(ty::TyId::color(), None));
            }
        }
    }

    fn visit_extern_item(&mut self, name: Ident, extern_item: &ExternItem, id: ItemId, hir: &HIR) {
        self.pp
            .string(name)
            .item_id(id)
            .ty_anno(id.hir_id())
            .punct(Punct::Colon);
        self.visit_ty(extern_item.ty, hir);
    }

    fn visit_body(&mut self, body: BodyId, _owner: BodyOwner, hir: &HIR) {
        let body = hir.body(body);
        self.visit_expr(body.value, hir);
    }

    // Expressions //
    fn visit_expr(&mut self, expr_id: Expr, hir: &HIR) {
        let expr = hir.expr(expr_id);

        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit, hir),
            &ExprKind::Path(path) => self.visit_path_expr(path, hir),
            &ExprKind::Block(block) => self.visit_block_expr(block, hir),
            ExprKind::Call(call) => self.visit_call_expr(call, hir),
            &ExprKind::Let(block) => self.visit_let_expr(block, hir),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda, hir),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr, hir),
            // ExprKind::FieldAccess(lhs, field) => self.visit_field_access_expr(lhs, field, hir),
            ExprKind::Builtin(bt) => self.visit_builtin_expr(bt, hir),
            ExprKind::Match(subject, arms) => self.visit_match_expr(*subject, arms, hir),
        }
        self.pp.ty_anno(expr_id).hir_id(expr);
    }

    fn visit_lit_expr(&mut self, lit: &Lit, _hir: &HIR) {
        self.pp.string(lit);
    }

    fn visit_path_expr(&mut self, path: ExprPath, hir: &HIR) {
        let path = hir.expr_path(path);

        // TODO: Operator name in parentheses
        self.pp.string(path).hir_id(path);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        let body = hir.body(lambda.body_id);
        self.pp.punct(Punct::Backslash);
        walk_each_delim!(self, body.params.iter().copied(), visit_pat, " ", hir);
        self.pp.punct(Punct::Arrow);
        self.visit_expr(body.value, hir);
    }

    fn visit_call_expr(&mut self, call: &Call, hir: &HIR) {
        self.visit_expr(call.lhs, hir);
        self.pp.sp();
        walk_each_delim!(self, call.args.iter().copied(), visit_expr, " ", hir);
    }

    fn visit_let_expr(&mut self, block: Block, hir: &HIR) {
        self.pp.kw(Kw::Let);
        self.visit_block(block, hir);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr, hir: &HIR) {
        self.visit_expr(ty_expr.expr, hir);
        self.pp.punct(Punct::Colon);
        self.visit_ty(ty_expr.ty, hir);
    }

    fn visit_field_access_expr(&mut self, lhs: Expr, field: Ident, hir: &HIR) {
        self.visit_expr(lhs, hir);
        self.pp.punct(Punct::Dot);
        self.visit_ident(field, hir);
    }

    fn visit_builtin_expr(&mut self, bt: &ValueBuiltin, _hir: &HIR) {
        self.pp.string(bt);
    }

    fn visit_match_expr(&mut self, subject: Expr, arms: &[Arm], hir: &HIR) {
        self.pp.kw(Kw::Match);
        self.visit_expr(subject, hir);
        self.pp.nl();
        walk_block!(self, arms.iter(), visit_match_arm, hir);
    }

    fn visit_match_arm(&mut self, arm: &Arm, hir: &HIR) {
        self.visit_pat(arm.pat, hir);
        self.pp.punct(Punct::FatArrow);
        self.visit_expr(arm.body, hir);
    }

    // Types //
    fn visit_ty(&mut self, ty: Ty, hir: &HIR) {
        let ty = hir.ty(ty);

        match &ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path, hir),
            TyKind::Func(params, body) => self.visit_func_ty(params, *body, hir),
            TyKind::App(cons, arg) => self.visit_ty_app(*cons, arg, hir),
            TyKind::Builtin(bt) => self.visit_builtin_ty(bt, hir),
        }
        self.pp.hir_id(ty);
    }

    fn visit_ty_path(&mut self, path: &TyPath, hir: &HIR) {
        let path = hir.ty_path(*path);
        self.pp.string(path).hir_id(path);
    }

    fn visit_func_ty(&mut self, params: &[Ty], body: Ty, hir: &HIR) {
        walk_each_delim!(self, params.iter().copied(), visit_ty, " ", hir);
        self.pp.punct(Punct::Arrow);
        self.visit_ty(body, hir);
    }

    fn visit_ty_app(&mut self, cons: Ty, args: &[Ty], hir: &HIR) {
        self.visit_ty(cons, hir);
        self.pp.sp();
        walk_each_delim!(self, args.iter().copied(), visit_ty, " ", hir);
    }

    fn visit_builtin_ty(&mut self, bt: &TyBuiltin, _hir: &HIR) {
        self.pp.string(bt);
    }

    // Patterns //
    fn visit_pat(&mut self, pat_id: Pat, hir: &HIR) {
        let pat = hir.pat(pat_id);
        self.pp.hir_id(pat);

        match pat.kind() {
            PatKind::Unit => {
                self.pp.kw(Kw::Unit);
            },
            &PatKind::Ident(ident, name_id) => {
                self.visit_ident_pat(ident, name_id, hir);
            },
            PatKind::Struct(ty_path, fields, rest) => todo!(),
            &PatKind::Or(lpat, rpat) => {
                self.visit_pat(lpat, hir);
                self.pp.op(Op::BitOr);
                self.visit_pat(rpat, hir);
            },
        }

        self.pp.ty_anno(pat_id);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: Ident, _hir: &HIR) {
        self.pp.string(ident);
    }

    fn visit_block(&mut self, block: Block, hir: &HIR) {
        let block = hir.block(block);

        self.pp.nl();
        walk_block!(self, block.stmts().iter().copied(), visit_stmt, hir);
        self.pp.indent().out_indent();
        block.expr().map(|expr| self.visit_expr(expr, hir));
        self.pp.dedent().hir_id(block);
    }
}

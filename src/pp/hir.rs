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
        Block, BodyId, BodyOwner, Expr, ExprPath, Pat, Stmt, Ty, TyPath, Variant, HIR,
    },
    parser::token::{Op, Punct},
    resolve::builtin::{TyBuiltin, ValueBuiltin},
    session::{impl_session_holder, Session, SessionHolder},
    span::sym::{Ident, Kw},
    typeck::ty,
};

macro_rules! walk_block {
    ($self: ident, $nodes: expr, $visitor: ident) => {{
        $self.pp.indent();
        $nodes.iter().for_each(|node| {
            $self.pp.out_indent();
            $self.$visitor(node);
            $self.pp.nl();
        });
        $self.pp.dedent();
    }};
}

macro_rules! walk_each_delim {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes.iter().enumerate().for_each(|(index, node)| {
            $self.$visitor(node);
            if index < $nodes.len() - 1 {
                $self.pp.str($sep);
            }
        })
    };

    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes.iter().enumerate().for_each(|(index, node)| {
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
    fn visit_hir(&mut self) {
        self.pp.line("== HIR ==").item_id(ROOT_ITEM_ID).nl();
        walk_each_delim!(self, self.hir().root().items, visit_item_stmt, "\n")
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        let stmt = self.hir().stmt(*stmt);

        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
            StmtKind::Local(local) => self.visit_local_stmt(local),
        }
        self.pp.hir_id(stmt);
    }

    fn visit_local_stmt(&mut self, local: &Local) {
        self.visit_pat(&local.pat);
        self.pp.ty_anno(local.id).op(Op::Assign);
        self.visit_expr(&local.value);
    }

    // Items //
    // fn visit_item(&mut self, id: &ItemId) {
    //     let id = *id;
    //     let item = self.hir().item(id);
    //     match item.kind() {
    //         ItemKind::TyAlias(ty) => self.visit_type_item(item.name(), ty, id,
    // hir),         ItemKind::Mod(m) => self.visit_mod_item(item.name(), m, id,
    // hir),         ItemKind::Value(value) =>
    // self.visit_value_item(item.name(), value, id),
    //         ItemKind::Func(value) => self.visit_func_item(item.name(), value, id,
    // hir),         ItemKind::ExternItem(extern_item) => {
    //             self.visit_extern_item(item.name(), extern_item, id)
    //         },
    //     }
    // }

    fn visit_generic_params(&mut self, generics: &GenericParams) {
        if !generics.ty_params.is_empty() {
            self.pp.sp();
        }
        walk_each_delim!(self, &generics.ty_params, visit_ty_param, " ");
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, id: ItemId) {
        self.pp.kw(Kw::Type).item_id(id);
        self.visit_ident(&name);
        self.pp.ty_anno(id.hir_id()).op(Op::Assign);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, item_id: ItemId) {
        self.pp.kw(Kw::Mod).item_id(item_id);
        self.visit_ident(&name);
        self.pp.nl();
        walk_block!(self, mod_item.items, visit_item);
    }

    fn visit_value_item(&mut self, name: Ident, value: &BodyId, id: ItemId) {
        self.pp
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id())
            .op(Op::Assign);
        self.visit_body(&value, BodyOwner::value(id.def_id()));
    }

    fn visit_func_item(&mut self, name: Ident, body: &BodyId, id: ItemId) {
        self.pp
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id())
            .sp();
        walk_each_delim!(self, self.hir().body(*body).params, visit_pat, " ");
        self.pp.op(Op::Assign);
        self.visit_body(body, BodyOwner::func(id.def_id()));
    }

    fn visit_adt_item(&mut self, name: Ident, data: &Adt, id: ItemId) {
        self.pp
            .kw(Kw::Data)
            .string(name.original_string())
            .item_id(id)
            .ty_anno(id.hir_id());
        self.pp.op(Op::Assign);
        walk_each_delim!(self, data.variants, visit_variant, " | ");
    }

    fn visit_variant(&mut self, &variant: &Variant) {
        let variant = self.hir().variant(variant);
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
        walk_each_delim!(self, &variant.fields, visit_field, " ");
    }

    fn visit_field(&mut self, field: &Field) {
        field.name.as_ref().map(|name| {
            self.pp.string(name.original_string()).punct(Punct::Colon);
        });
        self.visit_ty(&field.ty);
        self.pp.ty_anno(field.id);

        if self.pp.mode == AstPPMode::TyAnno {
            if let Some(accessor_def_id) = field.accessor_def_id {
                let accessor_ty = self.pp.sess.tyctx.def_ty(accessor_def_id).unwrap();
                self.pp
                    .string(format!("(accessor: {accessor_ty})").colorize(ty::TyId::color(), None));
            }
        }
    }

    fn visit_extern_item(&mut self, name: Ident, extern_item: &ExternItem, id: ItemId) {
        self.pp
            .string(name)
            .item_id(id)
            .ty_anno(id.hir_id())
            .punct(Punct::Colon);
        self.visit_ty(&extern_item.ty);
    }

    fn visit_body(&mut self, &body: &BodyId, _owner: BodyOwner) {
        let body = self.hir().body(body);
        self.visit_expr(&body.value);
    }

    // Expressions //
    fn visit_expr(&mut self, expr_id: &Expr) {
        let expr_id = *expr_id;
        let expr = self.hir().expr(expr_id);

        match &expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Call(call) => self.visit_call_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
            // ExprKind::FieldAccess(lhs, field) => self.visit_field_access_expr(lhs, field),
            ExprKind::Builtin(bt) => self.visit_builtin_expr(bt),
            ExprKind::Match(subject, arms) => self.visit_match_expr(subject, arms),
        }
        self.pp.ty_anno(expr_id).hir_id(expr);
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {
        self.pp.string(lit);
    }

    fn visit_path_expr(&mut self, path: &ExprPath) {
        let path = self.hir().expr_path(*path);

        // TODO: Operator name in parentheses
        self.pp.string(path).hir_id(path);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        let body = self.hir().body(lambda.body_id);
        self.pp.punct(Punct::Backslash);
        walk_each_delim!(self, body.params, visit_pat, " ");
        self.pp.punct(Punct::Arrow);
        self.visit_expr(&body.value);
    }

    fn visit_call_expr(&mut self, call: &Call) {
        self.visit_expr(&call.lhs);
        self.pp.sp();
        walk_each_delim!(self, &call.args, visit_expr, " ");
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.pp.kw(Kw::Let);
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr) {
        self.visit_expr(&ty_expr.expr);
        self.pp.punct(Punct::Colon);
        self.visit_ty(&ty_expr.ty);
    }

    fn visit_field_access_expr(&mut self, lhs: &Expr, field: &Ident) {
        self.visit_expr(lhs);
        self.pp.punct(Punct::Dot);
        self.visit_ident(field);
    }

    fn visit_builtin_expr(&mut self, bt: &ValueBuiltin) {
        self.pp.string(bt);
    }

    fn visit_match_expr(&mut self, subject: &Expr, arms: &[Arm]) {
        self.pp.kw(Kw::Match);
        self.visit_expr(subject);
        self.pp.nl();
        walk_block!(self, &arms, visit_match_arm);
    }

    fn visit_match_arm(&mut self, arm: &Arm) {
        self.visit_pat(&arm.pat);
        self.pp.punct(Punct::FatArrow);
        self.visit_expr(&arm.body);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) {
        let ty = self.hir().ty(*ty);

        match &ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path),
            TyKind::Func(params, body) => self.visit_func_ty(params, body),
            TyKind::App(cons, arg) => self.visit_ty_app(cons, arg),
            TyKind::Builtin(bt) => self.visit_builtin_ty(bt),
        }
        self.pp.hir_id(ty);
    }

    fn visit_ty_path(&mut self, path: &TyPath) {
        let path = self.hir().ty_path(*path);
        self.pp.string(path).hir_id(path);
    }

    fn visit_func_ty(&mut self, params: &[Ty], body: &Ty) {
        walk_each_delim!(self, params, visit_ty, " ");
        self.pp.punct(Punct::Arrow);
        self.visit_ty(body);
    }

    fn visit_ty_app(&mut self, cons: &Ty, args: &[Ty]) {
        self.visit_ty(cons);
        self.pp.sp();
        walk_each_delim!(self, args, visit_ty, " ");
    }

    fn visit_builtin_ty(&mut self, bt: &TyBuiltin) {
        self.pp.string(bt);
    }

    // Patterns //
    fn visit_pat(&mut self, &pat_id: &Pat) {
        let pat = self.hir().pat(pat_id);
        self.pp.hir_id(pat);

        match pat.kind() {
            PatKind::Unit => {
                self.pp.kw(Kw::Unit);
            },
            PatKind::Ident(ident) => {
                self.visit_ident_pat(&ident);
            },
        }

        self.pp.ty_anno(pat_id);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) {
        self.pp.string(ident);
    }

    fn visit_block(&mut self, block: &Block) {
        let block = self.hir().block(*block);

        self.pp.nl();
        walk_block!(self, block.stmts(), visit_stmt);
        self.pp.indent().out_indent();
        block.expr().map(|expr| self.visit_expr(&expr));
        self.pp.dedent().hir_id(block);
    }
}

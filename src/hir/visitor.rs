use super::{
    expr::{Arm, Call, ExprKind, Lambda, Lit, TyExpr},
    item::{
        Adt, ExternItem, Field, GenericParams, ItemId, ItemKind, Mod, Struct, TyAlias, TyParam,
    },
    pat::{PatKind, StructPatField},
    stmt::{Local, StmtKind},
    ty::TyKind,
    Block, BodyId, BodyOwner, Expr, ExprPath, HirId, Map, Param, Pat, Stmt, Ty, TyPath, Variant,
    HIR,
};
use crate::{
    resolve::builtin::{TyBuiltin, ValueBuiltin},
    span::sym::Ident,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident, $hir: expr) => {
        $els.for_each(|el| {
            $self.$visitor(el, $hir);
        })
    };
}

pub(crate) use walk_each;

pub trait HirVisitor {
    fn visit_hir(&mut self, hir: &HIR) {
        walk_each!(self, hir.root().items.iter().copied(), visit_item, hir);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: Stmt, hir: &HIR) {
        let stmt = hir.stmt(stmt);
        match stmt.kind() {
            &StmtKind::Expr(expr) => self.visit_expr_stmt(expr, hir),
            &StmtKind::Item(item) => self.visit_item_stmt(item, hir),
            StmtKind::Local(local) => self.visit_local_stmt(local, hir),
        }
    }

    fn visit_expr_stmt(&mut self, expr: Expr, hir: &HIR) {
        self.visit_expr(expr, hir)
    }

    fn visit_item_stmt(&mut self, id: ItemId, hir: &HIR) {
        self.visit_item(id, hir)
    }

    fn visit_local_stmt(&mut self, local: &Local, hir: &HIR) {
        self.visit_pat(local.pat, hir);
        self.visit_expr(local.value, hir);
    }

    // Items //
    fn visit_item(&mut self, id: ItemId, hir: &HIR) {
        let item = hir.item(id);
        match item.kind() {
            ItemKind::TyAlias(ty) => self.visit_type_item(item.name(), ty, id, hir),
            ItemKind::Mod(m) => self.visit_mod_item(item.name(), m, id, hir),
            &ItemKind::Value(value) => self.visit_value_item(item.name(), value, id, hir),
            &ItemKind::Func(body) => self.visit_func_item(item.name(), body, id, hir),
            ItemKind::Adt(adt) => self.visit_adt_item(item.name(), adt, id, hir),
            ItemKind::Struct(data) => self.visit_struct_item(item.name(), data, id, hir),
            ItemKind::ExternItem(extern_item) => {
                self.visit_extern_item(item.name(), extern_item, id, hir)
            },
        }
    }

    fn visit_generic_params(&mut self, generics: &GenericParams, hir: &HIR) {
        walk_each!(self, generics.ty_params.iter(), visit_ty_param, hir);
    }

    fn visit_ty_param(&mut self, ty_param: &TyParam, hir: &HIR) {
        self.visit_ident(ty_param.name, hir)
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, _id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&ty_item.generics, hir);
        self.visit_ty(ty_item.ty, hir);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, _id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        walk_each!(self, mod_item.items.iter().copied(), visit_item, hir);
    }

    fn visit_value_item(&mut self, name: Ident, value: BodyId, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_body(value, BodyOwner::value(id.def_id()), hir);
    }

    fn visit_func_item(&mut self, name: Ident, body: BodyId, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_body(body, BodyOwner::func(id.def_id()), hir);
    }

    fn visit_adt_item(&mut self, name: Ident, adt: &Adt, _id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&adt.generics, hir);
        walk_each!(self, adt.variants.iter(), visit_variant, hir);
    }

    fn visit_variant(&mut self, &variant: &Variant, hir: &HIR) {
        let variant = hir.variant(variant);
        self.visit_ident(variant.name, hir);
        walk_each!(self, variant.fields.iter(), visit_field, hir);
    }

    fn visit_struct_item(&mut self, name: Ident, data: &Struct, _id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&data.generics, hir);
        walk_each!(self, data.fields.iter(), visit_field, hir);
    }

    fn visit_field(&mut self, field: &Field, hir: &HIR) {
        field.name.map(|name| self.visit_ident(name, hir));
        self.visit_ty(field.ty, hir);
    }

    fn visit_extern_item(&mut self, name: Ident, extern_item: &ExternItem, _id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_ty(extern_item.ty, hir);
    }

    fn visit_body(&mut self, body: BodyId, _owner: BodyOwner, hir: &HIR) {
        let body = hir.body(body);
        walk_each!(self, body.params.iter().copied(), visit_param, hir);
        self.visit_expr(body.value, hir);
    }

    fn visit_param(&mut self, param: Param, hir: &HIR) {
        self.visit_pat(hir.param(param).pat, hir);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: Pat, hir: &HIR) {
        let pat = hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => self.visit_unit_pat(hir),
            &PatKind::Ident(ident, name_id) => self.visit_ident_pat(ident, name_id, hir),
            PatKind::Struct(ty_path, fields, rest) => {
                self.visit_struct_pat(ty_path, fields, rest, hir)
            },
            PatKind::Or(lpat, rpat) => self.visit_or_pat(lpat, rpat, hir),
            PatKind::Tuple(pats) => self.visit_tuple_pat(pats, hir),
        }
    }

    fn visit_unit_pat(&mut self, _hir: &HIR) {}

    fn visit_ident_pat(&mut self, ident: Ident, _name_id: HirId, hir: &HIR) {
        self.visit_ident(ident, hir);
    }

    fn visit_struct_pat(
        &mut self,
        ty_path: &TyPath,
        fields: &[StructPatField],
        _rest: &bool,
        hir: &HIR,
    ) {
        self.visit_ty_path(ty_path, hir);
        walk_each!(self, fields.iter(), visit_struct_pat_field, hir);
    }

    fn visit_struct_pat_field(&mut self, field: &StructPatField, hir: &HIR) {
        if let Some(name) = field.name {
            self.visit_ident(name, hir);
        }

        self.visit_pat(field.pat, hir);
    }

    fn visit_or_pat(&mut self, &lpat: &Pat, &rpat: &Pat, hir: &HIR) {
        self.visit_pat(lpat, hir);
        self.visit_pat(rpat, hir);
    }
    fn visit_tuple_pat(&mut self, pats: &[Pat], hir: &HIR) {
        walk_each!(self, pats.iter().copied(), visit_pat, hir);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: Expr, hir: &HIR) {
        let expr = hir.expr(expr);
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit, hir),
            &ExprKind::Path(path) => self.visit_path_expr(path, hir),
            &ExprKind::Block(block) => self.visit_block_expr(block, hir),
            ExprKind::Call(call) => self.visit_call_expr(call, hir),
            ExprKind::Tuple(values) => self.visit_tuple_expr(values, hir),
            &ExprKind::Let(block) => self.visit_let_expr(block, hir),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda, hir),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr, hir),
            // ExprKind::FieldAccess(lhs, field) => self.visit_field_access_expr(lhs, field, hir),
            ExprKind::Builtin(bt) => self.visit_builtin_expr(bt, hir),
            ExprKind::Match(subject, arms) => self.visit_match_expr(*subject, arms, hir),
        }
    }

    fn visit_lit_expr(&mut self, _: &Lit, _hir: &HIR) {}

    fn visit_path_expr(&mut self, _path: ExprPath, _hir: &HIR) {}

    fn visit_block_expr(&mut self, block: Block, hir: &HIR) {
        self.visit_block(block, hir)
    }

    fn visit_call_expr(&mut self, call: &Call, hir: &HIR) {
        self.visit_expr(call.lhs, hir);
        walk_each!(self, call.args.iter().copied(), visit_expr, hir);
    }

    fn visit_tuple_expr(&mut self, values: &[Expr], hir: &HIR) {
        walk_each!(self, values.iter().copied(), visit_expr, hir);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        self.visit_body(lambda.body_id, BodyOwner::lambda(lambda.def_id), hir);
    }

    fn visit_let_expr(&mut self, block: Block, hir: &HIR) {
        self.visit_block(block, hir);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr, hir: &HIR) {
        self.visit_expr(ty_expr.expr, hir);
        self.visit_ty(ty_expr.ty, hir);
    }

    fn visit_field_access_expr(&mut self, lhs: Expr, field: Ident, hir: &HIR) {
        self.visit_expr(lhs, hir);
        self.visit_ident(field, hir);
    }

    fn visit_builtin_expr(&mut self, _bt: &ValueBuiltin, _hir: &HIR) {}

    fn visit_match_expr(&mut self, subject: Expr, arms: &[Arm], hir: &HIR) {
        self.visit_expr(subject, hir);
        walk_each!(self, arms.iter(), visit_match_arm, hir);
    }

    fn visit_match_arm(&mut self, arm: &Arm, hir: &HIR) {
        self.visit_pat(arm.pat, hir);
        self.visit_expr(arm.body, hir);
    }

    // Types //
    fn visit_ty(&mut self, ty: Ty, hir: &HIR) {
        let ty = hir.ty(ty);
        match &ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path, hir),
            TyKind::Func(params, body) => self.visit_func_ty(params, *body, hir),
            TyKind::App(cons, args) => self.visit_ty_app(*cons, args, hir),
            TyKind::Builtin(bt) => self.visit_builtin_ty(bt, hir),
            TyKind::Tuple(tys) => self.visit_tuple_ty(tys, hir),
        }
    }

    fn visit_ty_path(&mut self, _path: &TyPath, _hir: &HIR) {}

    fn visit_func_ty(&mut self, params: &[Ty], body: Ty, hir: &HIR) {
        walk_each!(self, params.iter().copied(), visit_ty, hir);
        self.visit_ty(body, hir);
    }

    fn visit_ty_app(&mut self, cons: Ty, args: &[Ty], hir: &HIR) {
        self.visit_ty(cons, hir);
        walk_each!(self, args.iter().copied(), visit_ty, hir);
    }

    fn visit_builtin_ty(&mut self, _bt: &TyBuiltin, _hir: &HIR) {}

    fn visit_tuple_ty(&mut self, tys: &[Ty], hir: &HIR) {
        walk_each!(self, tys.iter().copied(), visit_ty, hir);
    }

    // Fragments //
    fn visit_ident(&mut self, _: Ident, _hir: &HIR) {}
    fn visit_block(&mut self, block: Block, hir: &HIR) {
        let block = hir.block(block);
        walk_each!(self, block.stmts().iter().copied(), visit_stmt, hir);

        block.expr().map(|expr| self.visit_expr(expr, hir));
    }
}

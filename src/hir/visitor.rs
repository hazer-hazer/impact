use super::{
    expr::{Arm, Call, ExprKind, Lambda, Lit, TyExpr},
    item::{
        Adt, ExternItem, Field, GenericParams, ItemId, ItemKind, Mod, Struct, TyAlias, TyParam,
    },
    pat::PatKind,
    stmt::{Local, StmtKind},
    ty::TyKind,
    Block, BodyId, BodyOwner, Expr, ExprPath, Pat, Stmt, Ty, TyPath, Variant,
};
use crate::{
    resolve::builtin::{TyBuiltin, ValueBuiltin},
    session::SessionHolder,
    span::sym::Ident,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident) => {
        $els.iter().for_each(|el| {
            $self.$visitor(el);
        })
    };
}

pub(crate) use walk_each;

pub trait HirVisitor
where
    Self: SessionHolder + Sized,
{
    fn visit_hir(&mut self) {
        walk_each!(self, &self.hir().root().items, visit_item);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        let stmt = self.hir().stmt(*stmt);
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
            StmtKind::Local(local) => self.visit_local_stmt(local),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) {
        self.visit_expr(expr)
    }

    fn visit_item_stmt(&mut self, id: &ItemId) {
        self.visit_item(id)
    }

    fn visit_local_stmt(&mut self, local: &Local) {
        self.visit_pat(&local.pat);
        self.visit_expr(&local.value);
    }

    // Items //
    fn visit_item(&mut self, &id: &ItemId) {
        let item = self.hir().item(id);
        match item.kind() {
            ItemKind::TyAlias(ty) => self.visit_type_item(item.name(), ty, id),
            ItemKind::Mod(m) => self.visit_mod_item(item.name(), m, id),
            ItemKind::Value(value) => self.visit_value_item(item.name(), value, id),
            ItemKind::Func(body) => self.visit_func_item(item.name(), body, id),
            ItemKind::Adt(data) => self.visit_adt_item(item.name(), data, id),
            ItemKind::ExternItem(extern_item) => {
                self.visit_extern_item(item.name(), extern_item, id)
            },
        }
    }

    fn visit_generic_params(&mut self, generics: &GenericParams) {
        walk_each!(self, &generics.ty_params, visit_ty_param);
    }

    fn visit_ty_param(&mut self, ty_param: &TyParam) {
        self.visit_ident(&ty_param.name)
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, _id: ItemId) {
        self.visit_ident(&name);
        self.visit_generic_params(&ty_item.generics);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, _id: ItemId) {
        self.visit_ident(&name);
        walk_each!(self, mod_item.items, visit_item);
    }

    fn visit_value_item(&mut self, name: Ident, value: &BodyId, id: ItemId) {
        self.visit_ident(&name);
        self.visit_body(value, BodyOwner::value(id.def_id()));
    }

    fn visit_func_item(&mut self, name: Ident, body: &BodyId, id: ItemId) {
        self.visit_ident(&name);
        self.visit_body(body, BodyOwner::func(id.def_id()));
    }

    fn visit_adt_item(&mut self, name: Ident, adt: &Adt, _id: ItemId) {
        self.visit_ident(&name);
        self.visit_generic_params(&adt.generics);
        walk_each!(self, adt.variants, visit_variant);
    }

    fn visit_variant(&mut self, &variant: &Variant) {
        let variant = self.hir().variant(variant);
        self.visit_ident(&variant.name);
        walk_each!(self, variant.fields, visit_field);
    }

    fn visit_struct_item(&mut self, name: Ident, data: &Struct, _id: ItemId) {
        self.visit_ident(&name);
        self.visit_generic_params(&data.generics);
        walk_each!(self, data.fields, visit_field);
    }

    fn visit_field(&mut self, field: &Field) {
        field.name.as_ref().map(|name| self.visit_ident(name));
        self.visit_ty(&field.ty);
    }

    fn visit_extern_item(&mut self, name: Ident, extern_item: &ExternItem, _id: ItemId) {
        self.visit_ident(&name);
        self.visit_ty(&extern_item.ty);
    }

    fn visit_body(&mut self, &body: &BodyId, _owner: BodyOwner) {
        let body = self.hir().body(body);
        walk_each!(self, body.params, visit_pat);
        self.visit_expr(&body.value);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &Pat) {
        let pat = self.hir().pat(*pat);
        match pat.kind() {
            PatKind::Unit => self.visit_unit_pat(),
            PatKind::Ident(ident) => self.visit_ident_pat(&ident),
        }
    }

    fn visit_unit_pat(&mut self) {}

    fn visit_ident_pat(&mut self, ident: &Ident) {
        self.visit_ident(ident);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_lit_expr(&mut self, _: &Lit) {}

    fn visit_path_expr(&mut self, _path: &ExprPath) {}

    fn visit_block_expr(&mut self, block: &Block) {
        self.visit_block(block)
    }

    fn visit_call_expr(&mut self, call: &Call) {
        self.visit_expr(&call.lhs);
        walk_each!(self, call.args, visit_expr);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        self.visit_body(&lambda.body_id, BodyOwner::lambda(lambda.def_id));
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr) {
        self.visit_expr(&ty_expr.expr);
        self.visit_ty(&ty_expr.ty);
    }

    fn visit_field_access_expr(&mut self, lhs: &Expr, field: &Ident) {
        self.visit_expr(lhs);
        self.visit_ident(field);
    }

    fn visit_builtin_expr(&mut self, _bt: &ValueBuiltin) {}

    fn visit_match_expr(&mut self, subject: &Expr, arms: &[Arm]) {
        self.visit_expr(subject);
        walk_each!(self, arms, visit_match_arm);
    }

    fn visit_match_arm(&mut self, arm: &Arm) {
        self.visit_pat(&arm.pat);
        self.visit_expr(&arm.body);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) {
        let ty = self.hir().ty(*ty);
        match &ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path),
            TyKind::Func(params, body) => self.visit_func_ty(params, body),
            TyKind::App(cons, args) => self.visit_ty_app(cons, args),
            TyKind::Builtin(bt) => self.visit_builtin_ty(bt),
        }
    }

    fn visit_ty_path(&mut self, _path: &TyPath) {}

    fn visit_func_ty(&mut self, params: &[Ty], body: &Ty) {
        walk_each!(self, params, visit_ty);
        self.visit_ty(body);
    }

    fn visit_ty_app(&mut self, cons: &Ty, args: &[Ty]) {
        self.visit_ty(cons);
        walk_each!(self, args, visit_ty);
    }

    fn visit_builtin_ty(&mut self, _bt: &TyBuiltin) {}

    // Fragments //
    fn visit_ident(&mut self, _: &Ident) {}
    fn visit_block(&mut self, block: &Block) {
        let block = self.hir().block(*block);
        walk_each!(self, block.stmts(), visit_stmt);

        block.expr().map(|expr| self.visit_expr(&expr));
    }
}

pub fn walk_expr(visitor: &mut impl HirVisitor, &expr: &Expr) {
    let expr = visitor.hir().expr(expr);
    match &expr.kind() {
        ExprKind::Lit(lit) => visitor.visit_lit_expr(lit),
        ExprKind::Path(path) => visitor.visit_path_expr(path),
        ExprKind::Block(block) => visitor.visit_block_expr(block),
        ExprKind::Call(call) => visitor.visit_call_expr(call),
        ExprKind::Let(block) => visitor.visit_let_expr(block),
        ExprKind::Lambda(lambda) => visitor.visit_lambda(lambda),
        ExprKind::Ty(ty_expr) => visitor.visit_type_expr(ty_expr),
        // ExprKind::FieldAccess(lhs, field) => visitor.visit_field_access_expr(lhs, field),
        ExprKind::Builtin(bt) => visitor.visit_builtin_expr(bt),
        ExprKind::Match(subject, arms) => visitor.visit_match_expr(subject, arms),
    }
}

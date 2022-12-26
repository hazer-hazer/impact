use std::fmt::Display;

use crate::{
    ast::{
        self,
        expr::{Block, Expr, ExprKind, Lit, TyExpr},
        Path, WithNodeId,
    },
    parser::token,
    resolve::{
        def::DefKind,
        res::{NamePath, ResKind},
    },
    session::Session,
    span::span::{Ident, Kw, Symbol},
};

use super::ty::{FloatKind, IntKind, PrimTy, Ty, TyError, TyKind, TyResult};

#[derive(Clone)]
pub enum CtxItem {
    /// Type variable
    Var(Ident),
    /// Under context Ψ, variable χ has type Α
    TypedTerm(Ident, Ty),
    /// Possible solved (if type is Some) existential variable
    Existential(Ident, Option<Ty>),
    /// Type marker, in CETC it's ‣α^
    Marker(Ident),
}

pub enum CtxItemName {
    Var(Ident),
    TypedTerm(Ident),
    Existential(Ident),
    Marker(Ident),
}

impl Display for CtxItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CtxItem::Var(ident) => write!(f, "{}", ident),
            CtxItem::TypedTerm(ident, ty) => write!(f, "{}: {}", ident, ty),
            CtxItem::Existential(ident, solution) => match solution {
                Some(solved) => write!(f, "{}^ = {}", ident, solved),
                None => write!(f, "{}^", ident),
            },
            CtxItem::Marker(ident) => write!(f, ">{}", ident),
        }
    }
}

#[derive(Clone)]
pub struct Ctx<'ast> {
    items: Vec<CtxItem>,
    sess: &'ast Session,
}

impl<'a> Ctx<'a> {
    pub fn initial(sess: &'a Session) -> Self {
        Self {
            items: vec![],
            sess,
        }
    }

    // Context items //
    pub fn add(&mut self, item: CtxItem) -> &mut Self {
        self.items.push(item);
        self
    }

    pub fn add_many(&mut self, items: Vec<CtxItem>) -> &mut Self {
        self.items.extend(items);
        self
    }

    pub fn append(&mut self, ctx: Ctx) -> &mut Self {
        self.add_many(ctx.items)
    }

    pub fn pop(&mut self) -> Option<CtxItem> {
        self.items.pop()
    }

    pub fn get_item_index(&self, item: CtxItemName) -> Option<usize> {
        let index = self.items.iter().position(|it| match (&item, it) {
            (CtxItemName::Var(ident1), CtxItem::Var(ident2))
            | (CtxItemName::TypedTerm(ident1), CtxItem::TypedTerm(ident2, _))
            | (CtxItemName::Existential(ident1), CtxItem::Existential(ident2, _))
            | (CtxItemName::Marker(ident1), CtxItem::Marker(ident2))
                if ident1 == ident2 =>
            {
                true
            },
            _ => false,
        });

        index
    }

    pub fn contains(&self, item: CtxItemName) -> bool {
        self.get_item_index(item).is_some()
    }

    pub fn lookup(&self, item: CtxItemName) -> Option<&CtxItem> {
        match self.get_item_index(item) {
            Some(index) => self.items.get(index),
            None => None,
        }
    }

    pub fn split(&mut self, item: CtxItemName) -> Vec<CtxItem> {
        if let Some(index) = self.get_item_index(item) {
            self.items.drain(index..).collect()
        } else {
            vec![]
        }
    }

    pub fn replace(&mut self, item: CtxItemName, add_items: Vec<CtxItem>) -> &mut Self {
        let right = self.split(item);
        self.add_many(add_items);
        self.add_many(right);
        self
    }

    pub fn enter(&mut self, marker_name: Ident, items: Vec<CtxItem>) {
        self.add(CtxItem::Marker(marker_name));
        self.add_many(items);
    }

    pub fn leave(&mut self, marker_name: Ident) {
        self.split(CtxItemName::Marker(marker_name));
    }

    pub fn leave_unsolved(&mut self, item: CtxItemName) -> Vec<Ident> {
        let right = self.split(item);

        let mut names = vec![];

        for item in right {
            match item {
                CtxItem::Existential(ident, ty) if ty.is_none() => names.push(ident),
                _ => {},
            }
        }

        names
    }

    /// The context is complete if all existentials inside it are solved
    pub fn is_complete(&self) -> bool {
        for item in &self.items {
            match item {
                CtxItem::Existential(_, solution) if solution.is_none() => return false,
                _ => {},
            }
        }

        true
    }

    // Conversion //
    fn conv(&self, ty: ast::ty::Ty) -> Ty {
        match ty.kind() {
            ast::ty::TyKind::Unit => Ty::new(TyKind::Unit),
            ast::ty::TyKind::Path(path) => todo!(),
            ast::ty::TyKind::Func(_, _) => todo!(),
            ast::ty::TyKind::Paren(_) => todo!(),
        }
    }

    fn conv_path(&self, path: &ast::Path) -> Ty {
        let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
        match res.kind() {
            ResKind::Def(def_id) => {
                let def = self.sess.def_table.get_def(*def_id).unwrap();
                match def.kind() {
                    DefKind::Type => todo!(),
                    DefKind::Mod => todo!(),
                    DefKind::Func => todo!(),
                }
            },
            _ => unreachable!(),
        }
    }

    fn conv_int_kind(&self, kind: token::IntKind) -> IntKind {
        match kind {
            token::IntKind::Unknown => todo!(),
            token::IntKind::U8 => IntKind::U8,
            token::IntKind::U16 => IntKind::U16,
            token::IntKind::U32 => IntKind::U32,
            token::IntKind::U64 => IntKind::U64,
            token::IntKind::Uint => IntKind::Uint,
            token::IntKind::I8 => IntKind::I8,
            token::IntKind::I16 => IntKind::I16,
            token::IntKind::I32 => IntKind::I32,
            token::IntKind::I64 => IntKind::I64,
            token::IntKind::Int => IntKind::Int,
        }
    }

    fn conv_float_kind(&self, kind: token::FloatKind) -> FloatKind {
        match kind {
            token::FloatKind::Unknown => todo!(),
            token::FloatKind::F32 => FloatKind::F32,
            token::FloatKind::F64 => FloatKind::F64,
        }
    }

    // Types //
    pub fn ty_wf(&mut self, ty: &Ty) -> TyResult<()> {
        match ty.kind() {
            TyKind::Var(ident) => {
                if self.contains(CtxItemName::Var(*ident)) {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            TyKind::Existential(ident) => {
                if self.contains(CtxItemName::Existential(*ident)) {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            TyKind::Func(param_ty, return_ty) => {
                self.ty_wf(param_ty)?;
                self.ty_wf(return_ty)
            },
            TyKind::Forall(ident, ty) => {
                let marker_name = Ident::synthetic(Symbol::from_kw(Kw::M));
                self.enter(marker_name, vec![CtxItem::Var(*ident)]);
                self.ty_wf(&ty.open_forall(Ty::new(TyKind::Var(*ident))))?;
                self.leave(marker_name);
                Ok(())
            },
            _ => Err(TyError()),
        }
    }

    // Synthesis //
    pub fn synth(&self, expr: &Expr) -> TyResult<(Ty, Ctx)> {
        match expr.kind() {
            ExprKind::Unit => Ok((Ty::new(TyKind::Unit), self.clone())),
            ExprKind::Lit(lit) => {
                let lit_ty = match lit {
                    Lit::Bool(_) => PrimTy::Bool,
                    Lit::Int(_, kind) => PrimTy::Int(self.conv_int_kind(*kind)),
                    Lit::Float(_, kind) => PrimTy::Float(self.conv_float_kind(*kind)),
                    Lit::String(_) => PrimTy::String,
                };

                Ok((Ty::lit(lit_ty), self.clone()))
            },
            ExprKind::Path(path) => self.synth_path(path.0.as_ref().unwrap()),
            ExprKind::Block(_) => todo!(),
            ExprKind::Infix(_) => todo!(),
            ExprKind::Prefix(_) => todo!(),
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Call(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(_) => todo!(),
        }
    }

    fn synth_path(&self, path: &Path) -> TyResult<(Ty, Ctx)> {
        if let Some(item) = self.lookup(CtxItemName::TypedTerm(path.target_name())) {
            Ok((
                match item {
                    CtxItem::TypedTerm(_, ty) => ty.clone(),
                    _ => unreachable!(),
                },
                self.clone(),
            ))
        } else {
            Err(TyError())
        }
    }

    fn synth_ty_expr(&self, ty_expr: &TyExpr) -> TyResult<(Ty, Ctx)> {
        todo!()
    }

    fn synth_block(&self, _block: Block) -> TyResult<(Ty, Ctx)> {
        todo!()
    }
}

use super::{kind::Kind, ty::IntKind, Typecker};
use crate::{
    hir::{self, ty::TyPath, visitor::HirVisitor, HIR},
    message::message::MessageStorage,
    resolve::builtin::Builtin,
    session::Session,
    typeck::Ty,
};

struct TyConv<'hir> {
    hir: &'hir HIR,

    msg: MessageStorage,
    sess: Session,
}

impl<'hir> TyConv<'hir> {
    fn conv(&mut self, ty: hir::Ty) -> Ty {
        let ty_node = self.hir.ty(ty);

        match ty_node.kind() {
            hir::ty::TyKind::Path(TyPath(path)) => {
                todo!()
            },
            hir::ty::TyKind::Func(params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| self.conv(param))
                    .collect();
                let body = self.conv(*body);
                Ty::func(None, params, body)
            },
            hir::ty::TyKind::App(cons, args) => match self.hir.ty(*cons).kind() {
                hir::ty::TyKind::Builtin(bt) => match bt {
                    Builtin::RefTy => {
                        let mut args = args.iter().map(|&arg| self.conv(arg)).collect::<Vec<_>>();
                        assert_eq!(args.len(), 1);
                        return Ty::ref_to(args.remove(0));
                    },
                    _ => panic!(),
                },
                _ => todo!(),
            },
            hir::ty::TyKind::Builtin(bt) => match bt {
                Builtin::UnitTy => Ty::unit(),
                Builtin::I32 => Ty::int(IntKind::I32),
                Builtin::Str => Ty::str(),
                Builtin::RefTy => {
                    let kind_var = Kind::next_kind_var_id();
                    Ty::ty_kind(Kind::new_forall(
                        kind_var,
                        Kind::new_abs(
                            Kind::new_var(kind_var),
                            Kind::new_ty(Ty::ref_to(Ty::ty_kind(Kind::new_var(kind_var)))),
                        ),
                    ))
                },
                Builtin::RefCons | Builtin::AddInt | Builtin::SubInt | Builtin::UnitValue => {
                    unreachable!()
                },
            },
        }
    }
}

impl<'hir> HirVisitor for TyConv<'hir> {
    fn visit_ty(&mut self, &ty: &hir::Ty, hir: &HIR) {
        
    }
}

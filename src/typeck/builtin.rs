use std::collections::HashMap;

use crate::{
    resolve::builtin::Builtin,
    span::span::{Ident, Symbol},
};

use super::{
    ty::{IntKind, PrimTy, Ty},
    tyctx::TyCtx,
};

macro_rules! ty {
    ($tyctx: expr; $var: ident) => {
        $tyctx.var(Ident::synthetic(Symbol::intern(stringify!($var))))
    };

    ($tyctx: expr; ()) => {
        $tyctx.unit()
    };

    // ($tyctx: expr; $($param: tt)* -> $body: expr) => {{
    //     let param = ty!($tyctx; $($param)*);
    //     let body = ty!($tyctx; $($body)*);
    //     $tyctx.func(param, body)
    // }};

    ($tyctx: expr; $param: tt -> $($body: tt)*) => {{
        let param = ty!($tyctx; $param);
        let body = ty!($tyctx; $($body)*);
        $tyctx.func(param, body)
    }};

    ($tyctx: expr; forall $alpha: ident. $($ty: tt)*) => {{
        let ty = ty!($tyctx; $($ty)*);
        $tyctx.forall(Ident::synthetic(Symbol::intern(stringify!($alpha))), ty)
    }};

    ($tyctx: expr; ($($prior: tt)*)) => {
        ty!($tyctx; $($prior)*)
    };

    // ($tyctx: expr; $any: tt) => {
    //     $any
    // };
}

pub(super) fn builtins(tyctx: &mut TyCtx) -> HashMap<Builtin, Ty> {
    let builtins = HashMap::from([
        // Values //
        (Builtin::UnitValue, tyctx.unit()),
        // Operators //
        (
            Builtin::Add,
            ty!(
                tyctx;
                forall a. a -> a -> a
            ),
        ),
        (
            Builtin::Minus,
            ty!(
                tyctx;
                forall a. a -> a -> a
            ),
        ),
        // Types //
        (Builtin::UnitTy, tyctx.unit()),
        (Builtin::I32, tyctx.prim(PrimTy::Int(IntKind::I32))),
    ]);

    builtins
}

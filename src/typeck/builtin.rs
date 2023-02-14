use std::collections::HashMap;

use crate::{
    resolve::builtin::Builtin,
    span::span::{Ident, Symbol},
};

use super::ty::{IntKind, PrimTy, Ty};

macro_rules! ty {
    ($var: ident) => {
        Ty::var(Ident::synthetic(Symbol::intern(stringify!($var))))
    };

    (()) => {
        Ty::unit()
    };

    // ($tyctx: expr; $($param: tt)* -> $body: expr) => {{
    //     let param = ty!($tyctx; $($param)*);
    //     let body = ty!($tyctx; $($body)*);
    //     $tyctx.func(param, body)
    // }};

    ($param: tt -> $($body: tt)*) => {{
        Ty::func(ty!($param), ty!($($body)*))
    }};

    (forall $alpha: ident. $($ty: tt)*) => {{
        Ty::forall(Ident::synthetic(Symbol::intern(stringify!($alpha))), ty!($($ty)*))
    }};

    (($($prior: tt)*)) => {
        ty!($($prior)*)
    };

    // ($tyctx: expr; $any: tt) => {
    //     $any
    // };
}

pub(super) fn builtins() -> HashMap<Builtin, Ty> {
    let builtins = HashMap::from([
        // Values //
        (Builtin::UnitValue, Ty::unit()),
        // Operators //
        (Builtin::Add, ty!(forall a. a -> a -> a)),
        (Builtin::Minus, ty!(forall a. a -> a -> a)),
        // Types //
        (Builtin::UnitTy, Ty::unit()),
        (Builtin::I32, Ty::prim(PrimTy::Int(IntKind::I32))),
    ]);

    builtins
}

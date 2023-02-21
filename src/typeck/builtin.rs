use std::collections::HashMap;

use crate::resolve::builtin::Builtin;

use super::ty::{IntKind, PrimTy, Ty};

pub fn builtins() -> HashMap<Builtin, Ty> {
    let mut ty_vars = HashMap::new();

    macro_rules! ty {
        ($var: ident) => {
            Ty::var(ty_vars.get(stringify!($var)).copied().unwrap())
        };

        (()) => {
            Ty::unit()
        };

        // ($tyctx: expr$($param: tt)* -> $body: expr) => {{
        //     let param = ty!($tyctx$($param)*);
        //     let body = ty!($tyctx$($body)*);
        //     $tyctx.func(param, body)
        // }};

        (($($prior: tt)+):expr) => {
            ty!($($prior)+)
        };

        ($param: tt -> $($body: tt)+) => {{
            Ty::func(ty!($param), ty!($($body)+))
        }};

        (forall $alpha: ident. $($ty: tt)+) => {{
            if ty_vars.contains_key(stringify!($alpha)) {
                ty_vars.clear();
            }
            let ty_var = Ty::next_ty_var_id();
            assert!(ty_vars.insert(stringify!($alpha), ty_var).is_none());
            Ty::forall(ty_var, ty!($($ty)+))
        }};
    }

    let builtins = HashMap::from([
        // Values //
        (Builtin::UnitValue, ty!(())),
        // Operators //
        (Builtin::Add, ty!(forall a. a -> a -> a)),
        (Builtin::Minus, ty!(forall a. a -> a -> a)),
        // Types //
        (Builtin::UnitTy, ty!(())),
        (Builtin::I32, Ty::prim(PrimTy::Int(IntKind::I32))),
    ]);

    builtins
}

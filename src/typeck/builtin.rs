use std::collections::HashMap;

use crate::{resolve::builtin::Builtin, typeck::ty::TyVarId};

use super::ty::{IntKind, Ty};

pub fn builtins() -> HashMap<Builtin, Ty> {
    let _ty_vars = HashMap::<&str, TyVarId>::new();

    macro_rules! ty {
        (()) => {
            Ty::unit()
        };

        (i32) => {
            Ty::int(IntKind::I32)
        };

        ($var: ident) => {
            Ty::var(ty_vars.get(stringify!($var)).copied().unwrap())
        };

        (($($prior: tt)+):expr) => {
            ty!($($prior)+)
        };

        // ([$def_id: expr] $param: tt -> $($body: tt)+) => {{
        //     Ty::func(Some($def_id), ty!($param), ty!($($body)+))
        // }};

        ($($params: tt)-+ -> $($body: tt)+) => {{
            Ty::func(None, vec![$(ty!($params)),+], ty!($($body)+))
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
        // (
        //     Builtin::AddInt,
        //     ty!([sess.def_table.builtin(Builtin::AddInt)] i32 -> i32 -> i32),
        // ),
        // (
        //     Builtin::SubInt,
        //     ty!([sess.def_table.builtin(Builtin::SubInt)] i32 -> i32 -> i32),
        // ),
        (Builtin::AddInt, ty!(i32 - i32 -> i32)),
        (Builtin::SubInt, ty!(i32 - i32 -> i32)),
        // Types //
        (Builtin::UnitTy, ty!(())),
        (Builtin::I32, ty!(i32)),
    ]);

    builtins
}

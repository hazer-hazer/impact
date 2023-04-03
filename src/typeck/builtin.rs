use std::collections::HashMap;

use crate::{
    resolve::builtin::Builtin,
    typeck::{kind::Kind, kind::KindVarId, ty::TyVarId},
};

use super::ty::{IntKind, Ty};

pub fn builtins() -> HashMap<Builtin, Ty> {
    let mut ty_vars = HashMap::<&str, TyVarId>::new();
    let mut kind_vars = HashMap::<&str, KindVarId>::new();

    macro_rules! ty {
        (()) => {
            Ty::unit()
        };

        (i32) => {
            Ty::int(IntKind::I32)
        };

        (str) => {
            Ty::str()
        };

        (ref $($ty: tt)+) => {{
            Ty::ref_to(ty!($($ty)+))
        }};

        ($var: ident) => {
            Ty::var(ty_vars.get(stringify!($var)).copied().unwrap())
        };

        (($($prior: tt)+):expr) => {
            ty!($($prior)+)
        };

        // ([$def_id: expr] $param: tt -> $($body: tt)+) => {{
        //     Ty::func(Some($def_id), ty!($param), ty!($($body)+))
        // }};

        ($kind_var: lifetime -> $($ty: tt)+) => {{
            if kind_vars.contains_key(stringify!($kind_var)) {
                kind_vars.clear();
            }
            let kind_var = Kind::next_kind_var_id();
            assert!(kind_vars.insert(stringify!($kind_var), kind_var).is_none());
            Ty::ty_kind(Kind::new_forall(kind_var, Kind::new_abs(Kind::new_var(kind_var), Kind::new_ty(ty!($($ty)+)))))
        }};

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

        ($kind_var: lifetime) => {{
            Ty::ty_kind(Kind::new_var(kind_vars.get(stringify!($kind_var)).copied().unwrap()))
        }};

        // (forall $alpha: lifetime. $($kind: tt)+) => {{
        //     if kind_vars.contains_key(stringify!($alpha)) {
        //         kind_vars.clear();
        //     }
        //     let kind_var = Kind::next_kind_var_id();
        //     assert!(kind_vars.insert(stringify!($alpha), kind_var).is_none());
        //     Kind::new_forall(kind_var, kind!($($kind)+))
        // }};
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
        // FIXME: Must be a constructor?
        (Builtin::RefCons, ty!(forall r. r -> ref r)),
        // Types //
        (Builtin::UnitTy, ty!(())),
        (Builtin::I32, ty!(i32)),
        (Builtin::Str, ty!(str)),
        (Builtin::RefTy, ty!('a -> ref 'a)),
    ]);

    builtins
}

use std::collections::HashMap;

use super::ty::{IntKind, Ty};
use crate::{
    resolve::builtin::Builtin,
    typeck::{
        kind::{Kind, KindVarId},
        ty::TyVarId,
    },
};

/// Macro that handles simple type syntax used and must be used only for builtin
/// types declarations and in tests
macro_rules! ty {
    // Inner constructors //
    (@inner $ty_vars: expr, $kind_vars: expr;
        ()
    ) => {
        Ty::unit()
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        i32
    ) => {
        Ty::int(IntKind::I32)
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        str
    ) => {
        Ty::str()
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        ref $($ref_ty: tt)+
    ) => {
        Ty::ref_to(ty!(@inner $ty_vars, $kind_vars; $($ref_ty)+))
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        $var: ident
    ) => {
        Ty::var($ty_vars.get(stringify!($var)).copied().unwrap())
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        ($($paren: tt)+)
    ) => {
        ty!(@inner $ty_vars, $kind_vars; $($paren)+)
    };

    (@inner $ty_vars: expr, $kind_vars: expr;
        $($params: tt)-+ -> $($body: tt)+
    ) => {{
        Ty::func(None, vec![$(ty!(@inner $ty_vars, $kind_vars; $params)),+], ty!(@inner $ty_vars, $kind_vars; $($body)+))
    }};

    (@inner $ty_vars: expr, $kind_vars: expr;
        $kind_var: lifetime -> $($cons: tt)+
    ) => {{
        if $kind_vars.contains_key(stringify!($kind_var)) {
            $kind_vars.clear();
        }
        let kind_var = Kind::next_kind_var_id();
        assert!($kind_vars.insert(stringify!($kind_var), kind_var).is_none());
        Ty::ty_kind(Kind::new_forall(kind_var, Kind::new_abs(Kind::new_var(kind_var), Kind::new_ty(ty!(@inner $ty_vars, $kind_vars; $($cons)+)))))
    }};

    (@inner $ty_vars: expr, $kind_vars: expr;
        forall $alpha: ident. $($forall_body: tt)+
    ) => {{
        if $ty_vars.contains_key(stringify!($alpha)) {
            $ty_vars.clear();
        }
        let ty_var = Ty::next_ty_var_id();
        assert!($ty_vars.insert(stringify!($alpha), ty_var).is_none());
        Ty::forall(ty_var, ty!(@inner $ty_vars, $kind_vars; $($forall_body)+))
    }};

    (@inner $ty_vars: expr, $kind_vars: expr;
        $kind_var: lifetime
    ) => {{
        Ty::ty_kind(Kind::new_var($kind_vars.get(stringify!($kind_var)).copied().unwrap()))
    }};

    (@ctx $ty_vars: expr, $kind_vars: expr; $($ty: tt)+) => {
        ty!(@inner $ty_vars, $kind_vars; $($ty)+)
    };

    ($($ty: tt)+) => {
        ty!(@inner HashMap::<&str, TyVarId>::new(), HashMap::<&str, KindVarId>::new(); $($ty)+)
    };
}

pub(crate) use ty;

pub fn builtins() -> HashMap<Builtin, Ty> {
    let builtins = HashMap::from([
        // Values //
        (Builtin::UnitValue, ty!(())),
        // Operators //
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

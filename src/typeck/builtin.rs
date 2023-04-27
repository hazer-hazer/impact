use std::collections::HashMap;

use super::ty::{IntKind, Ty};
use crate::{
    cli::verbose,
    resolve::builtin::Builtin,
    typeck::{
        kind::{Kind, KindVarId},
        ty::TyVarId,
    },
};

#[derive(Default)]
pub struct TyMacroCtx<'a> {
    pub ty_vars: HashMap<&'a str, TyVarId>,
    pub kind_vars: HashMap<&'a str, KindVarId>,
}

impl<'a> TyMacroCtx<'a> {
    pub fn add_ty_var(&mut self, name: &'a str, var: TyVarId) {
        verbose!("Add ty var `{name}`");
        assert!(self.ty_vars.insert(name, var).is_none());
    }

    pub fn ty_var(&self, name: &str) -> TyVarId {
        self.ty_vars.get(name).copied().expect(&format!(
            "Type variable `{name}` not found in current ty! macro context. Available: [{}]",
            self.ty_vars
                .keys()
                .map(|name| format!("`{name}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }

    pub fn add_kind_var(&mut self, name: &'a str, var: KindVarId) {
        verbose!("Add kind var `'{name}`");
        assert!(self.kind_vars.insert(name, var).is_none());
    }

    pub fn kind_var(&self, name: &str) -> KindVarId {
        self.kind_vars.get(name).copied().expect(&format!(
            "Kind variable `'{name}` not found in current ty! macro context. Available: [{}]",
            self.kind_vars
                .keys()
                .map(|name| format!("`{name}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }
}

/// Macro that handles simple type syntax used and must be used only for builtin
/// types declarations and in tests
macro_rules! ty {
    // Inner constructors //
    (@inner $ctx: expr;
        ()
    ) => {
        Ty::unit()
    };

    (@inner $ctx: expr;
        i32
    ) => {
        Ty::int(IntKind::I32)
    };

    (@inner $ctx: expr;
        str
    ) => {
        Ty::str()
    };

    (@inner $ctx: expr;
        ref $($ref_ty: tt)+
    ) => {
        Ty::ref_to(ty!(@inner $ctx; $($ref_ty)+))
    };

    (@inner $ctx: expr;
        $var: ident
    ) => {
        Ty::var($ctx.ty_var(stringify!($var)))
    };

    (@inner $ctx: expr;
        ($($paren: tt)+)
    ) => {
        ty!(@inner $ctx; $($paren)+)
    };

    (@inner $ctx: expr;
        $kind_var: lifetime -> $($cons: tt)+
    ) => {{
        use crate::span::sym::{Internable, Ident};
        let kind_var = Kind::next_kind_var_id(Some(Ident::synthetic(stringify!($kind_var).intern())));
        $ctx.add_kind_var(stringify!($kind_var), kind_var);
        Ty::ty_kind(Kind::new_forall(kind_var, Kind::new_abs(Kind::new_var(kind_var), Kind::new_ty(ty!(@inner $ctx; $($cons)+)))))
    }};

    (@inner $ctx: expr;
        $($params: tt)-+ -> $($body: tt)+
    ) => {{
        Ty::func(None, vec![$(ty!(@inner $ctx; $params)),+], ty!(@inner $ctx; $($body)+))
    }};

    (@inner $ctx: expr;
        forall $alpha: ident. $($forall_body: tt)+
    ) => {{
        use crate::span::sym::{Internable, Ident};
        let ty_var = Ty::next_ty_var_id(Some(Ident::synthetic(stringify!($alpha).intern())));
        $ctx.add_ty_var(stringify!($alpha), ty_var);
        Ty::forall(ty_var, ty!(@inner $ctx; $($forall_body)+))
    }};

    (@inner $ctx: expr;
        $kind_var: lifetime
    ) => {{
        Ty::ty_kind(Kind::new_var($ctx.kind_var(stringify!($kind_var))))
    }};

    (@ctx $ctx: expr; $($ty: tt)+) => {
        ty!(@inner $ctx; $($ty)+)
    };

    // (@ctx $ctx: expr; ty_vars) => {{
    //     $ctx.ty_vars
    // }};

    // (@ctx $ctx: expr; kind_vars) => {{
    //     $ctx.kind_vars
    // }};

    ($($ty: tt)+) => {{
        #[allow(unused)]
        let mut ctx = TyMacroCtx::default();
        ty!(@inner ctx; $($ty)+)
    }};
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

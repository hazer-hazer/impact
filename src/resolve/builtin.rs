use std::fmt::Display;

use crate::span::span::{Ident, Internable, Symbol};

use super::def::{DefId, Namespace};

#[derive(Clone, Copy)]
pub struct DeclareBuiltin {
    def_id: DefId,
}

impl DeclareBuiltin {
    pub fn new(def_id: DefId) -> Self {
        Self { def_id }
    }

    pub fn def_id(&self) -> DefId {
        self.def_id
    }

    pub fn sym() -> Symbol {
        Symbol::intern("builtin")
    }

    pub fn ident() -> Ident {
        Ident::synthetic(Self::sym())
    }
}

macro_rules! builtin_table {
    ($($ns: ident $variant: ident $name: expr;)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
        pub enum Builtin {
            $($variant),*
        }

        impl Builtin {
            pub fn from_name(ns: Namespace, name: &str) -> Self {
                match (ns, name) {
                    $((Namespace::$ns, $name) => Self::$variant,)*
                    _ => panic!("Cannot declare builtin `{}` as {}", name, ns),
                }
            }

            pub fn from_sym(ns: Namespace, sym: Symbol) -> Self {
                Self::from_name(ns, sym.as_str())
            }

            pub fn name(&self) -> &str {
                match self {
                    $(Self::$variant => $name,)*
                }
            }

            pub fn sym(&self) -> Symbol {
                self.name().intern()
            }

            fn ns_of(builtin: Builtin) -> Namespace {
                match builtin {
                    $(Self::$variant => Namespace::$ns,)*
                }
            }

            pub fn ns(&self) -> Namespace {
                Self::ns_of(*self)
            }

            pub fn is_ty(&self) -> bool {
                self.ns() == Namespace::Type
            }

            pub fn is_value(&self) -> bool {
                self.ns() == Namespace::Value
            }

            pub fn each(mut f: impl FnMut(Builtin)) {
                $(f(Self::$variant));*
            }
        }
    };
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} `{}`", self.ns(), self.name())
    }
}

builtin_table! {
    // Operators //
    Value Add "+";
    Value Minus "-";
    Value UnitValue "()";

    // Primitive //
    Type UnitTy "()";
    Type I32 "i32";
}

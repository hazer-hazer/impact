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
    ($($ns: ident $name: ident;)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
        pub enum Builtin {
            $($name),*
        }

        impl Builtin {
            pub fn name(&self) -> &str {
                match self {
                    $(Self::$name => stringify!($name),)*
                }
            }

            pub fn sym(&self) -> Symbol {
                self.name().intern()
            }

            fn ns_of(builtin: Builtin) -> Namespace {
                match builtin {
                    $(Self::$name => Namespace::$ns,)*
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
                $(f(Self::$name));*
            }
        }

        impl TryFrom<&str> for Builtin {
            type Error = ();

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value {
                    $(stringify!($name) => Ok(Self::$name),)*
                    _ => Err(()),
                }
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
    Value AddInt;
    Value SubInt;

    // Values //
    Value UnitValue;

    // Primitive //
    Type UnitTy;
    Type I32;
}

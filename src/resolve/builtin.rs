use std::fmt::Display;

use crate::span::span::{Internable, Symbol};

use super::def::Namespace;

macro_rules! builtin_table {
    ($($ns: ident $variant: ident $name: expr;)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
        pub enum Builtin {
            $($variant),*
        }

        impl Builtin {
            pub fn from_name(name: &str) -> Self {
                match name {
                    $($name => Self::$variant,)*
                    _ => panic!(),
                }
            }

            pub fn from_sym(sym: Symbol) -> Self {
                Self::from_name(sym.as_str())
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
        write!(f, "{}", self.name())
    }
}

builtin_table! {
    // Operators //
    Value Add "+";
    Value Minus "-";

    // Primitive //
    Type Unit "()";
    Type I32 "i32";
}

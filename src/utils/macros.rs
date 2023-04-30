macro_rules! match_expected {
    ($expr: expr, $($patterns: pat => $arms: expr),+ $(;$($panic_args: tt)+)?) => {
        match $expr {
            $($patterns => $arms,)+
            _ => panic!($($($panic_args)+)?)
        }
    };
    ($expr: expr, $($patterns: pat => $arms: expr),+ $(,)?) => {
        match $expr {
            $($patterns => $arms,)+
            _ => panic!("Expected expression to match {} pattern", stringify!($($patterns),+))
        }
    };
    (@pp $expr: expr, $($patterns: pat => $arms: expr),+) => {
        match_expected!($expr, $($patterns => $arms),+; "Expected {} to match one of {{{}}}", $expr, stringify!($($patterns),+))
    };
}

pub(crate) use match_expected;

macro_rules! match_opt {
    ($expr: expr, $($patterns: pat => $arms: expr),+) => {
        match $expr {
            $($patterns => Some($arms),)+
            _ => None,
        }
    };
}

pub(crate) use match_opt;

macro_rules! concat_string {
    () => { String::with_capacity(0) };
    ($($s:expr),+) => {{
        use std::ops::AddAssign;
        let mut len = 0;
        $(len.add_assign(AsRef::<str>::as_ref(&$s).len());)+
        let mut buf = String::with_capacity(len);
        $(buf.push_str($s.as_ref());)+
        buf
    }};
}

pub(crate) use concat_string;

/// For given `subset` and `superset` enum variants impl `Into<superset> for
/// subset` and `TryFrom<superset> for subset`
macro_rules! sub_enum_conversion {
    ($subset_enum: ident <: $superset_enum: ident {$($same_variant_name: ident),+}) => {
        sub_enum_conversion!($subset_enum <: $superset_enum {$($same_variant_name <: $same_variant_name),+});
    };

    ($subset_enum: ident <: $superset_enum: ident {$($subset: ident <: $superset: ident),+}) => {
        impl Into<$superset_enum> for $subset_enum {
            fn into(self) -> $superset_enum {
                match self {
                    $(Self::$subset => $superset_enum::$superset),+
                }
            }
        }

        impl TryFrom<$superset_enum> for $subset_enum {
            type Error = ();

            fn try_from(superset: $superset_enum) -> Result<Self, Self::Error> {
                match superset {
                    $($superset_enum::$superset => Ok($subset_enum::$subset),)+
                    _ => Err(()),
                }
            }
        }
    };
}

pub(crate) use sub_enum_conversion;

// Impossible in stable Rust :(
// macro_rules! create_trait_with {
//     ($impl_macro_name: ident; $vis: vis trait $trait_name: ident {
//         fn $getter: ident (&self) -> $ty: ty;
//     }) => {
//         $vis trait $trait_name {
//             fn $getter(&self) -> $ty;
//         }

//         macro_rules! $impl_macro_name {
//             ($for_ty: ident $($gen: ident)?) => {
//                 impl<$($($gen),*)?> $trait_name for $for_ty<$($($gen),*)?> {
//                     fn $getter(&self) -> $ty {
//                         self.$getter
//                     }
//                 }
//             };
//         }

//         pub(crate) use $impl_macro_name;
//     };
// }

// pub(crate) use create_trait_with;

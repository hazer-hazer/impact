// pub trait Idx {
//     type Inner;

//     fn as_usize(&self) -> usize;
//     fn from_usize(value: usize) -> Self
//     where
//         Self::Inner: TryFrom<usize>;
// }

// impl Idx for usize {
//     type Inner = usize;

//     fn as_usize(&self) -> usize {
//         *self
//     }

//     fn from_usize(value: usize) -> Self
//     where
//         Self::Inner: TryFrom<usize>,
//     {
//         value
//     }
// }

// macro_rules! impl_uints {
//     ($($types: ty),*) => {
//         $(
//             impl Idx for $types {
//                 type Inner = $types;

//                 fn as_usize(&self) -> usize {
//                     usize::try_from(*self).unwrap()
//                 }

//                 fn from_usize(value: usize) -> Self
//                 where
//                     Self::Inner: TryFrom<usize>,
//                 {
//                     assert!(value <= <Self::Inner>::MAX as usize);
//                     value as Self::Inner
//                 }
//             }
//         )*
//     };
// }

// impl_uints!(u8, u16, u32, u64);

macro_rules! declare_idx {
    ($name: ident, u8, $format: expr, $color: expr) => {
        declare_idx!(uint $name, u8, $format, $color);
    };

    ($name: ident, u16, $format: expr, $color: expr) => {
        declare_idx!(uint $name, u16, $format, $color);
    };

    ($name: ident, u32, $format: expr, $color: expr) => {
        declare_idx!(uint $name, u32, $format, $color);
    };

    ($name: ident, u64, $format: expr, $color: expr) => {
        declare_idx!(uint $name, u64, $format, $color);
    };

    ($name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(new_type $name, $inner_ty, $format, $color);

        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                usize::from(id)
            }
        }

        impl From<$inner_ty> for $name {
            fn from(value: $inner_ty) -> Self {
                Self::new(<$inner_ty>::from(value))
            }
        }

        impl From<$name> for $inner_ty {
            fn from(value: $name) -> $inner_ty {
                value.0
            }
        }
    };

    (uint $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(new_type $name, $inner_ty, $format, $color);

        impl $name {
            pub const MIN: $inner_ty = <$inner_ty>::MIN;
            pub const MAX: $inner_ty = <$inner_ty>::MAX;
        }

        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                Self::from(id.0 as usize)
            }
        }

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                assert!(value <= <$inner_ty>::MAX as usize);
                Self::new(<$inner_ty>::try_from(value).unwrap())
            }
        }

        impl From<$name> for $inner_ty {
            fn from(value: $name) -> $inner_ty {
                value.0
            }
        }
    };

    (new_type $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(private $name, $inner_ty, $format, $color);
    };

    (private $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        #[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
        pub struct $name($inner_ty);

        impl $name {
            pub fn new(value: $inner_ty) -> Self {
                Self(value)
            }

            pub fn as_usize(&self) -> usize {
                usize::from(*self)
            }
        }

        // impl crate::dt::idx::Idx for $name {
        //     type Inner = $inner_ty;

        //     fn as_usize(&self) -> usize {
        //         usize::try_from(self.0).unwrap()
        //     }

        //     fn from_usize(value: usize) -> Self
        //     where
        //         Self::Inner: TryFrom<usize>,
        //     {
        //         Self(Self::Inner::from_usize(value))
        //     }
        // }

        impl PartialEq<$inner_ty> for $name {
            fn eq(&self, other: &$inner_ty) -> bool {
                self.0 == *other
            }
        }

        impl PartialOrd<$inner_ty> for $name {
            fn partial_cmp(&self, other: &$inner_ty) -> Option<std::cmp::Ordering> {
                Some(self.0.cmp(other))
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", format!($format, self.0.to_string()).fg_color($color))
            }
        }
    };
}

pub(crate) use declare_idx;

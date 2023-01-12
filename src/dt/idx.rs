use crate::cli::color::Colorize;

macro_rules! declare_idx {
    ($name: ident, u8, $tag: expr, $color: expr) => {
        declare_idx!(uint $name, u8, $tag, $color);
    };

    ($name: ident, u16, $tag: expr, $color: expr) => {
        declare_idx!(uint $name, u16, $tag, $color);
    };

    ($name: ident, u32, $tag: expr, $color: expr) => {
        declare_idx!(uint $name, u32, $tag, $color);
    };

    ($name: ident, u64, $tag: expr, $color: expr) => {
        declare_idx!(uint $name, u64, $tag, $color);
    };

    ($name: ident, $inner_ty: ty, $tag: expr, $color: expr) => {
        declare_idx!(private $name, $inner_ty, $tag, $color);

        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                Self::from(id.as_usize())
            }
        }
    };

    (uint $name: ident, $inner_ty: ty, $tag: expr, $color: expr) => {
        declare_idx!(private $name, $inner_ty, $tag, $color);

        impl $name {
            pub const MIN: $inner_ty = <$inner_ty>::MIN;
            pub const MAX: $inner_ty = <$inner_ty>::MAX;
        }

        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                assert!(id <= <$inner_ty>::MAX);
                Self::from(id.as_usize())
            }
        }
    };

    (private $name: ident, $inner_ty: ty, $tag: expr, $color: expr) => {
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

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                Self(<$inner_ty>::try_from(value).unwrap())
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $tag, self.0.to_string().fg_color($color))
            }
        }
    };
}

pub(crate) use declare_idx;

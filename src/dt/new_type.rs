macro_rules! new_type {
    ($name: ident $ty: ty) => {
        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
        pub struct $name($ty);

        impl $name {
            pub fn inner(&self) -> $ty {
                self.0
            }
        }

        impl Into<$ty> for $name {
            fn into(self) -> $ty {
                self.inner()
            }
        }

        impl From<$ty> for $name {
            fn from(val: $ty) -> Self {
                Self(val)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

pub(crate) use new_type;

macro_rules! bool_enum {
    ($name: ident) => {
        bool_enum! { $name Yes/No }
    };

    ($name: ident $yes: ident/$no: ident) => {
        #[derive(Clone, Copy, Debug)]
        pub enum $name {
            $yes,
            $no,
        }

        impl From<bool> for $name {
            fn from(val: bool) -> Self {
                if val {
                    Self::$yes
                } else {
                    Self::$no
                }
            }
        }

        impl Into<bool> for $name {
            fn into(self) -> bool {
                match self {
                    Self::$yes => true,
                    Self::$no => false,
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::$yes => stringify!($yes),
                    Self::$no => stringify!($no),
                }
                .fmt(f)
            }
        }
    };
}

pub(crate) use bool_enum;

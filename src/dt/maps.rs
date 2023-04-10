/// enum_str_map
macro_rules! enum_str_map {
    ($(#[$attrs: meta])? $vis: vis $name: ident { $($variant: ident: $mapping: literal $(= $discriminant: expr)?,)* }) => {
        $(#[$attrs])?
        $vis enum $name {
            $($variant $(= $discriminant)?,)*
        }

        impl $name {
            pub fn as_str(&self) -> &str {
                match self {
                    $(Self::$variant => $mapping,)*
                }
            }

            pub fn try_from_str(str: &str) -> Option<Self> {
                match str {
                    $($mapping => Some(Self::$variant),)*
                    _ => None,
                }
            }
        }

        impl<'a> TryFrom<&'a str> for $name {
            type Error = ();

            fn try_from(val: &'a str) -> Result<Self, Self::Error> {
                Self::try_from_str(val).ok_or(())
            }
        }

        impl<'a> Into<&'a str> for $name {
            fn into(self) -> &'a str {
                match self {
                    $(Self::$variant => $mapping,)*
                }
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }
    };
}

pub(crate) use enum_str_map;

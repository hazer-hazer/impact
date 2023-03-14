use std::{fmt::Display, num::NonZeroU8};

#[derive(Clone, Copy)]
pub struct Scalar {
    pub data: u64,

    /// Size in bytes
    pub size: NonZeroU8,
}

impl Scalar {
    pub fn new(data: u64, size: u8) -> Self {
        Self {
            data,
            size: NonZeroU8::new(size).unwrap(),
        }
    }
}

impl Display for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

macro_rules! from {
    ($($ty: ty),*) => {$(
        impl From<$ty> for Scalar {
            fn from(value: $ty) -> Self {
                Scalar {
                    data: value.into(),
                    size: NonZeroU8::new(std::mem::size_of::<$ty>() as u8).unwrap(),
                }
            }
        }
    )*};
}

from!(bool, u8, u16, u32, u64);

impl From<f32> for Scalar {
    fn from(value: f32) -> Self {
        Scalar {
            data: value.to_bits().into(),
            size: NonZeroU8::new(4).unwrap(),
        }
    }
}

impl From<f64> for Scalar {
    fn from(value: f64) -> Self {
        Scalar {
            data: value.to_bits().into(),
            size: NonZeroU8::new(8).unwrap(),
        }
    }
}

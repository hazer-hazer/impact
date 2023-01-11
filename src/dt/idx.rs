macro_rules! declare_idx {
    ($name: ident, $inner_ty: ty) => {
        #[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
        pub struct $name($inner_ty);

        impl $name {
            pub fn new(value: $inner_ty) -> Self {
                Self(value)
            }

            pub fn as_usize(&self) -> usize {
                self.value as usize
            }
        }

        impl From<usize> for $name {}
    };
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub struct Idx(u32);

impl Idx {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Into<usize> for Idx {
    fn into(self) -> usize {
        self.as_usize()
    }
}

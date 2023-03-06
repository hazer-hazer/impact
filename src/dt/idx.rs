#[derive(Debug, Clone)]
pub struct IndexVec<I: Idx, T> {
    vec: Vec<T>,
    _i: PhantomData<I>,
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self {
            vec: Vec::default(),
            _i: PhantomData::default(),
        }
    }
}

impl<I: Idx + From<usize>, T> FromIterator<T> for IndexVec<I, T> {
    fn from_iter<V: IntoIterator<Item = T>>(iter: V) -> Self {
        let mut index_vec = IndexVec::<I, T>::default();
        for v in iter {
            index_vec.push(v);
        }
        index_vec
    }
}

impl<I: Idx, T> FromIterator<(I, Option<T>)> for IndexVec<I, Option<T>> {
    fn from_iter<V: IntoIterator<Item = (I, Option<T>)>>(iter: V) -> Self {
        let mut index_vec = IndexVec::<I, Option<T>>::default();
        for (i, v) in iter {
            if let Some(v) = v {
                index_vec.insert(i, v);
            }
        }
        index_vec
    }
}

impl<I: Idx, T, const N: usize> From<[(I, Option<T>); N]> for IndexVec<I, Option<T>> {
    fn from(values: [(I, Option<T>); N]) -> Self {
        Self::from_iter(values)
    }
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn new_decent() -> Self
    where
        I: BoundedIdx,
        I::Inner: Into<usize>,
    {
        Self {
            vec: Vec::with_capacity(I::MAX.into()),
            _i: PhantomData::default(),
        }
    }

    pub fn new_of(count: usize) -> Self {
        Self {
            vec: Vec::with_capacity(count),
            _i: PhantomData::default(),
        }
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.vec.iter()
    }

    #[inline]
    pub fn iter_enumerated(
        &self,
    ) -> impl DoubleEndedIterator<Item = (I, &T)> + ExactSizeIterator + '_
    where
        I: From<usize>,
    {
        self.vec.iter().enumerate().map(|(i, v)| (I::from(i), v))
    }

    #[inline]
    pub fn get(&self, i: I) -> Option<&T> {
        self.vec.get(i.into())
    }

    #[inline]
    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        self.vec.get_mut(i.into())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    #[inline]
    pub fn push(&mut self, v: T) -> I
    where
        I: From<usize>,
    {
        let id = I::from(self.len());
        self.vec.push(v);
        id
    }

    pub fn resize_with_el(&mut self, i: I, fill: impl FnMut() -> T) {
        let min_len = i.into() + 1;
        if self.len() < min_len {
            self.vec.resize_with(min_len, fill)
        }
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    #[inline]
    fn index(&self, i: I) -> &Self::Output {
        &self.vec[i.into()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    #[inline]
    fn index_mut(&mut self, i: I) -> &mut Self::Output {
        &mut self.vec[i.into()]
    }
}

impl<I: Idx, T> IndexVec<I, Option<T>> {
    pub fn insert(&mut self, i: I, v: T) -> Option<T> {
        self.resize_with_el(i, || None);
        self[i].replace(v)
    }

    pub fn upsert_default(&mut self, i: I) -> &mut T
    where
        T: Default,
    {
        if !self.has(i) {
            self.insert(i, Default::default());
        }
        self.get_mut_unwrap(i)
    }

    #[inline]
    pub fn get_flat(&self, i: I) -> Option<&T> {
        if let Some(v) = self.get(i) {
            v.as_ref()
        } else {
            None
        }
    }

    #[inline]
    pub fn iter_enumerated_flat(&self) -> impl Iterator<Item = (I, &T)> + '_
    where
        I: From<usize>,
    {
        self.vec
            .iter()
            .enumerate()
            .filter(|(_, v)| v.is_some())
            .map(|(i, v)| (I::from(i), v.as_ref().unwrap()))
    }

    #[inline]
    pub fn get_mut_unwrap(&mut self, i: I) -> &mut T {
        let a: &mut Option<T> = self.get_mut(i).unwrap();
        a.as_mut().unwrap()
    }

    #[inline]
    pub fn get_mut_expect(&mut self, i: I, msg: &str) -> &mut T {
        let a: &mut Option<T> = self.get_mut(i).expect(msg);
        a.as_mut().expect(msg)
    }

    #[inline]
    pub fn get_unwrap(&self, i: I) -> &T {
        self.get(i).unwrap().as_ref().unwrap()
    }

    #[inline]
    pub fn get_expect(&self, i: I, msg: &str) -> &T {
        self.get(i).expect(msg).as_ref().expect(msg)
    }

    #[inline]
    pub fn has(&self, i: I) -> bool {
        self.len() > i.as_usize() && self.get(i).is_some()
    }
}

impl<I: Idx, T> IntoIterator for IndexVec<I, T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> vec::IntoIter<T> {
        self.vec.into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexVec<I, T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> slice::Iter<'a, T> {
        self.vec.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IndexVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> slice::IterMut<'a, T> {
        self.vec.iter_mut()
    }
}

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

pub trait Idx: Copy + Into<usize> {
    type Inner;

    fn new(inner: Self::Inner) -> Self;

    fn inner(&self) -> Self::Inner;

    fn as_usize(&self) -> usize;
}

pub trait BoundedIdx: Idx {
    const MIN: Self::Inner;
    const MAX: Self::Inner;
}

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

    ($name: ident, usize, $format: expr, $color: expr) => {
        declare_idx!(uint $name, u64, $format, $color);
    };

    ($name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(new_type $name, $inner_ty, $format, $color);

        impl From<$name> for usize {
            fn from(id: $name) -> Self {
                usize::from(id.0)
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
            pub fn inc(&mut self) -> &mut Self {
                self.0 += 1;
                self
            }

            pub fn next(&self) -> Self {
                Self::new(self.0 + 1)
            }
        }

        impl crate::dt::idx::BoundedIdx for $name {
            const MIN: $inner_ty = <$inner_ty>::MIN;
            const MAX: $inner_ty = <$inner_ty>::MAX;
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

    (wrapper $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(@inner $name, $inner_ty, $format, $color);

        impl From<$name> for $inner_ty {
            fn from(value: $name) -> $inner_ty {
                value.0
            }
        }
    };

    (new_type $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        declare_idx!(@inner $name, $inner_ty, $format, $color);

        impl crate::dt::idx::Idx for $name {
            type Inner = $inner_ty;

            fn new(inner: Self::Inner) -> Self {
                Self(inner)
            }

            fn as_usize(&self) -> usize {
                usize::from(*self)
            }

            fn inner(&self) -> $inner_ty {
                self.0
            }
        }
    };

    (@inner $name: ident, $inner_ty: ty, $format: expr, $color: expr) => {
        #[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
        pub struct $name($inner_ty);

        impl $name {
            pub const fn new(value: $inner_ty) -> Self {
                Self(value)
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

use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice, vec,
};

pub(crate) use declare_idx;

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, time::Instant};

    use crate::cli::color::{Color, Colorize};

    use super::*;

    declare_idx!(BenchIdx, u32, "BenchIdx#{}", Color::Black);

    #[test]
    fn vs_hashmap_inserts() {
        const MAX: <BenchIdx as Idx>::Inner = 10000;

        let mut index_vec = IndexVec::<BenchIdx, u32>::default();
        let mut hashmap = HashMap::<BenchIdx, u32>::default();

        let index_vec_time = Instant::now();
        for i in BenchIdx::MIN..MAX {
            index_vec.push(i as u32);
        }
        let index_vec_time = index_vec_time.elapsed();

        let hashmap_time = Instant::now();
        for i in BenchIdx::MIN..MAX {
            hashmap.insert(BenchIdx::new(i), i as u32);
        }
        let hashmap_time = hashmap_time.elapsed();

        assert!(index_vec_time < hashmap_time);
        println!(
            "IndexVec /vs/ HashMap descent full inserts: {}ns /vs/ {}ns",
            index_vec_time.as_nanos(),
            hashmap_time.as_nanos()
        );
    }
}

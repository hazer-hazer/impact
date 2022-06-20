use std::cell::{RefCell, RefMut};

#[derive(Debug)]
pub struct Lock<T>(RefCell<T>);

impl<T> Lock<T> {
    pub fn new(inner: T) -> Self {
        Lock(RefCell::new(inner))
    }

    pub fn lock(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }
}

impl<T: Default> Default for Lock<T> {
    fn default() -> Self {
        Lock::new(T::default())
    }
}

#[macro_export]
macro_rules! declare_arena {
    ($($name: ident: $ty: ty,)*) => {
        pub mod arena {
            pub struct TypedArena<T> {
                items: Vec<T>,
            }

            impl<T> TypedArena<T> {
                fn new() -> Self {
                    Self {
                        items: Vec::new(),
                    }
                }

                pub fn alloc(&mut self, value: T) -> &mut T {
                    self.items.push(value);
                    self.items.last_mut().unwrap()
                }
            }

            pub struct Arena<'ar> {
                $($name: TypedArena<$ty>,)*
            }

            impl<'ar> Arena<'ar> {
                pub fn new() -> Self {
                    Self {
                        $($name: TypedArena::new(),)*
                    }
                }

                pub fn alloc<T: Allocatable<'ar>>(&mut self, value: T) -> &'ar mut T {
                    value.alloc_on(self)
                }
            }

            pub trait Allocatable<'ar>: Sized {
                fn alloc_on<'a>(self, arena: &'a mut Arena<'ar>) -> &'a mut Self;
            }

            $(
                impl<'ar> Allocatable<'ar> for $ty {
                    fn alloc_on<'a>(self, arena: &'a mut Arena<'ar>) -> &'a mut Self {
                        arena.$name.alloc(self)
                    }
                }
            )*
        }
    };
}

pub(crate) use declare_arena;

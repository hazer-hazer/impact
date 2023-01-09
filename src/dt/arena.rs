#[macro_export]
macro_rules! declare_arena {
    ($($name: ident: $ty: ty,)*) => {
        pub mod arena {
            pub struct TypedArena<T> {
                items: Vec<T>,
            }

            impl<T> TypedArena<T> {
                fn alloc(&mut self, value: T) -> &mut T {
                    self.items.push(value);
                    self.items.last_mut().unwrap()
                }
            }

            #[derive(Default)]
            pub struct Arena<'a> {
                $($name: TypedArena<$ty>),*
            }

            impl<'a> Arena<'a> {
                pub fn alloc<T: Allocatable<'a>>(&mut self, value: T) -> &mut T {
                    value.alloc_on(self)
                }
            }

            pub trait Allocatable<'a>: Sized + 'a {
                fn alloc_on(self, arena: &mut Arena) -> &'a mut Self;
            }

            $(
                impl<'a> Allocatable<'a> for $ty {
                    fn alloc_on(self, arena: &mut Arena) -> &'a mut Self {
                        arena.$name.alloc(self)
                    }
                }
            )*
        }
    };
}

pub(crate) use declare_arena;

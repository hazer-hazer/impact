use crate::{ast::AST, hir::arena::Arena};

pub struct GlobalCtx<'ctx> {
    pub ast: &'ctx AST,
    pub hir_arena: Arena<'ctx>,
}

impl<'ctx> GlobalCtx<'ctx> {
    pub fn new(ast: &'ctx AST) -> Self {
        Self {
            ast,
            hir_arena: Arena::new(),
        }
    }
}

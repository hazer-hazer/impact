use std::collections::HashMap;

use crate::{ast::NodeId, span::span::Symbol};

use super::def::ModuleId;

struct Rib {
    locals: HashMap<Symbol, NodeId>,
    bound_module: Option<ModuleId>,
}

impl Rib {
    pub fn new(bound_module: Option<ModuleId>) -> Self {
        Self {
            locals: Default::default(),
            bound_module,
        }
    }
}

pub struct NameResolver {
    
}

use std::collections::HashMap;

use crate::{
    ast::{
        self,
        visitor::{visit_each_pr, visit_pr, AstVisitor},
    },
    span::span::Symbol,
};

use super::def::{DefId, PerNS};

enum ModuleKind {
    File,
    Mod,
}

struct Module {
    kind: ModuleKind,
    per_ns: PerNS<HashMap<Symbol, DefId>>,
}

impl Module {
    fn new(kind: ModuleKind) -> Self {
        Self {
            kind,
            per_ns: Default::default(),
        }
    }
}

struct Scope {
    parent: Option<Box<Scope>>,
    bound_module: Option<Module>,
}

impl Scope {
    fn new(parent: Option<Box<Scope>>, bound_module: Option<Module>) -> Self {
        Self {
            parent,
            bound_module,
        }
    }
}

pub struct DefCollector {
    last_def_index: u32,
}

impl DefCollector {
    fn next_def_id(&mut self) -> DefId {
        let def_id = DefId(self.last_def_index);
        self.last_def_index += 1;
        def_id
    }
}

impl AstVisitor<()> for DefCollector {
    fn visit_err(&self, _: &ast::ErrorNode) -> () {}

    fn visit_ast(&mut self, ast: &ast::AST) -> () {
        visit_each_pr!(self, ast.stmts(), visit_stmt)
    }

    
}

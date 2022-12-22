use crate::{
    ast::{
        expr::Block,
        item::{Item, ItemKind},
        visitor::{walk_each_pr, AstVisitor},
        ErrorNode, NodeId, Path, WithNodeId, AST,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Span},
};

use super::{
    def::{DefId, DefKind, DefMap, ModuleId, Namespace, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{NamePath, Res},
};

// #[derive(Debug)]
// enum ScopeKind {
//     Module(ModuleId),
// }

// #[derive(Debug)]
// struct Scope {
//     module_id: ModuleId,
// }

// impl Scope {
//     pub fn new_module(module_id: ModuleId) -> Self {
//         Self { module_id }
//     }
// }

pub struct NameResolver<'a> {
    ast: &'a AST,
    scopes: Vec<ModuleId>,
    nearest_mod: ModuleId, // Nearest `mod` item
    locals_spans: DefMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl<'a> NameResolver<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            ast,
            scopes: Default::default(),
            nearest_mod: ModuleId::Module(ROOT_DEF_ID),
            locals_spans: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn define_var(&mut self, node_id: NodeId, ident: &Ident) -> DefId {
        verbose!("Define var {} {}", node_id, ident);

        let def_id = self.sess.def_table.define(node_id, DefKind::Var, ident);
        let old_def = self.sess.def_table.get_module_mut(self.scope()).define(
            Namespace::Value,
            ident.sym(),
            def_id,
        );

        if let Some(old_def) = old_def {
            let old_def = self.sess.def_table.get_def(old_def).unwrap();
            MessageBuilder::error()
                .span(ident.span())
                .text(format!(
                    "Duplicate local variable `{}` definition",
                    ident.sym()
                ))
                .label(old_def.name().span(), "Previously defined here".to_string())
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        }

        def_id
    }

    fn enter_scope(&mut self, module_id: ModuleId) {
        verbose!("Enter scope {}", module_id);

        self.nearest_mod = module_id;

        self.scopes.push(module_id);
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn scope(&self) -> ModuleId {
        *self.scopes.last().unwrap()
    }

    fn resolve_path(&mut self, path: &Path) -> Res {
        let segments = path.segments();

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod);

        for seg_index in 0..segments.len() {
            let seg = segments[seg_index];
            let seg_name = seg.sym();
            let is_target = seg_index == segments.len() - 1;

            // Path prefix segments must be type identifiers
            if !is_target && !seg.is_ty() {
                MessageBuilder::error()
                    .span(path.prefix_span(seg_index))
                    .text(format!("Invalid path `{}`", path))
                    .label(
                        seg.span(),
                        format!("{} must be a type or module name", seg_name),
                    )
                    .emit(self);
                return Res::error();
            }

            let def = match search_mod.get_by_ident(&seg) {
                Some(def) => def,
                None => {
                    println!("Failed to resolve {};", path);
                    println!("{}", self.sess.def_table.defs().iter().map(|));

                    let prefix = path.prefix_str(seg_index);
                    MessageBuilder::error()
                        .span(path.prefix_span(seg_index))
                        .text(format!("Cannot find `{}` in {}", seg_name, prefix))
                        .label(
                            seg.span(),
                            format!("`{}` is not defined in {}", seg_name, prefix),
                        )
                        .emit(self);
                    return Res::error();
                },
            };

            if is_target {
                return Res::def(*def);
            } else {
                search_mod = self.sess.def_table.get_module(ModuleId::Module(*def))
            }
        }

        unreachable!()
    }
}

impl<'a> MessageHolder for NameResolver<'a> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'a> AstVisitor for NameResolver<'a> {
    fn visit_err(&mut self, _: &ErrorNode) {}

    // visit_let: We don't have locals for now

    fn visit_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Mod(name, items) => {
                let module_id =
                    ModuleId::Module(self.sess.def_table.get_def_id(item.id()).unwrap());
                self.nearest_mod = module_id;
                self.enter_scope(module_id);
                self.visit_mod_item(name, items, item.id());
                self.exit_scope();
            },
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id());

                if params.is_empty() {
                    self.define_var(item.id(), item.name().unwrap());
                }
            },
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.enter_scope(ModuleId::Block(block.id()));
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_scope();
    }

    fn visit_path(&mut self, path: &Path) {
        let res = self.resolve_path(path);

        verbose!("Resolved path `{}` as {}", path, res);

        self.sess.res.set(NamePath::new(path.id()), res);
    }
}

impl<'a> Stage<()> for NameResolver<'a> {
    fn run(mut self) -> StageOutput<()> {
        self.enter_scope(ROOT_MODULE_ID);
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}

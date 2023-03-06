use crate::{
    ast::{
        expr::Block,
        item::{Item, ItemKind},
        visitor::{walk_each_pr, AstVisitor},
        ErrorNode, NodeId, Path, WithNodeId, AST, DUMMY_NODE_ID, PR,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{builtin::DeclareBuiltin, def::Namespace},
    session::{Session, SourceId, Stage, StageOutput},
    span::span::{Ident, Internable},
};

use super::def::{DefId, DefKind, Module, ModuleId, ModuleKind, ROOT_DEF_ID};

pub struct DefCollector<'ast> {
    sess: Session,
    ast: &'ast AST,

    declare_builtin_func_mod: Option<ModuleId>,

    current_module: ModuleId,
    msg: MessageStorage,
}

impl<'ast> MessageHolder for DefCollector<'ast> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'ast> DefCollector<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            sess,
            ast,
            declare_builtin_func_mod: None,
            current_module: ModuleId::Module(ROOT_DEF_ID),
            msg: Default::default(),
        }
    }

    fn module(&mut self) -> &mut Module {
        self.sess.def_table.get_module_mut(self.current_module)
    }

    fn enter_def_module(&mut self, def_id: DefId) {
        verbose!("Enter def module {}", def_id);
        self.current_module = self.sess.def_table.add_module(def_id, self.current_module);
    }

    fn enter_block_module(&mut self, node_id: NodeId) {
        verbose!("Enter block module {}", node_id);
        self.current_module = self.sess.def_table.add_block(node_id, self.current_module)
    }

    fn exit_module(&mut self) {
        verbose!("Exit module");
        self.current_module = self
            .sess
            .def_table
            .get_module(self.current_module)
            .parent()
            .expect("Tried to exit root module");
    }

    fn define(&mut self, node_id: NodeId, kind: DefKind, ident: &Ident) -> DefId {
        let def_id = self.sess.def_table.define(node_id, kind, ident);
        let old_def = self.module().define(kind.namespace(), ident.sym(), def_id);

        verbose!("Define {} {} {} - {}", node_id, kind, ident, def_id);

        if let Some(old_def) = old_def {
            let old_def = self.sess.def_table.get_def(old_def).unwrap();
            MessageBuilder::error()
                .span(ident.span())
                .text(format!("Tried to redefine `{}`", ident.sym()))
                .label(old_def.name().span(), "Previously defined here".to_string())
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        }

        def_id
    }

    // Builtins //
    fn define_declare_builtin_func(&mut self) {
        let def_id = self.sess.def_table.define(
            DUMMY_NODE_ID,
            DefKind::DeclareBuiltin,
            &DeclareBuiltin::ident(),
        );

        // `builtin` is defined in both `Value` and `Type` namespaces
        assert!(self
            .module()
            .define(Namespace::Value, DeclareBuiltin::sym(), def_id)
            .is_none());

        assert!(self
            .module()
            .define(Namespace::Type, DeclareBuiltin::sym(), def_id)
            .is_none());

        self.declare_builtin_func_mod = Some(self.current_module);

        self.sess
            .def_table
            .set_declare_builtin(DeclareBuiltin::new(def_id));
    }

    fn check_path_is_declare_builtin(path: &PR<Path>) -> bool {
        if path.as_ref().unwrap().original_str().intern() == DeclareBuiltin::sym() {
            true
        } else {
            false
        }
    }

    // /// Checks if expression is `builtin` path
    // fn check_expr_for_builtin(&mut self, expr: &Expr) -> bool {
    //     if !self.in_declare_builtin_func_scope {
    //         return false;
    //     }

    //     match expr.kind() {
    //         ExprKind::Path(PathExpr(path)) => Self::check_path_is_declare_builtin(path),
    //         _ => false,
    //     }
    // }

    // fn check_ty_for_builtin(&mut self, ty: &Ty) -> bool {
    //     if !self.in_declare_builtin_func_scope {
    //         return false;
    //     }

    //     match ty.kind() {
    //         TyKind::Path(TyPath(path)) => Self::check_path_is_declare_builtin(path),
    //         _ => false,
    //     }
    // }
}

impl<'ast> AstVisitor<'ast> for DefCollector<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {}

    fn visit_item(&mut self, item: &'ast Item) {
        // Do not collect locals, they are defined and resolved in NameResolver.
        if let ModuleKind::Block(_) = self.module().kind() {
            match item.kind() {
                ItemKind::Decl(name, params, body) if params.is_empty() => {
                    self.visit_decl_item(name, params, body, item.id());
                    return;
                },
                _ => {},
            }
        }

        let def_id = self.define(
            item.id(),
            DefKind::from_item_kind(item.kind()),
            item.name()
                .expect("Cannot define unnamed item. TODO: Synthesize unnamed item name"),
        );

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.enter_def_module(def_id);
                self.visit_mod_item(name, items, item.id());
                self.exit_module();
            },
            ItemKind::Type(_, _) => {},
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
        }
    }

    fn visit_block(&mut self, block: &'ast Block) -> () {
        self.enter_block_module(block.id());
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_module();
    }
}

impl<'ast> Stage<()> for DefCollector<'ast> {
    fn run(mut self) -> StageOutput<()> {
        // FIXME: SourceId(0) is root file?
        assert_eq!(
            self.sess
                .def_table
                .add_root_module(SourceId::new(0))
                .as_module(),
            ROOT_DEF_ID
        );

        self.define_declare_builtin_func();

        self.visit_ast(self.ast);

        StageOutput::new(self.sess, (), self.msg)
    }
}

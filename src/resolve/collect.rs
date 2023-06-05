use super::def::{DefId, DefKind, Module, ModuleId, ModuleKind, ROOT_DEF_ID};
use crate::{
    ast::{
        expr::{Arm, Block, Expr, ExprKind},
        item::{ExternItem, Field, Item, ItemKind, TyParam, Variant},
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, NodeId, Path, WithNodeId, AST, DUMMY_NODE_ID, N, PR,
    },
    cli::verbose,
    message::message::{impl_message_holder, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{builtin::DeclareBuiltin, def::Namespace},
    session::{impl_session_holder, stage_result, Session, Stage, StageResult},
    span::{
        source::SourceId,
        sym::{Ident, Internable},
    },
};

pub struct DefCollector<'ast> {
    ast: &'ast AST,

    current_module: ModuleId,

    sess: Session,
    msg: MessageStorage,
}

impl_message_holder!(DefCollector<'ast>);
impl_session_holder!(DefCollector<'ast>);

impl<'ast> DefCollector<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            sess,
            ast,
            current_module: ModuleId::Def(ROOT_DEF_ID),
            msg: Default::default(),
        }
    }

    fn module(&mut self) -> &mut Module {
        self.sess.def_table.get_module_mut(self.current_module)
    }

    fn enter_def_module(&mut self, def_id: DefId) {
        verbose!("Enter def module {def_id}");
        self.current_module = self.sess.def_table.add_module(def_id, self.current_module);
    }

    fn enter_block_module(&mut self, node_id: NodeId) {
        verbose!("Enter block module {node_id}");
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

        verbose!(
            "Define {node_id} {kind} {ident} => {def_id} inside {}",
            self.module()
        );

        if let Some(old_def) = old_def {
            let old_def = self.sess.def_table.def(old_def);
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
}

impl<'ast> AstVisitor<'ast> for DefCollector<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {}

    fn visit_item(&mut self, item: &'ast Item) {
        // External block is not an item by itself, it is a container for them
        match item.kind() {
            ItemKind::Extern(items) => return self.visit_extern_block(items),
            _ => {},
        }

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

        self.enter_def_module(def_id);

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.visit_mod_item(name, items, item.id());
            },
            ItemKind::Type(name, generics, ty) => self.visit_ty_item(name, generics, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
            ItemKind::Adt(name, generics, variants) => {
                self.visit_adt_item(name, generics, variants, item.id())
            },
            ItemKind::Struct(name, generics, fields, ctor_id) => {
                self.visit_struct_item(name, generics, fields, *ctor_id, item.id())
            },
            ItemKind::Extern(_) => unreachable!(),
        }

        self.exit_module();
    }

    fn visit_ty_param(&mut self, ty_param: &'ast TyParam) {
        self.define(
            ty_param.id(),
            DefKind::TyParam,
            ty_param.name.as_ref().unwrap(),
        );
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        let def_id = self.define(variant.id, DefKind::Variant, variant.name.as_ref().unwrap());
        self.define(
            variant.ctor_id,
            DefKind::Ctor,
            variant.name.as_ref().unwrap(),
        );

        self.enter_def_module(def_id);
        walk_each_pr!(self, &variant.fields, visit_field);
        self.exit_module();
    }

    fn visit_field(&mut self, field: &'ast Field) {
        field.accessor_id.map(|accessor_id| {
            self.define(accessor_id, DefKind::FieldAccessor, &field.accessor_name());
        });
    }

    fn visit_struct_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast crate::ast::item::GenericParams,
        fields: &'ast [PR<Field>],
        ctor_def_id: NodeId,
        _id: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);
        self.visit_generic_params(generics);
        walk_each_pr!(self, fields, visit_field);
        self.define(ctor_def_id, DefKind::Ctor, name.as_ref().unwrap());
    }

    fn visit_extern_item(&mut self, item: &'ast ExternItem) {
        self.define(item.id(), DefKind::External, item.name.as_ref().unwrap());
    }

    fn visit_block(&mut self, block: &'ast Block) -> () {
        self.enter_block_module(block.id());
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_module();
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => walk_pr!(self, inner, visit_expr),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
            ExprKind::Lambda(lambda) => {
                self.define(
                    expr.id(),
                    DefKind::Lambda,
                    &Ident::synthetic(format!("lambda{}", expr.id()).intern()),
                );
                self.visit_lambda_expr(lambda);
            },
            ExprKind::DotOp(expr, field) => self.visit_dot_op_expr(expr, field),
            ExprKind::Match(subject, arms) => self.visit_match_expr(subject, arms),
        }
    }
}

impl<'ast> Stage<()> for DefCollector<'ast> {
    fn run(mut self) -> StageResult<()> {
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

        stage_result(self.sess, (), self.msg)
    }
}

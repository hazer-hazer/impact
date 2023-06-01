use crate::{
    ast::{
        expr::{Arm, Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, TyExpr},
        item::{ExternItem, Field, GenericParams, Item, ItemKind, TyParam, Variant},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind, TyPath},
        IdentNode, NodeId, NodeMap, Path, PathSeg, ReservedNodeId, WithNodeId, AST, N, PR,
        ROOT_NODE_ID,
    },
    cli::verbose,
    hir::{
        self,
        item::{Adt, ItemId, ItemNode, Mod, TyAlias},
        stmt::Local,
        Body, BodyId, ExprDefKind, ExprRes, HirId, Node, Owner, OwnerChildId, OwnerId, TyDefKind,
        TyRes, FIRST_OWNER_CHILD_ID, HIR, OWNER_SELF_CHILD_ID,
    },
    message::message::{impl_message_holder, MessageBuilder, MessageHolder, MessageStorage},
    parser::token::{FloatKind, IntKind},
    resolve::{
        builtin::DeclareBuiltin,
        def::{DefId, DefKind},
        res::{self, NamePath, ResKind},
    },
    session::{stage_result, Session, Stage, StageResult, impl_session_holder},
    span::{
        sym::{Ident, Internable, Kw},
        Span, WithSpan,
    },
};

macro_rules! lower_pr {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => $self.$lower(node),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_each_pr {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.into_iter()
            .map(|pr| lower_pr!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

macro_rules! lower_each_pr_flat {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.into_iter()
            .map(|pr| lower_pr!($self, pr, $lower))
            .flatten()
            .collect::<Vec<_>>()
    };
}

macro_rules! lower_each {
    ($self: ident, $nodes: expr, $lower: ident) => {
        $nodes
            .into_iter()
            .map(|pr| $self.$lower(pr))
            .collect::<Vec<_>>()
    };
}

pub struct OwnerCollection {
    owner_id: OwnerId,
    child_id: OwnerChildId,
    owner: Owner,
}

impl OwnerCollection {
    fn next_hir_id(&mut self) -> HirId {
        let hir_id = HirId::new(self.owner_id, self.child_id);
        self.child_id.inc();
        hir_id
    }
}

pub struct Lower<'ast> {
    ast: &'ast AST,

    // HirId
    owner_stack: Vec<OwnerCollection>,
    node_id_hir_id: NodeMap<HirId>,

    sess: Session,
    msg: MessageStorage,
}

impl_message_holder!(Lower<'ast>);
impl_session_holder!(Lower<'ast>);

#[derive(Debug)]
enum LoweredOwner {
    Root(Mod),
    Item(ItemNode),
}

impl Into<Node> for LoweredOwner {
    fn into(self) -> Node {
        match self {
            LoweredOwner::Root(root) => Node::Root(root),
            LoweredOwner::Item(item) => Node::Item(item),
        }
    }
}

impl<'ast> Lower<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            ast,
            owner_stack: Default::default(),
            node_id_hir_id: Default::default(),
            sess,
            msg: Default::default(),
        }
    }

    fn _with_owner(
        &mut self,
        node_id: NodeId,
        def_id: DefId,
        f: impl FnOnce(&mut Self) -> LoweredOwner,
    ) -> OwnerId {
        verbose!("With owner {def_id}");

        let owner_id = OwnerId::new(def_id);

        self.owner_stack.push(OwnerCollection {
            child_id: FIRST_OWNER_CHILD_ID,
            owner_id,
            owner: Owner::default(),
        });

        let owner_stack_size = self.owner_stack.len();

        let owner_node = f(self);

        assert_eq!(owner_stack_size, self.owner_stack.len());

        // Set owner node as the first node in owner
        self.owner_stack
            .last_mut()
            .unwrap()
            .owner
            .nodes
            .insert(OWNER_SELF_CHILD_ID, owner_node.into());

        let owner = self.owner_stack.pop().unwrap().owner;
        self.sess.hir.add_owner(def_id, owner);
        self.node_id_hir_id
            .insert(node_id, HirId::new_owner(def_id));

        owner_id
    }

    fn with_owner(&mut self, owner: NodeId, f: impl FnOnce(&mut Self) -> LoweredOwner) -> OwnerId {
        let def_id = self.sess.def_table.get_def_id(owner).unwrap();
        self._with_owner(owner, def_id, f)
    }

    fn add_node(&mut self, node: Node) -> HirId {
        let hir_id = node.hir_id();
        self.owner_stack
            .last_mut()
            .unwrap()
            .owner
            .nodes
            .insert(hir_id.child_id(), node);

        assert_eq!(self.owner_stack.last().unwrap().owner_id, hir_id.owner());

        hir_id
    }

    fn get_current_owner_node<T>(&self, id: T) -> &Node
    where
        T: Into<HirId>,
    {
        self.owner_stack
            .last()
            .unwrap()
            .owner
            .nodes
            .get_unwrap(id.into().child_id())
    }

    fn lower_node_id(&mut self, id: NodeId) -> HirId {
        if let Some(hir_id) = self.node_id_hir_id.get_flat(id) {
            verbose!("Lower node id {id} -> {}", hir_id);
            *hir_id
        } else {
            let hir_id = self.next_hir_id();
            self.node_id_hir_id.insert(id, hir_id);

            verbose!("Lower node id {id} -> {}", hir_id);

            hir_id
        }
    }

    fn next_hir_id(&mut self) -> HirId {
        self.owner_stack.last_mut().unwrap().next_hir_id()
    }

    fn lower_ast(&mut self) {
        self.with_owner(ROOT_NODE_ID, |this| {
            let mut items = lower_each_pr_flat!(this, this.ast.items(), lower_item);
            items.push(this.builtin_func());
            LoweredOwner::Root(Mod {
                items,
                span: this.sess.def_table.root_span(),
            })
        });
    }

    // Statements //
    fn lower_stmt(&mut self, stmt: &Stmt) -> Vec<hir::Stmt> {
        match stmt.kind() {
            StmtKind::Expr(expr) => {
                vec![hir::stmt::StmtKind::Expr(lower_pr!(self, expr, lower_expr))]
            },
            StmtKind::Item(item) => lower_pr!(self, item, lower_item)
                .into_iter()
                .map(|item| hir::stmt::StmtKind::Item(item))
                .collect(),
            StmtKind::Local(pat, value) => vec![hir::stmt::StmtKind::Local(Local {
                id: self.lower_node_id(stmt.id()),
                pat: lower_pr!(self, pat, lower_pat),
                value: lower_pr!(self, value, lower_expr),
                span: stmt.span(),
            })],
        }
        .into_iter()
        .map(|kind| {
            let id = self.lower_node_id(stmt.id());
            self.add_node(Node::Stmt(hir::stmt::StmtNode::new(id, kind, stmt.span())))
                .into()
        })
        .collect()
    }

    // Items //
    fn lower_item(&mut self, item: &Item) -> Vec<ItemId> {
        match item.kind() {
            ItemKind::Extern(items) => return self.lower_extern_block(items),
            _ => {},
        }

        let owner_id = self.with_owner(item.id(), |this| {
            let def_id = this.sess.def_table.get_def_id(item.id()).unwrap();

            let kind = match item.kind() {
                ItemKind::Type(name, generics, ty) => {
                    this.lower_type_item(name, generics, ty, def_id)
                },
                ItemKind::Mod(name, items) => this.lower_mod_item(name, items, item.span()),
                ItemKind::Decl(name, params, body) => {
                    this.lower_decl_item(name, params, body, def_id)
                },
                ItemKind::Adt(is_adt, name, generics, variants) => {
                    this.lower_data_item(is_adt, name, generics, variants)
                },
                ItemKind::Extern(_) => unreachable!(),
            };

            LoweredOwner::Item(hir::item::ItemNode::new(
                *item.name().unwrap(),
                def_id,
                kind,
                item.span(),
            ))
        });

        vec![ItemId::new(owner_id)]
    }

    fn lower_generic_params(&mut self, generics: &GenericParams) -> hir::item::GenericParams {
        hir::item::GenericParams {
            ty_params: lower_each_pr!(self, &generics.ty_params, lower_ty_param),
        }
    }

    fn lower_ty_param(&mut self, ty_param: &TyParam) -> hir::item::TyParam {
        hir::item::TyParam {
            id: self.lower_node_id(ty_param.id()),
            def_id: self.sess.def_table.get_def_id(ty_param.id()).unwrap(),
            name: lower_pr!(self, &ty_param.name, lower_ident),
        }
    }

    fn lower_type_item(
        &mut self,
        _: &PR<Ident>,
        generics: &GenericParams,
        ty: &PR<N<Ty>>,
        def_id: DefId,
    ) -> hir::item::ItemKind {
        let ty = if let Some(bt_ty) = self.sess.def_table.as_builtin(def_id) {
            self.ty(
                ty.as_ref().unwrap().span(),
                hir::ty::TyKind::Builtin(bt_ty.try_into().unwrap()),
            )
        } else {
            lower_pr!(self, ty, lower_ty)
        };

        hir::item::ItemKind::TyAlias(TyAlias {
            generics: self.lower_generic_params(generics),
            ty,
        })
    }

    fn lower_mod_item(
        &mut self,
        _: &PR<Ident>,
        items: &Vec<PR<N<Item>>>,
        span: Span,
    ) -> hir::item::ItemKind {
        hir::item::ItemKind::Mod(Mod {
            items: lower_each_pr_flat!(self, items, lower_item),
            span,
        })
    }

    fn lower_decl_item(
        &mut self,
        _name: &PR<Ident>,
        ast_params: &Vec<PR<Pat>>,
        body: &PR<N<Expr>>,
        def_id: DefId,
    ) -> hir::item::ItemKind {
        let body = if let Some(bt_expr) = self.sess.def_table.as_builtin(def_id) {
            self.expr(
                body.as_ref().unwrap().span(),
                hir::expr::ExprKind::Builtin(bt_expr.try_into().unwrap()),
            )
        } else {
            lower_pr!(self, body, lower_expr)
        };

        let params = ast_params
            .iter()
            .map(|param| lower_pr!(self, param, lower_pat))
            .collect::<Vec<_>>();

        let body = self.body(params, body);

        if ast_params.is_empty() {
            hir::item::ItemKind::Value(body)
        } else {
            // Note: Removed currying
            // for (index, &param) in params[1..].iter().enumerate().rev() {
            //     let span = ast_params[index].span().to(body.span());
            //     let body = self.body(params, value);
            //     value = self.expr_lambda(span, body);
            // }

            hir::item::ItemKind::Func(body)
        }
    }

    fn lower_data_item(
        &mut self,
        &is_adt: &bool,
        _: &PR<Ident>,
        generics: &GenericParams,
        variants: &[PR<Variant>],
    ) -> hir::item::ItemKind {
        let variants = lower_each_pr!(self, variants, lower_variant);

        if is_adt {
            assert!(variants.iter().copied().all(|v| {
                self.get_current_owner_node(v)
                    .variant()
                    .fields
                    .iter()
                    .all(|f| f.accessor_def_id.is_none())
            }));
        }

        hir::item::ItemKind::Adt(Adt {
            is_adt,
            generics: self.lower_generic_params(generics),
            variants,
        })
    }

    fn lower_variant(&mut self, variant: &Variant) -> hir::Variant {
        let id = self.lower_node_id(variant.id);
        let name = lower_pr!(self, &variant.name, lower_ident);
        let fields = lower_each_pr!(self, &variant.fields, lower_field);
        self.add_node(hir::Node::Variant(hir::item::VariantNode {
            id,
            def_id: self.sess.def_table.get_def_id(variant.id).unwrap(),
            ctor_def_id: self.sess.def_table.get_def_id(variant.ctor_id).unwrap(),
            name,
            fields,
            span: variant.span(),
        }))
        .into()
    }

    fn lower_field(&mut self, field: &Field) -> hir::item::Field {
        hir::item::Field {
            id: self.lower_node_id(field.id),
            name: field
                .name
                .as_ref()
                .map(|name| lower_pr!(self, name, lower_ident)),
            accessor_def_id: field
                .accessor_id
                .and_then(|id| self.sess.def_table.get_def_id(id)),
            ty: lower_pr!(self, &field.ty, lower_ty),
            span: field.span(),
        }
    }

    fn lower_extern_block(&mut self, items: &Vec<PR<ExternItem>>) -> Vec<ItemId> {
        lower_each_pr!(self, items, lower_extern_item)
    }

    fn lower_extern_item(&mut self, item: &ExternItem) -> ItemId {
        let owner_id = self.with_owner(item.id(), |this| {
            let def_id = this.sess.def_table.get_def_id(item.id()).unwrap();
            let name = lower_pr!(this, &item.name, lower_ident);
            let extern_item = hir::item::ExternItem {
                ty: lower_pr!(this, &item.ty, lower_ty),
            };

            LoweredOwner::Item(hir::item::ItemNode::new(
                name,
                def_id,
                hir::item::ItemKind::ExternItem(extern_item),
                item.span(),
            ))
        });

        owner_id.into()
    }

    // Patterns //
    fn lower_pat(&mut self, pat: &Pat) -> hir::Pat {
        verbose!("lower pat {}", pat.id());
        let id = self.lower_node_id(pat.id());

        let kind = match pat.kind() {
            PatKind::Unit => hir::pat::PatKind::Unit,
            PatKind::Ident(ident) => hir::pat::PatKind::Ident(lower_pr!(self, ident, lower_ident)),
        };

        self.add_node(hir::Node::Pat(hir::pat::PatNode::new(id, kind, pat.span())))
            .into()
    }

    // Expressions //
    fn lower_expr(&mut self, expr: &Expr) -> hir::Expr {
        let kind = match expr.kind() {
            ExprKind::Lit(lit) => self.lower_lit_expr(lit),
            ExprKind::Paren(inner) => return lower_pr!(self, inner, lower_expr),
            ExprKind::Path(path) => self.lower_path_expr(path),
            ExprKind::Block(block) => self.lower_block_expr(block),
            ExprKind::Infix(infix) => self.lower_infix_expr(infix),
            ExprKind::Lambda(lambda) => self.lower_lambda_expr(lambda, expr.id()),
            ExprKind::Call(call) => self.lower_call_expr(call),
            ExprKind::Let(block) => self.lower_let_expr(block),
            ExprKind::Ty(ty_expr) => self.lower_ty_expr(ty_expr),
            ExprKind::DotOp(lhs, field) => self.lower_dot_op_expr(lhs, field),
            ExprKind::Match(subject, arms) => self.lower_match_expr(subject, arms),
        };

        let id = self.lower_node_id(expr.id());

        self.add_node(hir::Node::Expr(hir::expr::ExprNode::new(
            id,
            kind,
            expr.span(),
        )))
        .into()
    }

    fn lower_int_kind(&mut self, kind: IntKind) -> hir::expr::IntKind {
        match kind {
            IntKind::Unknown => hir::expr::IntKind::Unknown,
            IntKind::U8 => hir::expr::IntKind::U8,
            IntKind::U16 => hir::expr::IntKind::U16,
            IntKind::U32 => hir::expr::IntKind::U32,
            IntKind::U64 => hir::expr::IntKind::U64,
            IntKind::I8 => hir::expr::IntKind::I8,
            IntKind::I16 => hir::expr::IntKind::I16,
            IntKind::I32 => hir::expr::IntKind::I32,
            IntKind::I64 => hir::expr::IntKind::I64,
            IntKind::Uint => hir::expr::IntKind::Uint,
            IntKind::Int => hir::expr::IntKind::Int,
        }
    }

    fn lower_float_kind(&self, kind: FloatKind) -> hir::expr::FloatKind {
        match kind {
            FloatKind::Unknown => hir::expr::FloatKind::Unknown,
            FloatKind::F32 => hir::expr::FloatKind::F32,
            FloatKind::F64 => hir::expr::FloatKind::F64,
        }
    }

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lit(match lit {
            Lit::Bool(val) => hir::expr::Lit::Bool(*val),
            Lit::Int(val, kind) => hir::expr::Lit::Int(*val, self.lower_int_kind(*kind)),
            Lit::Float(val, kind) => hir::expr::Lit::Float(*val, self.lower_float_kind(*kind)),
            Lit::String(val) => hir::expr::Lit::String(*val),
        })
    }

    fn lower_path_expr(&mut self, path: &PathExpr) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Path(lower_pr!(self, &path.0, lower_expr_path))
    }

    fn lower_block_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Block(lower_pr!(self, block, lower_block))
    }

    fn lower_infix_expr(&mut self, infix: &Infix) -> hir::expr::ExprKind {
        // Note [TRANSFORM]: [lhs] [op] [rhs] -> [op] [lhs] [rhs]
        let op_path_span = infix.op.0.as_ref().unwrap().span();
        let op_path = lower_pr!(self, &infix.op.0, lower_expr_path);
        let op_path = self.expr_path(op_path_span, op_path);

        let first_op_arg = lower_pr!(self, &infix.lhs, lower_expr);
        hir::expr::ExprKind::Call(hir::expr::Call {
            lhs: self.expr_call(
                op_path_span.to(infix.lhs.span()),
                op_path,
                vec![first_op_arg],
            ),
            args: vec![lower_pr!(self, &infix.rhs, lower_expr)],
        })
    }

    fn lower_lambda_expr(&mut self, lambda: &Lambda, node_id: NodeId) -> hir::expr::ExprKind {
        let params = lower_each_pr!(self, &lambda.params, lower_pat);
        let body = lower_pr!(self, &lambda.body, lower_expr);
        hir::expr::ExprKind::Lambda(hir::expr::Lambda {
            def_id: self.sess.def_table.get_def_id(node_id).unwrap(),
            body_id: self.body(params, body),
        })
    }

    fn lower_call_expr(&mut self, call: &Call) -> hir::expr::ExprKind {
        let lhs = lower_pr!(self, &call.lhs, lower_expr);
        let args = lower_each_pr!(self, &call.args, lower_expr);

        hir::expr::ExprKind::Call(hir::expr::Call { lhs, args })
    }

    fn lower_let_expr(&mut self, block: &PR<Block>) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Let(lower_pr!(self, block, lower_block))
    }

    fn lower_ty_expr(&mut self, ty_expr: &TyExpr) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ty(hir::expr::TyExpr {
            expr: lower_pr!(self, &ty_expr.expr, lower_expr),
            ty: lower_pr!(self, &ty_expr.ty, lower_ty),
        })
    }

    fn lower_dot_op_expr(
        &mut self,
        lhs: &PR<N<Expr>>,
        field: &PR<IdentNode>,
    ) -> hir::expr::ExprKind {
        let field = lower_pr!(self, field, lower_ident_node);
        let field_span = self.get_current_owner_node(field).expr_path().span();
        hir::expr::ExprKind::Call(hir::expr::Call {
            lhs: self.expr_path(field_span, field),
            args: vec![lower_pr!(self, lhs, lower_expr)],
        })
        // let lhs = lower_pr!(self, lhs, lower_expr);
        // let field = lower_pr!(self, field, lower_ident_node);
        // let field_node = self.get_current_owner_node(field).path();

        // if let Res::Def(DefKind::FieldAccessor, _) = field_node.res() {
        //     assert!(field_node.segments().len() == 1);
        //     hir::expr::ExprKind::FieldAccess(lhs, field_node.target_name())
        // } else {
        //     hir::expr::ExprKind::Call(hir::expr::Call {
        //         lhs: self.expr_path(field_node.span(), field),
        //         args: vec![lhs],
        //     })
        // }
    }

    fn lower_match_expr(&mut self, subject: &PR<N<Expr>>, arms: &[PR<Arm>]) -> hir::expr::ExprKind {
        let subject = lower_pr!(self, subject, lower_expr);
        let arms = lower_each_pr!(self, arms, lower_match_arm);
        hir::expr::ExprKind::Match(subject, arms)
    }

    fn lower_match_arm(&mut self, arm: &Arm) -> hir::expr::Arm {
        hir::expr::Arm {
            pat: lower_pr!(self, &arm.pat, lower_pat),
            body: lower_pr!(self, &arm.body, lower_expr),
            span: arm.span(),
        }
    }

    // Types //
    fn lower_ty(&mut self, ty: &Ty) -> hir::Ty {
        let kind = match ty.kind() {
            TyKind::Path(path) => Ok(self.lower_ty_path(path)),
            TyKind::Func(params, body) => Ok(self.lower_func_ty(params, body)),
            TyKind::Paren(inner) => return lower_pr!(self, inner, lower_ty),
            TyKind::App(cons, args) => Ok(self.lower_ty_app(cons, args)),
            TyKind::AppExpr(cons, args) => self.lower_ty_app_expr(cons, args),
        };

        let id = self.lower_node_id(ty.id());

        if let Ok(kind) = kind {
            self.add_node(Node::Ty(hir::ty::TyNode::new(id, kind, ty.span())))
        } else {
            self.add_node(Node::Error(hir::ErrorNode {
                id,
                span: ty.span(),
            }))
        }
        .into()
    }

    fn lower_ty_path(&mut self, path: &TyPath) -> hir::ty::TyKind {
        let path = path.0.as_ref().unwrap();
        let id = self.lower_node_id(path.id());
        let segments = lower_each!(self, path.segments(), lower_path_seg);
        let res = self.lower_ty_res(self.sess.res.get(NamePath::new(path.id())).unwrap());
        let path = self
            .add_node(Node::TyPath(hir::PathNode::new(
                id,
                res,
                segments,
                path.span(),
            )))
            .into();
        hir::ty::TyKind::Path(path)
    }

    fn lower_func_ty(&mut self, params: &Vec<PR<N<Ty>>>, body: &PR<N<Ty>>) -> hir::ty::TyKind {
        hir::ty::TyKind::Func(
            lower_each_pr!(self, params, lower_ty),
            lower_pr!(self, body, lower_ty),
        )
    }

    fn lower_ty_app(&mut self, cons: &PR<N<Ty>>, args: &[PR<N<Ty>>]) -> hir::ty::TyKind {
        hir::ty::TyKind::App(
            lower_pr!(self, cons, lower_ty),
            lower_each_pr!(self, args, lower_ty),
        )
    }

    fn lower_ty_app_expr(
        &mut self,
        cons: &PR<N<Ty>>,
        _args: &[PR<N<Expr>>],
    ) -> Result<hir::ty::TyKind, ()> {
        let cons = lower_pr!(self, cons, lower_ty);
        let cons_node = self.get_current_owner_node(cons).ty();
        let cons_span = cons_node.span();

        MessageBuilder::error()
            .span(cons_span)
            .text(
                "Type constructors with const parameters can only be used with builtin for now"
                    .to_string(),
            )
            .emit_single_label(self);
        Err(())
    }

    // Fragments //
    fn lower_ident(&mut self, ident: &Ident) -> Ident {
        *ident
    }

    fn lower_ident_node(&mut self, ident: &IdentNode) -> hir::ExprPath {
        let id = self.lower_node_id(ident.id());
        let res = self.lower_expr_res(self.sess.res.get(NamePath::new(ident.id())).unwrap());
        self.add_node(Node::ExprPath(hir::PathNode::new(
            id,
            res,
            vec![hir::PathSeg::new(ident.ident, ident.ident.span())],
            ident.ident.span(),
        )))
        .into()
    }

    fn lower_expr_res(&mut self, res: res::Res) -> ExprRes {
        match res.kind() {
            &res::ResKind::Local(node_id) => ExprRes::Local(
                *self
                    .node_id_hir_id
                    .get_expect(node_id, &format!("Local resolution {}", res)),
            ),
            &res::ResKind::Def(def_id) => {
                let def = self.sess.def_table.def(def_id);
                // assert!(self.sess.def_table.as_builtin(def_id).expect(&format!("{def} builtin
                // must not appear in ")));

                ExprRes::Def(
                    match def.kind() {
                        DefKind::Func => ExprDefKind::Func,
                        DefKind::Value => ExprDefKind::Value,
                        DefKind::Ctor => ExprDefKind::Ctor,
                        DefKind::FieldAccessor => ExprDefKind::FieldAccessor,
                        DefKind::External => ExprDefKind::External,
                        _ => unreachable!(),
                    },
                    def_id,
                )
            },
            ResKind::DeclareBuiltin | res::ResKind::Err => unreachable!(),
        }
    }

    fn lower_ty_res(&mut self, res: res::Res) -> TyRes {
        match res.kind() {
            &res::ResKind::Local(_) => unreachable!(),
            &res::ResKind::Def(def_id) => {
                // assert!(self.sess.def_table.as_builtin(def_id).is_none());

                let def_kind = self.sess.def_table.def(def_id).kind();
                TyRes::Def(
                    match def_kind {
                        DefKind::TyAlias => TyDefKind::TyAlias,
                        DefKind::Adt => TyDefKind::Adt,
                        DefKind::TyParam => TyDefKind::TyParam,
                        _ => unreachable!(),
                    },
                    def_id,
                )
            },
            &res::ResKind::DeclareBuiltin | res::ResKind::Err => unreachable!(),
        }
    }

    fn lower_expr_path(&mut self, path: &Path) -> hir::ExprPath {
        let id = self.lower_node_id(path.id());
        let segments = lower_each!(self, path.segments(), lower_path_seg);
        let res = self.lower_expr_res(self.sess.res.get(NamePath::new(path.id())).unwrap());
        self.add_node(Node::ExprPath(hir::PathNode::new(
            id,
            res,
            segments,
            path.span(),
        )))
        .into()
    }

    fn lower_path_seg(&mut self, seg: &PathSeg) -> hir::PathSeg {
        hir::PathSeg::new(self.lower_ident(seg.expect_name()), seg.span())
    }

    fn lower_block(&mut self, block: &Block) -> hir::Block {
        assert!(!block.stmts().is_empty());

        let mut stmts = block.stmts()[0..block.stmts().len() - 1]
            .iter()
            .map(|stmt| lower_pr!(self, stmt, lower_stmt))
            .flatten()
            .collect::<Vec<_>>();

        let last_stmt = block.stmts().last().unwrap().as_ref().unwrap();

        let expr = match last_stmt.kind() {
            StmtKind::Expr(expr) => Some(lower_pr!(self, expr, lower_expr)),
            StmtKind::Item(item) => {
                let span = item.span();
                stmts.extend(
                    lower_pr!(self, item, lower_item)
                        .into_iter()
                        .map(|item| self.stmt_item(span, item)),
                );
                None
            },
            StmtKind::Local(pat, value) => {
                let span = last_stmt.span();
                let id = self.lower_node_id(last_stmt.id());
                let pat = lower_pr!(self, pat, lower_pat);
                let value = lower_pr!(self, value, lower_expr);
                stmts.push(self.stmt(
                    span,
                    hir::stmt::StmtKind::Local(hir::stmt::Local {
                        id,
                        pat,
                        value,
                        span,
                    }),
                ));
                None
            },
        };

        let id = self.lower_node_id(block.id());
        self.add_node(Node::Block(hir::expr::BlockNode::new(
            id,
            stmts,
            expr,
            block.span(),
        )))
        .into()
    }

    // Synthesis //
    fn stmt(&mut self, span: Span, kind: hir::stmt::StmtKind) -> hir::Stmt {
        let id = self.next_hir_id();
        self.add_node(Node::Stmt(hir::stmt::StmtNode::new(id, kind, span)))
            .into()
    }

    fn stmt_item(&mut self, span: Span, item: ItemId) -> hir::Stmt {
        self.stmt(span, hir::stmt::StmtKind::Item(item))
    }

    fn item(
        &mut self,
        span: Span,
        name: Ident,
        node_id: NodeId,
        def_id: DefId,
        kind: hir::item::ItemKind,
    ) -> ItemId {
        let owner_id = self._with_owner(node_id, def_id, |_this| {
            LoweredOwner::Item(ItemNode::new(name, def_id, kind, span))
        });
        ItemId::new(owner_id)
    }

    fn pat(&mut self, span: Span, kind: hir::pat::PatKind) -> hir::Pat {
        let id = self.next_hir_id();
        self.add_node(Node::Pat(hir::pat::PatNode::new(id, kind, span)))
            .into()
    }

    fn pat_ident(&mut self, ident: Ident) -> hir::Pat {
        self.pat(ident.span(), hir::pat::PatKind::Ident(ident))
    }

    fn ty(&mut self, span: Span, kind: hir::ty::TyKind) -> hir::Ty {
        let id = self.next_hir_id();
        self.add_node(Node::Ty(hir::ty::TyNode::new(id, kind, span)))
            .into()
    }

    fn expr(&mut self, span: Span, kind: hir::expr::ExprKind) -> hir::Expr {
        let id = self.next_hir_id();
        self.add_node(Node::Expr(hir::expr::ExprNode::new(id, kind, span)))
            .into()
    }

    fn expr_call(&mut self, span: Span, lhs: hir::Expr, args: Vec<hir::Expr>) -> hir::Expr {
        self.expr(
            span,
            hir::expr::ExprKind::Call(hir::expr::Call { lhs, args }),
        )
    }

    fn expr_lit(&mut self, span: Span, lit: hir::expr::Lit) -> hir::Expr {
        self.expr(span, hir::expr::ExprKind::Lit(lit))
    }

    fn expr_path(&mut self, span: Span, path: hir::ExprPath) -> hir::Expr {
        self.expr(span, hir::expr::ExprKind::Path(path))
    }

    fn expr_lambda(&mut self, span: Span, body: hir::BodyId) -> hir::Expr {
        let node_id = self.sess.next_node_id();
        let def_id = self.sess.def_table.define(
            node_id,
            DefKind::Lambda,
            &Ident::synthetic("lambda".intern()),
        );
        self.expr(
            span,
            hir::expr::ExprKind::Lambda(hir::expr::Lambda {
                def_id,
                body_id: body,
            }),
        )
    }

    fn body(&mut self, params: Vec<hir::Pat>, value: hir::Expr) -> BodyId {
        let body = Body::new(params, value);
        let id = body.id();

        self.owner_stack
            .last_mut()
            .unwrap()
            .owner
            .bodies
            .insert(id.inner().child_id(), body);

        assert_eq!(
            self.owner_stack.last().unwrap().owner_id,
            id.inner().owner()
        );

        id
    }

    fn builtin_func(&mut self) -> ItemId {
        let params = vec![self.pat_ident(Ident::kw(Kw::Underscore))];
        let body = self.expr_lit(
            Span::new_error(),
            hir::expr::Lit::String(DeclareBuiltin::sym()),
        );
        let body = self.body(params, body);
        let _value = self.expr_lambda(Span::new_error(), body);
        let node_id = self
            .sess
            .ast_metadata
            .reserve(ReservedNodeId::DeclareBuiltin);

        self.item(
            Span::new_error(),
            DeclareBuiltin::ident(),
            node_id,
            self.sess.def_table.builtin_func().def_id(),
            hir::item::ItemKind::Func(body),
        )
    }
}

impl<'ast> Stage<()> for Lower<'ast> {
    fn run(mut self) -> StageResult<()> {
        self.lower_ast();
        stage_result(self.sess, (), self.msg)
    }
}

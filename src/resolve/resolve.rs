use std::collections::HashMap;

use super::{
    builtin::Builtin,
    def::{DefId, ModuleId, Namespace, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{Candidate, NamePath, Res},
};
use crate::{
    ast::{
        expr::{Arm, Block, Call, Expr, ExprKind, Lit, PathExpr},
        item::{GenericParams, Item, ItemKind, Variant},
        pat::{Pat, PatKind},
        ty::{Ty, TyKind, TyPath},
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, IdentNode, NodeId, NodeMap, Path, WithNodeId, AST, N, PR,
    },
    cli::verbose,
    message::message::{impl_message_holder, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::{TyBuiltin, ValueBuiltin},
        def::{DebugModuleTree, DefKind},
        res::ResKind,
    },
    session::{impl_session_holder, stage_result, Session, Stage, StageResult},
    span::{
        sym::{Ident, IdentKind, Symbol},
        Span, WithSpan,
    },
};

impl MessageBuilder {
    fn candidate(self, candidate: Candidate) -> Self {
        match candidate {
            Candidate::None => self,
            Candidate::Local(local) => self.label(local.span(), format!("local {local}")),
            Candidate::Defs(defs) => defs.iter().fold(self, |this, def| {
                this.label(
                    def.name().span(),
                    format!(
                        "Found {} `{}` ({})",
                        def.kind(),
                        def.name(),
                        def.kind().namespace()
                    ),
                )
            }),
        }
    }
}

#[derive(Clone, Copy)]
enum SearchForSpecific {
    Op,
}

impl std::fmt::Display for SearchForSpecific {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SearchForSpecific::Op => "operator",
            }
        )
    }
}

#[derive(Clone, Copy)]
enum SearchForKind {
    Specific(SearchForSpecific),
    Def(DefKind),
    FromNs(Namespace),
}

impl SearchForKind {
    fn namespace(&self) -> Namespace {
        match self {
            SearchForKind::Def(def_kind) => def_kind.namespace(),
            &SearchForKind::FromNs(ns) => ns,
            SearchForKind::Specific(specific) => match specific {
                SearchForSpecific::Op => Namespace::Value,
            },
        }
    }
}

impl std::fmt::Display for SearchForKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SearchForKind::Def(def_kind) => write!(f, "{def_kind}"),
            SearchForKind::FromNs(ns) => write!(f, "{ns}"),
            SearchForKind::Specific(specific) => write!(f, "{specific}"),
        }
    }
}

#[derive(Clone, Copy)]
struct SearchFor(Ident, SearchForKind);

impl std::fmt::Display for SearchFor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} `{}`", self.1, self.0)
    }
}

/// The Err variant here is Path prefix (None if "current scope"), prefix and
/// possible candidate for resolution.
struct ResolutionErr {
    /// Target name attempted to resolve
    target: SearchFor,
    /// Path prefix string (None if "current scope")
    prefix_str: Option<String>,
    /// Path prefix span
    prefix_span: Span,
    /// Possible candidate(-s) of resolution
    candidate: Candidate,
}

impl ResolutionErr {
    fn absolute(
        target: SearchFor,
        prefix_str: Option<String>,
        prefix_span: Span,
        candidate: Candidate,
    ) -> Self {
        Self {
            target,
            prefix_str,
            prefix_span,
            candidate,
        }
    }

    pub fn relative(target: SearchFor, candidate: Candidate) -> Self {
        Self {
            target,
            prefix_str: None,
            prefix_span: target.0.span(),
            candidate,
        }
    }
}

/// Resolution result, handled by error checker, resolution is not only a Res,
/// but can be ModuleId.
type ResolutionResult<R> = Result<R, ResolutionErr>;

trait ResolutionResultImpl {
    fn emit(self, mh: &mut impl MessageHolder) -> Res;
}

impl ResolutionResultImpl for ResolutionResult<Res> {
    fn emit(self, mh: &mut impl MessageHolder) -> Res {
        match self {
            Ok(ok) => ok,
            Err(err) => {
                let prefix_str = err.prefix_str.unwrap_or("current scope".to_string());

                MessageBuilder::error()
                    .span(err.prefix_span)
                    .text(format!(
                        "Cannot find {} `{}` in {}",
                        err.target.1, err.target.0, prefix_str
                    ))
                    .label(
                        err.target.0.span(),
                        format!(
                            "{} `{}`{} is not defined in {}",
                            err.target.1,
                            err.target.0,
                            if let Candidate::None = err.candidate {
                                "".to_string()
                            } else {
                                format!(" ({})", err.target.1.namespace())
                            },
                            prefix_str
                        ),
                    )
                    .candidate(err.candidate)
                    .emit(mh);

                Res::error()
            },
        }
    }
}

#[derive(Debug)]
enum Scope {
    Local(HashMap<Symbol, NodeId>),
    Module(ModuleId),
}

impl Scope {
    pub fn add_local(&mut self, name: Symbol, node_id: NodeId) -> Option<NodeId> {
        match self {
            Self::Local(locals) => locals.insert(name, node_id),
            _ => panic!("Cannot add local to non-local scope"),
        }
    }
}

pub struct NameResolver<'ast> {
    ast: &'ast AST,
    scopes: Vec<Scope>,
    /// Nearest `mod` item
    nearest_mod_item: ModuleId,
    locals_spans: NodeMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl_message_holder!(NameResolver<'ast>);
impl_session_holder!(NameResolver<'ast>);

impl<'ast> NameResolver<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            ast,
            scopes: Default::default(),
            nearest_mod_item: ModuleId::Def(ROOT_DEF_ID),
            locals_spans: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn define_local(&mut self, node_id: NodeId, ident: &Ident) {
        verbose!("Define var {node_id} {}", ident);

        if let &Scope::Module(module_id) = self.scope() {
            // If we're not in block, then variable must defined as value in `DefCollector`
            // TODO: Review this assert, condition is strange
            assert!(
                self.sess
                    .def_table
                    .get_module(module_id)
                    .get_from_ns(Namespace::Value, ident.sym())
                    .is_ok(),
                "Value {} must be defined in Module scope",
                ident
            );
            return;
        }

        let old_local = self.scope_mut().add_local(ident.sym(), node_id);

        if let Some(old_local) = old_local {
            MessageBuilder::error()
                .span(ident.span())
                .text(format!(
                    "Duplicate local variable `{}` definition",
                    ident.sym()
                ))
                .label(
                    *self.locals_spans.get_unwrap(old_local),
                    "Previously defined here".to_string(),
                )
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        } else {
            self.sess.def_table.define(node_id, DefKind::Local, ident);
            self.locals_spans.insert(node_id, ident.span());
        }
    }

    fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn in_local_scope(&self) -> bool {
        matches!(self.scope(), Scope::Local(_))
    }

    fn enter_module_scope(&mut self, module_id: ModuleId) {
        verbose!(
            "Enter module scope {} {:?}",
            module_id,
            self.sess.def_table.get_module(module_id)
        );

        self.scopes.push(Scope::Module(module_id));
    }

    fn enter_local_scope(&mut self) {
        self.scopes.push(Scope::Local(Default::default()));
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn def_res(&self, search_for: SearchFor, def_id: DefId) -> Res {
        let def = self.sess.def_table.def(def_id);

        match def.kind() {
            DefKind::Lambda
            | DefKind::Root
            | DefKind::TyAlias
            | DefKind::Mod
            | DefKind::Func
            | DefKind::External
            | DefKind::Adt
            | DefKind::Variant
            | DefKind::Struct
            | DefKind::Ctor
            | DefKind::FieldAccessor
            | DefKind::Value
            | DefKind::TyParam => {
                assert_eq!(def.kind().namespace(), search_for.1.namespace());
                return Res::def(def_id);
            },
            DefKind::DeclareBuiltin => {
                // Is does not matter in which namespace we found `builtin`
                // Yeah, crutches
                return Res::declare_builtin();
            },
            DefKind::Local => unreachable!(),
        }
    }

    fn resolve_in_module(
        &self,
        module_id: ModuleId,
        search_for: SearchFor,
    ) -> Result<DefId, Candidate> {
        let name = search_for.0;
        let ns = search_for.1.namespace();
        verbose!("Resolve {name} from {ns} in {module_id}");

        match self
            .sess
            .def_table
            .get_module(module_id)
            .get_from_ns(search_for.1.namespace(), search_for.0.sym())
        {
            Ok(def_id) => Ok(def_id),
            Err(defs) => Err(if defs.is_empty() {
                Candidate::None
            } else {
                Candidate::defs(
                    defs.iter()
                        .copied()
                        .map(|def_id| self.sess.def_table.def(def_id))
                        .collect(),
                )
            }),
        }
    }

    /// Try to find a local variable or ascend scopes looking for a name
    fn resolve_relative(&mut self, search_for: SearchFor) -> ResolutionResult<Res> {
        let mut candidate = Candidate::None;

        let name = search_for.0;
        let target_ns = search_for.1.namespace();

        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Local(locals) if target_ns == Namespace::Value => {
                    let local = &locals.get(&name.sym());
                    if let Some(&local) = local {
                        return Ok(Res::local(local));
                    }
                },
                Scope::Local(locals_candidates) => {
                    if let Some(&local) = locals_candidates.get(&name.sym()) {
                        candidate.set_local(Ident::new(
                            self.locals_spans.get_copied_unwrap(local),
                            name.sym(),
                        ));
                    }
                },
                &Scope::Module(module_id) => match self.resolve_in_module(module_id, search_for) {
                    Ok(def_id) => return Ok(self.def_res(search_for, def_id)),
                    Err(candidate_) => {
                        verbose!(
                            "Def {name} not found in, candidate: {candidate_}\n{}",
                            DebugModuleTree::new(
                                &self.sess,
                                module_id,
                                Some(name.sym()),
                                Some(module_id),
                            )
                            .debug()
                        );

                        candidate.set_if_none(candidate_);
                    },
                },
            }
        }

        // MessageBuilder::error()
        //     .span(name.span())
        //     .text(format!("`{}` is not defined in current scope", name))
        //     .candidate(candidate)
        //     .emit_single_label(self);

        Err(ResolutionErr::relative(search_for, candidate))
    }

    // fn resolve_member(&mut self, field: &Ident) -> Res {
    //     self.resolve_relative(SearchFor(
    //         *field,
    //         SearchForKind::Def(DefKind::FieldAccessor),
    //     ))
    //     .emit(self)
    // }

    fn resolve_module_relative(&mut self, name: Ident) -> ResolutionResult<ModuleId> {
        let search_for = SearchFor(name, SearchForKind::Def(DefKind::Mod));
        let relative_res = self.resolve_relative(search_for)?;

        let def_id = match relative_res.kind() {
            // We cannot get local resolution from type namespace
            ResKind::Local(_) => unreachable!(),
            &ResKind::Def(def_id) => def_id,
            ResKind::DeclareBuiltin => self.sess.def_table.builtin_func().def_id(),
            ResKind::Err => unreachable!(),
        };

        let def = self.sess.def_table.def(def_id);
        match def.kind() {
            // TODO: Review Adt + Variant as module
            DefKind::Struct | DefKind::Adt | DefKind::Variant | DefKind::Mod => {
                Ok(ModuleId::Def(def_id))
            },
            DefKind::DeclareBuiltin | DefKind::TyAlias => {
                MessageBuilder::error()
                    .span(name.span())
                    .text(format!("{} `{}` is not a module", def.kind(), def.name()))
                    .emit_single_label(self);
                // Note: Do not suggest useless candidate which is not a module
                Err(ResolutionErr::relative(search_for, Candidate::None))
            },
            // FIXME: Really unreachable?
            DefKind::Root
            | DefKind::Ctor
            | DefKind::FieldAccessor
            | DefKind::Func
            | DefKind::Value
            | DefKind::Lambda
            | DefKind::Local
            | DefKind::External
            | DefKind::TyParam => unreachable!(),
        }
    }

    /// Resolution strategy is different for single-segment paths
    /// and multiple-segment paths.
    fn resolve_path(&mut self, path: &Path, search_for_kind: SearchForKind) {
        let res = if path.segments().len() == 1 {
            // TODO: Assert that segment is lowercase for local variable?
            let search_for = SearchFor(*path.segments()[0].expect_name(), search_for_kind);
            self.resolve_relative(search_for)
        } else {
            self.resolve_path_absolute(search_for_kind, path)
        }
        .emit(self);

        verbose!("Resolve path `{path}`{} as {res}", path.id());

        self.sess.res.set(NamePath::new(path.id()), res);
    }

    /// Relatively find definition of first segment then go by modules rest
    /// segments point to
    fn resolve_path_absolute(
        &mut self,
        search_for_kind: SearchForKind,
        path: &Path,
    ) -> ResolutionResult<Res> {
        assert!(path.segments().len() > 1);

        let target_ns = search_for_kind.namespace();

        let mut search_mod =
            self.resolve_module_relative(*path.segments().first().as_ref().unwrap().expect_name())?;

        for (index, seg) in path.segments().iter().enumerate().skip(1) {
            let is_target = index == path.segments().len() - 1;
            let name = *seg.expect_name();
            let prefix_str = &path.prefix_str(index);
            let prefix_span = path.prefix_span(index);

            // Path prefix segments must be type identifiers
            if !is_target && !seg.expect_name().is_ty() {
                panic!("Must be check in ASTValidator");
                // MessageBuilder::error()
                //     .span(prefix_span)
                //     .text(format!("Invalid path `{}`", path))
                //     .label(
                //         seg.span(),
                //         format!("{} must be a type or module name",
                // seg_name),     )
                //     .emit(self);
                // return Res::error();
            }

            let search_for = if is_target {
                SearchFor(name, search_for_kind)
            } else {
                let search_for_kind = match name.kind() {
                    Some(kind) => match kind {
                        IdentKind::Var => SearchForKind::Def(DefKind::Mod),
                        IdentKind::Ty => SearchForKind::FromNs(Namespace::Type),
                        // Actually, operator cannot be prefix
                        IdentKind::Op => SearchForKind::FromNs(Namespace::Value),
                    },
                    None => SearchForKind::Def(DefKind::Mod),
                };
                SearchFor(name, search_for_kind)
            };

            verbose!(
                "Try to find [{prefix_str}] ::{name} inside\n{}",
                DebugModuleTree::new(&self.sess, search_mod, Some(name.sym()), None).debug()
            );

            let def_id = match self.resolve_in_module(search_mod, search_for) {
                Ok(def_id) => def_id,
                Err(candidate) => {
                    verbose!(
                        "Def {search_for} not found inside this module, prefix {prefix_str}, candidate: {candidate}",
                    );
                    return Err(ResolutionErr::absolute(
                        search_for,
                        Some(prefix_str.clone()),
                        prefix_span,
                        candidate,
                    ));
                },
            };

            verbose!(
                "Found {} by [{prefix_str}]::{name}",
                self.sess.def_table.def(def_id)
            );

            if is_target {
                return Ok(self.def_res(search_for, def_id));
            } else {
                search_mod = ModuleId::Def(def_id);
            }
        }

        unreachable!()
    }

    fn maybe_builtin(&mut self, path: &Path, args: &[&Expr]) -> Option<Builtin> {
        // Note: All paths in declaration body must already be resolved
        let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
        if let ResKind::DeclareBuiltin = res.kind() {
            if args.len() != 1 {
                MessageBuilder::error()
                    .span(path.span())
                    .text(format!(
                        "`builtin` expects exactly one argument, but {} were supplied",
                        args.len()
                    ))
                    .emit_single_label(self);
                return None;
            }

            let builtin_name_arg = args[0];
            let arg_span = builtin_name_arg.span();

            let name = match builtin_name_arg.kind() {
                ExprKind::Lit(lit) => match lit {
                    Lit::String(name) => Some(name),
                    _ => None,
                },
                _ => None,
            };
            if let Some(name) = name {
                if let Ok(bt) = Builtin::try_from(name.as_str()) {
                    Some(bt)
                } else {
                    MessageBuilder::error()
                        .text(format!("Unknown builtin `{}`", name))
                        .span(arg_span)
                        .emit_single_label(self);
                    None
                }
            } else {
                MessageBuilder::error()
                    .text("`builtin` expects string literal (builtin name) as argument".to_string())
                    .span(arg_span)
                    .emit_single_label(self);
                None
            }
        } else {
            None
        }
    }

    fn match_path_expr(expr: &PR<N<Expr>>) -> Option<&Path> {
        if let Ok(ExprKind::Path(PathExpr(Ok(path)))) =
            expr.as_ref().map_or(Err(()), |expr| Ok(expr.kind()))
        {
            Some(path)
        } else {
            None
        }
    }

    fn match_ty_path(ty: &PR<N<Ty>>) -> Option<&Path> {
        if let Ok(TyKind::Path(TyPath(Ok(path)))) = ty.as_ref().map_or(Err(()), |ty| Ok(ty.kind()))
        {
            Some(path)
        } else {
            None
        }
    }
}

impl<'ast> AstVisitor<'ast> for NameResolver<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {}

    fn visit_item(&mut self, item: &'ast Item) {
        match item.kind() {
            ItemKind::Decl(name, params, body) if params.is_empty() => {
                self.define_local(item.id(), name.as_ref().unwrap());
                self.visit_decl_item(name, params, body, item.id());
                return;
            },
            ItemKind::Extern(items) => return self.visit_extern_block(items),
            _ => {},
        }

        let def_id = self
            .sess
            .def_table
            .get_def_id(item.id())
            .expect(&format!("Expected DefId for NodeId{}", item.id()));
        self.enter_module_scope(ModuleId::Def(def_id));

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.nearest_mod_item = ModuleId::Def(def_id);
                self.visit_mod_item(name, items, item.id());
            },
            ItemKind::Type(name, generics, ty) => self.visit_ty_item(name, generics, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id());
            },
            ItemKind::Adt(name, generics, variants) => {
                self.visit_adt_item(name, generics, variants, item.id())
            },
            ItemKind::Struct(name, generics, fields, ctor_id) => {
                self.visit_struct_item(name, generics, fields, *ctor_id, item.id())
            },
            ItemKind::Extern(_) => unreachable!(),
        }

        self.exit_scope();
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);

        self.enter_local_scope();

        walk_each_pr!(self, params, visit_pat);
        walk_pr!(self, body, visit_expr);

        self.exit_scope();

        // Resolve builtin declaration
        if params.is_empty() {
            if let ExprKind::Call(Call { lhs, args }) = body.as_deref().unwrap().kind() {
                if let ExprKind::Path(PathExpr(path)) = lhs.as_deref().unwrap().kind() {
                    let path = path.as_ref().unwrap();
                    let maybe_builtin = self.maybe_builtin(
                        path,
                        &args
                            .iter()
                            .map(|arg| arg.as_deref().unwrap())
                            .collect::<Vec<_>>(),
                    );

                    if let Some(bt) = maybe_builtin {
                        let bt_expr: Result<ValueBuiltin, _> = bt.try_into();
                        if let Ok(bt_expr) = bt_expr {
                            self.sess.def_table.add_builtin(
                                bt_expr.into(),
                                self.sess.def_table.get_def_id(id).unwrap(),
                            )
                        } else {
                            MessageBuilder::error()
                                .text(format!("{} builtin cannot be used as value", bt))
                                .span(args.first().unwrap().span())
                                .emit_single_label(self);
                        }
                    }
                }
            }
        }
    }

    fn visit_ty_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast GenericParams,
        ty: &'ast PR<N<Ty>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);
        self.visit_generic_params(generics);
        walk_pr!(self, ty, visit_ty);

        if let TyKind::AppExpr(lhs, args) = ty.as_deref().unwrap().kind() {
            if let TyKind::Path(TyPath(path)) = lhs.as_deref().unwrap().kind() {
                let path = path.as_ref().unwrap();
                let maybe_builtin = self.maybe_builtin(
                    path,
                    &args
                        .iter()
                        .map(|arg| arg.as_deref().unwrap())
                        .collect::<Vec<_>>(),
                );

                if let Some(bt) = maybe_builtin {
                    let bt_ty: Result<TyBuiltin, _> = bt.try_into();
                    if let Ok(bt_expr) = bt_ty {
                        self.sess.def_table.add_builtin(
                            bt_expr.into(),
                            self.sess.def_table.get_def_id(id).unwrap(),
                        )
                    } else {
                        MessageBuilder::error()
                            .text(format!("{} builtin cannot be used as type", bt))
                            .span(args.first().unwrap().span())
                            .emit_single_label(self);
                    }
                    // Continue lowering even an error appeared.
                    // Lowering methods do not handle Results :(
                }
            }
        }
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        self.enter_module_scope(ModuleId::Def(
            self.sess.def_table.get_def_id(variant.id()).unwrap(),
        ));
        walk_each_pr!(self, &variant.fields, visit_field);
        self.exit_scope();
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        verbose!("Visit pat {}", pat.id());
        match pat.kind() {
            PatKind::Unit => {},
            PatKind::Ident(ident) => self.define_local(pat.id(), ident.as_ref().unwrap()),
        }
    }

    fn visit_match_expr(&mut self, subject: &'ast PR<N<Expr>>, arms: &'ast [PR<Arm>]) {
        self.enter_local_scope();
        walk_pr!(self, subject, visit_expr);
        walk_each_pr!(self, arms, visit_match_arm);
        self.exit_scope();
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.enter_module_scope(ModuleId::Block(block.id()));
        self.enter_local_scope();
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_scope();
        self.exit_scope();
    }

    fn visit_path_expr(&mut self, _path: &'ast PathExpr) {
        unreachable!();
    }

    fn visit_ty_path(&mut self, _path: &'ast TyPath) {
        unreachable!();
    }

    fn visit_path(&mut self, _: &'ast Path) {
        unreachable!()
    }

    // fn visit_dot_op_expr(&mut self, lhs: &'ast PR<N<Expr>>, field: &'ast
    // PR<IdentNode>) {     walk_pr!(self, lhs, visit_expr);
    //     let field = field.as_ref().unwrap();
    //     let res = self.resolve_member(&field.ident);
    //     self.sess.res.set(NamePath::new(field.id()), res);
    // }

    // FIXME: Should resolve to field accessor NodeId
    // fn visit_dot_op_expr(&mut self, lhs: &'ast PR<N<Expr>>, field: &'ast
    // PR<IdentNode>) {     walk_pr!(self, lhs, visit_expr);
    //     let field = field.as_ref().unwrap();
    //     let res = self.resolve_member(&field.ident);
    //     self.sess.res.set(NamePath::new(field.id()), res);
    // }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr.kind() {
            ExprKind::Lit(_) => {},
            ExprKind::Paren(inner) => walk_pr!(self, inner, visit_expr),
            ExprKind::Path(path) => {
                if let Ok(path) = &path.0 {
                    let search_for_kind = match path.target_name().kind() {
                        Some(kind) => match kind {
                            IdentKind::Var => SearchForKind::FromNs(Namespace::Value),
                            IdentKind::Ty => SearchForKind::Def(DefKind::Ctor),
                            IdentKind::Op => SearchForKind::Specific(SearchForSpecific::Op),
                        },
                        None => SearchForKind::Def(DefKind::Value),
                    };
                    self.resolve_path(path, search_for_kind);
                } else {
                    walk_pr!(
                        self,
                        &path.0,
                        resolve_path,
                        SearchForKind::FromNs(Namespace::Value)
                    );
                }
            },
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => {
                if let Some(path) = Self::match_path_expr(&call.lhs) {
                    walk_each_pr!(self, &call.args, visit_expr);

                    let search_for_kind = match path.target_name().kind() {
                        Some(kind) => match kind {
                            IdentKind::Var => SearchForKind::Def(DefKind::Func),
                            IdentKind::Ty => SearchForKind::Def(DefKind::Ctor),
                            IdentKind::Op => SearchForKind::Specific(SearchForSpecific::Op),
                        },
                        None => SearchForKind::Def(DefKind::Func),
                    };
                    self.resolve_path(path, search_for_kind);
                } else {
                    self.visit_app_expr(call);
                }
            },
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => {
                self.visit_type_expr(ty_expr);
            },
            ExprKind::DotOp(lhs, field) => self.visit_dot_op_expr(lhs, field),
            ExprKind::Match(subject, arms) => self.visit_match_expr(subject, arms),
        }
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        match ty.kind() {
            TyKind::Path(path) => {
                if let Ok(path) = &path.0 {
                    let search_for_kind = match path.target_name().kind() {
                        Some(kind) => match kind {
                            IdentKind::Var => SearchForKind::Def(DefKind::TyParam),
                            IdentKind::Op | IdentKind::Ty => SearchForKind::FromNs(Namespace::Type),
                        },
                        None => SearchForKind::FromNs(Namespace::Type),
                    };
                    self.resolve_path(path, search_for_kind);
                } else {
                    walk_pr!(
                        self,
                        &path.0,
                        resolve_path,
                        SearchForKind::FromNs(Namespace::Type)
                    );
                }
            },
            TyKind::Func(params, body) => self.visit_func_ty(params, body),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
            TyKind::App(cons, args) => self.visit_ty_app(cons, args),
            TyKind::AppExpr(cons, const_arg) => self.visit_ty_app_expr(cons, const_arg),
        }
    }
}

impl<'ast> Stage<()> for NameResolver<'ast> {
    fn run(mut self) -> StageResult<()> {
        self.enter_module_scope(ROOT_MODULE_ID);
        self.visit_ast(self.ast);
        stage_result(self.sess, (), self.msg)
    }
}

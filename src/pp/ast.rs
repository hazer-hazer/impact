use super::AstLikePP;
use crate::{
    ast::{
        expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, TyExpr},
        item::{ExternItem, Field, GenericParams, Item, ItemKind, TyParam, Variant},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        visitor::{walk_pr, AstVisitor},
        ErrorNode, IdentNode, NodeId, Path, WithNodeId, AST, N, PR,
    },
    parser::token::{Op, Punct},
    span::sym::{Ident, Kw},
};

macro_rules! walk_block {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {{
        $self.indent();
        $prs.iter().for_each(|pr| {
            $self.out_indent();
            walk_pr!($self, pr, $ok_visitor);
            $self.nl();
        });
        $self.dedent();
    }};
}

// Would be nice if intersperse was stable
macro_rules! walk_each_pr_delim {
    ($self: ident, $prs: expr, $ok_visitor: ident, $sep: expr) => {
        $prs.iter().enumerate().for_each(|(index, pr)| {
            walk_pr!($self, pr, $ok_visitor);
            if index < $prs.len() - 1 {
                $self.str($sep);
            }
        })
    };
}

impl<'ast> AstVisitor<'ast> for AstLikePP<'ast, ()> {
    fn visit_err(&mut self, err: &'ast ErrorNode) {
        if let Some(parsed) = err.parsed() {
            self.string(parsed);
            self.sp();
        }

        self.str("[ERROR]");
    }

    fn visit_ast(&mut self, ast: &'ast AST) {
        walk_each_pr_delim!(self, ast.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => walk_pr!(self, expr, visit_expr),
            StmtKind::Item(item) => walk_pr!(self, item, visit_item),
        }
        self.node_id(stmt);
    }

    // Items //
    fn visit_item(&mut self, item: &'ast Item) {
        match item.kind() {
            ItemKind::Type(name, generics, ty) => self.visit_ty_item(name, generics, ty, item.id()),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
            ItemKind::Adt(name, generics, variants) => {
                self.visit_adt_item(name, generics, variants, item.id())
            },
            ItemKind::Struct(name, generics, fields) => {
                self.visit_struct_item(name, generics, fields)
            },
            ItemKind::Extern(items) => self.visit_extern_block(items),
        }
        self.node_id(item);
    }

    fn visit_generic_params(&mut self, generics: &'ast GenericParams) {
        if !generics.ty_params.is_empty() {
            self.sp();
        }
        walk_each_pr_delim!(self, &generics.ty_params, visit_ty_param, " ");
    }

    fn visit_ty_param(&mut self, ty_param: &'ast TyParam) {
        walk_pr!(self, &ty_param.name, name, ty_param.id(), true);
    }

    fn visit_ty_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast GenericParams,
        ty: &'ast PR<N<Ty>>,
        id: NodeId,
    ) {
        self.kw(Kw::Type);
        walk_pr!(self, name, name, id, true);
        self.visit_generic_params(generics);
        self.op(Op::Assign);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &'ast PR<Ident>, items: &'ast Vec<PR<N<Item>>>, id: NodeId) {
        self.kw(Kw::Mod);
        walk_pr!(self, name, name, id, true);
        self.nl();
        walk_block!(self, items, visit_item);
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, name, id, true);
        if !params.is_empty() {
            self.sp();
        }
        walk_each_pr_delim!(self, params, visit_pat, " ");
        self.op(Op::Assign);
        walk_pr!(self, body, visit_expr);
    }

    fn visit_adt_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast GenericParams,
        variants: &'ast [PR<Variant>],
        id: NodeId,
    ) {
        self.kw(Kw::Data);
        walk_pr!(self, name, name, id, true);
        self.visit_generic_params(generics);
        self.op(Op::Assign);
        walk_each_pr_delim!(self, variants, visit_variant, " | ");
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        walk_pr!(self, &variant.name, name, variant.id, true);
        self.sp();
        walk_each_pr_delim!(self, &variant.fields, visit_field, " ");
    }

    fn visit_field(&mut self, field: &'ast Field) {
        field.name.as_ref().map(|name| {
            walk_pr!(self, name, name, field.id, true);
            self.punct(Punct::Colon);
        });
        walk_pr!(self, &field.ty, visit_ty);
    }

    fn visit_extern_block(&mut self, items: &'ast [PR<ExternItem>]) {
        self.kw(Kw::Extern);
        self.indent();
        walk_each_pr_delim!(self, items, visit_extern_item, "\n");
        self.dedent();
    }

    fn visit_extern_item(&mut self, item: &'ast ExternItem) {
        walk_pr!(self, &item.name, visit_ident);
        self.punct(Punct::Colon);
        walk_pr!(self, &item.ty, visit_ty);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &'ast Pat) {
        match pat.kind() {
            PatKind::Unit => {
                self.kw(Kw::Unit);
            },
            PatKind::Ident(ident) => {
                walk_pr!(self, ident, name, pat.id(), true);
            },
        }
        self.node_id(pat);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => {
                self.ch('(');
                walk_pr!(self, inner, visit_expr);
                self.ch(')');
            },
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
            ExprKind::DotOp(expr, field) => self.visit_dot_op_expr(expr, field),
        }
        self.node_id(expr);
    }

    fn visit_lit_expr(&mut self, lit: &'ast Lit) {
        self.string(lit);
    }

    fn visit_infix_expr(&mut self, infix: &'ast Infix) {
        walk_pr!(self, &infix.lhs, visit_expr);
        self.sp();
        self.visit_path_expr(&infix.op);
        self.sp();
        walk_pr!(self, &infix.rhs, visit_expr);
    }

    fn visit_lambda_expr(&mut self, lambda: &'ast Lambda) {
        self.punct(Punct::Backslash);
        walk_each_pr_delim!(self, &lambda.params, visit_pat, " ");
        self.punct(Punct::Arrow);
        walk_pr!(self, &lambda.body, visit_expr);
    }

    fn visit_app_expr(&mut self, call: &'ast Call) {
        walk_pr!(self, &call.lhs, visit_expr);
        self.sp();
        walk_each_pr_delim!(self, &call.args, visit_expr, " ");
    }

    fn visit_let_expr(&mut self, block: &'ast PR<Block>) {
        self.kw(Kw::Let);
        walk_pr!(self, block, visit_block);
    }

    fn visit_type_expr(&mut self, ty_expr: &'ast TyExpr) {
        walk_pr!(self, &ty_expr.expr, visit_expr);
        self.punct(Punct::Colon);
        walk_pr!(self, &ty_expr.ty, visit_ty);
    }

    fn visit_dot_op_expr(&mut self, expr: &'ast PR<Box<Expr>>, field: &'ast PR<IdentNode>) {
        walk_pr!(self, expr, visit_expr);
        self.punct(Punct::Dot);
        walk_pr!(self, field, visit_ident_node);
    }

    // Types //
    fn visit_ty(&mut self, ty: &'ast Ty) {
        match ty.kind() {
            TyKind::Path(path) => self.visit_ty_path(path),
            TyKind::Func(params, return_ty) => self.visit_func_ty(params, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
            TyKind::App(cons, arg) => self.visit_ty_app(cons, arg),
            TyKind::AppExpr(cons, const_arg) => self.visit_ty_app_expr(cons, const_arg),
        }
        self.node_id(ty);
    }

    fn visit_func_ty(&mut self, params: &'ast [PR<N<Ty>>], body: &'ast PR<N<Ty>>) {
        walk_each_pr_delim!(self, params, visit_ty, " - ");
        self.punct(Punct::Arrow);
        walk_pr!(self, body, visit_ty);
    }

    fn visit_paren_ty(&mut self, inner: &'ast PR<N<Ty>>) {
        self.punct(Punct::LParen);
        walk_pr!(self, inner, visit_ty);
        self.punct(Punct::RParen);
    }

    fn visit_ty_app(&mut self, cons: &'ast PR<N<Ty>>, args: &'ast [PR<N<Ty>>]) {
        walk_pr!(self, cons, visit_ty);
        self.sp();
        walk_each_pr_delim!(self, args, visit_ty, " ");
    }

    fn visit_ty_app_expr(&mut self, cons: &'ast PR<N<Ty>>, args: &'ast [PR<N<Expr>>]) {
        walk_pr!(self, cons, visit_ty);
        self.sp();
        walk_each_pr_delim!(self, args, visit_expr, " ");
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &'ast Ident) {
        self.ident(ident);
    }

    fn visit_ident_node(&mut self, ident: &'ast IdentNode) {
        self.name(&ident.ident, ident.id, false);
    }

    fn visit_path(&mut self, path: &'ast Path) {
        self.path(path)
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.nl();
        walk_block!(self, block.stmts(), visit_stmt);
    }
}

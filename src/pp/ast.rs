use crate::{
    ast::{
        expr::{Block, Expr, InfixOp, Lit, PrefixOp},
        item::Item,
        stmt::{Stmt, StmtKind},
        ty::{LitTy, Ty},
        visitor::visit_pr,
        visitor::AstVisitor,
        ErrorNode, Path, AST, N, PR,
    },
    span::span::Ident,
};

use super::AstLikePP;

macro_rules! visit_block {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {{
        $self.indent();
        let string = $prs
            .iter()
            .map(|pr| {
                format!(
                    "{}{}",
                    $self.cur_indent(),
                    visit_pr!($self, pr, $ok_visitor)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        $self.dedent();
        string
    }};
}

macro_rules! visit_pr_vec {
    ($self: ident, $prs: expr, $ok_visitor: ident, $sep: expr) => {
        $prs.iter()
            .map(|pr| visit_pr!($self, pr, $ok_visitor))
            .collect::<Vec<_>>()
            .join($sep)
    };
}

impl<'a> AstVisitor<String> for AstLikePP<'a> {
    fn visit_err(&self, _: &ErrorNode) -> String {
        "[ERROR]".to_string()
    }

    fn visit_ast(&mut self, ast: &AST) -> String {
        println!("=== AST ===");
        visit_pr_vec!(self, ast.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        format!(
            "{}{}",
            self.cur_indent(),
            match stmt.kind() {
                StmtKind::Expr(expr) => visit_pr!(self, expr, visit_expr),
                StmtKind::Item(item) => visit_pr!(self, item, visit_item),
            }
        )
    }

    // Items //
    fn visit_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) -> String {
        format!(
            "type {} = {}",
            visit_pr!(self, name, visit_ident),
            visit_pr!(self, ty, visit_ty)
        )
    }

    fn visit_mod_item(&mut self, name: &PR<Ident>, items: &Vec<PR<N<Item>>>) -> String {
        format!(
            "mod {}\n{}",
            visit_pr!(self, name, visit_ident),
            visit_block!(self, items, visit_item)
        )
    }

    fn visit_decl_item(
        &mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Ident>>,
        body: &PR<N<Expr>>,
    ) -> String {
        format!(
            "{} {} = {}",
            visit_pr!(self, name, visit_ident),
            visit_pr_vec!(self, params, visit_ident, " "),
            visit_pr!(self, body, visit_expr)
        )
    }

    // Expressions //
    fn visit_lit_expr(&mut self, lit: &Lit) -> String {
        format!("{}", lit)
    }

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) -> String {
        format!(
            "{} {} {}",
            visit_pr!(self, lhs, visit_expr),
            op,
            visit_pr!(self, rhs, visit_expr)
        )
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> String {
        format!("{}{}", op, visit_pr!(self, rhs, visit_expr))
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> String {
        format!(
            "\\{} -> {}",
            visit_pr!(self, param, visit_ident),
            visit_pr!(self, body, visit_expr)
        )
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> String {
        format!(
            "{} {}",
            visit_pr!(self, lhs, visit_expr),
            visit_pr!(self, arg, visit_expr)
        )
    }

    fn visit_let_expr(&mut self, block: &PR<Block>) -> String {
        format!("let {}", visit_pr!(self, block, visit_block))
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> String {
        format!(
            "{}: {}",
            visit_pr!(self, expr, visit_expr),
            visit_pr!(self, ty, visit_ty)
        )
    }

    // Types //
    fn visit_unit_ty(&mut self) -> String {
        "()".to_string()
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> String {
        format!("{}", lit_ty)
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> String {
        format!(
            "{} -> {}",
            visit_pr!(self, param_ty, visit_ty),
            visit_pr!(self, return_ty, visit_ty)
        )
    }

    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) -> String {
        format!("({})", visit_pr!(self, inner, visit_ty))
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> String {
        ident.to_string()
    }

    fn visit_path(&mut self, path: &Path) -> String {
        format!(
            "{}",
            path.segments()
                .iter()
                .map(|seg| format!("{}", seg))
                .collect::<Vec<_>>()
                .join(".")
        )
    }

    fn visit_block(&mut self, block: &Block) -> String {
        format!(
            "{}\n{}",
            visit_block!(self, block.stmts(), visit_stmt),
            visit_pr!(self, block.expr(), visit_expr)
        )
    }
}

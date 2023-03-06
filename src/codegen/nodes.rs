use inkwell::values::{BasicValueEnum, CallableValue};

use crate::hir::{
    expr::{BlockNode, Call, ExprKind, ExprNode, Lambda, Lit, PathExpr},
    item::ItemNode,
    stmt::{StmtKind, StmtNode},
    PathNode,
};

use super::codegen::CodeGen;

pub type CodeGenResult<'g, T = Option<BasicValueEnum<'g>>> = Result<T, ()>;

pub trait NodeCodeGen<'g> {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g>;
}

impl<'g> NodeCodeGen<'g> for StmtNode {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g> {
        match self.kind() {
            &StmtKind::Expr(expr) => g.hir.expr(expr).codegen(g),
            &StmtKind::Item(item) => g.hir.item(item).codegen(g),
        }
    }
}

impl<'g> NodeCodeGen<'g> for PathNode {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g> {
        Ok(Some(g.expect_res_value(self.res())))
    }
}

impl<'g> NodeCodeGen<'g> for ExprNode {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g> {
        Ok(Some(match self.kind() {
            ExprKind::Lit(lit) => match lit {
                Lit::Bool(val) => g.bool_value(*val),

                // FIXME: Assert int/float kinds eq to types
                Lit::Int(val, _kind) => g.int_value(*val, g.tyctx().tyof(self.id()).as_int_kind()),
                Lit::Float(val, _kind) => {
                    g.float_value(*val, g.tyctx().tyof(self.id()).as_float_kind())
                },

                Lit::String(sym) => g.string_value(sym.as_str()),
            },
            &ExprKind::Path(PathExpr(path)) => g.hir.path(path).codegen(g)?.unwrap(),
            &ExprKind::Block(block) => g.hir.block(block).codegen(g)?.unwrap(),
            &ExprKind::Lambda(Lambda { body: _ }) => {
                todo!()
                // let name = g.get_lambda_name(self.id());
                // let ty = g.tyctx().tyof(self.id());

                // return g.function(name.as_str(), ty, |g, func| {
                //     g.bind_res_value(Res::Node(param), func.get_nth_param(0).unwrap());
                //     g.hir.expr(body).codegen(g)
                // });
            },
            &ExprKind::Call(Call { lhs, arg }) => {
                let func = g.hir.expr(lhs).codegen(g)?.unwrap().into_pointer_value();
                let arg = g.hir.expr(arg).codegen(g)?.unwrap();

                let func = CallableValue::try_from(func).unwrap();
                g.builder()
                    .build_call(func, &[arg.into()], "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            },
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(_) => todo!(),
            ExprKind::BuiltinExpr(_) => todo!(),
        }))
    }
}

impl<'g> NodeCodeGen<'g> for BlockNode {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g> {
        assert!(!self.stmts().is_empty() || self.expr().is_some());

        self.stmts().iter().try_for_each(|&stmt| {
            g.hir.stmt(stmt).codegen(g)?;
            Ok(())
        })?;

        if let Some(&expr) = self.expr() {
            g.hir.expr(expr).codegen(g)
        } else {
            Ok(Some(g.unit_value()))
        }
    }
}

impl<'g> NodeCodeGen<'g> for ItemNode {
    fn codegen(&self, g: &mut CodeGen<'g>) -> CodeGenResult<'g> {
        // match self.kind() {
        //     &ItemKind::Decl(Decl { value }) => {
        //         g.under_def(
        //             (self.name(), self.def_id(), value),
        //             |g| -> CodeGenResult<'g, ()> {
        //                 let val = match g.sess.def_table.get_def(self.def_id()).unwrap().kind() {
        //                     DefKind::DeclareBuiltin => Ok(None),
        //                     // &DefKind::Builtin(bt) => builtin(g, bt),
        //                     DefKind::Func | DefKind::Var => {
        //                         Ok(Some(g.hir.expr(value).codegen(g)?.unwrap()))
        //                     },

        //                     _ => unreachable!(),
        //                 }?;

        //                 if let Some(val) = val {
        //                     g.bind_res_value(Res::Node(HirId::new_owner(self.def_id())), val);
        //                 }

        //                 Ok(())
        //             },
        //         )?;
        //     },
        //     ItemKind::Mod(_) | ItemKind::TyAlias(_) => {},
        // }

        Ok(Some(g.unit_value()))
    }
}

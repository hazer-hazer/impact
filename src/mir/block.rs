use crate::mir::build::unpack;

use super::{
    build::MirBuilder,
    thir::{BlockId, Stmt},
    BBWith, LValue, BB,
};

impl<'ctx> MirBuilder<'ctx> {
    pub(super) fn block(&mut self, mut bb: BB, block_id: BlockId, dest: LValue) -> BBWith<()> {
        let block = self.thir.block(block_id);
        let expr = block.expr;

        for stmt in block.stmts.clone() {
            let stmt = self.thir.stmt(stmt);
            match stmt {
                &Stmt::Expr(expr_id) => unpack!(bb = self.expr_stmt(bb, expr_id)),
                &Stmt::Local(pat, init) => {
                    self.declare_bindings(&pat, false);
                    unpack!(bb = self.store_expr_in_pat(bb, &pat, init));
                },
            }
        }

        if let Some(expr) = expr {
            unpack!(bb = self.store_expr(bb, dest, expr));
        } else {
            let dest_ty = self.builder.local_info(dest.local).ty;
            if dest_ty.is_unit() {
                self.push_assign_unit(bb, dest)
            }
        }

        bb.unit()
    }
}

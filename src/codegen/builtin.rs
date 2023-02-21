use inkwell::{
    attributes::{Attribute, AttributeLoc},
    values::BasicValue,
};

use crate::{
    resolve::{builtin::Builtin, res::Res},
    typeck,
};

use super::{codegen::CodeGen, nodes::CodeGenResult};

pub fn builtin<'ctx>(g: &mut CodeGen<'ctx>, bt: Builtin) -> CodeGenResult<'ctx> {
    // FIXME: Wtf types are doing here?
    let ty = typeck::builtin::builtins()[&bt];

    match bt {
        // Operator functions //
        Builtin::Add => g.function(bt.name(), ty, |g, func| {
            let a = func.get_nth_param(0).unwrap().into_int_value();
            let b = func.get_nth_param(1).unwrap().into_int_value();

            Ok(Some(
                g.builder().build_int_add(a, b, "add").as_basic_value_enum(),
            ))
        }),

        Builtin::Minus => todo!(),

        // Values //
        Builtin::UnitValue => return Ok(Some(g.unit_value())),

        // Types //
        Builtin::I32 | Builtin::UnitTy => Ok(None),
    }
}

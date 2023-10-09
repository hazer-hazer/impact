use super::{
    build::{unpack, MirBuilder},
    thir::{Pat, PatKind},
    Operand, SwitchTargets, SwitchValue, BB,
};

/// How pattern in match expression is matched against expression.
///
/// Let's start with primitive types.
/// 1. `bool` (0/1), integers of different size, `char` (C char, not UTF8, just
///    a byte)
/// Switch value is just an int of some size, now u64, just match value
/// against pattern. Pretty simple.
///
/// 2. Structs and sequential types.
/// Matched by canonical position, i.e. for array it is pattern index = array
/// index, for structs it is field index (named fields must have canonical
/// order) from type (for pattern) and from value.
///
/// 3. Irrefutable patterns.
/// Actually, some patterns are irrefutable and we don't need to do any check if
/// it is type-checked correctly (and in MIR building we assume that typeck is
/// valid). Irrefutable patterns are:
/// - Any unit struct (including `()`) pattern, because if unit instance is
///   created then there're no more options than this one unit value.
/// - Wildcard pattern, i.e. `_`.
/// - Ident pattern.

impl<'ctx> MirBuilder<'ctx> {
    pub fn pat_to_switch(
        &self,
        mut bb: BB,
        subject: Operand,
        pat: &Pat,
        target: BB,
    ) -> SwitchTargets {
        match pat.kind {
            // Irrefutable
            PatKind::Unit => {
                todo!()
            },
            PatKind::Ident { name, var, ty } => todo!(),
        }
    }
}

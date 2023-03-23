mod block;
pub mod build;
mod expr;
mod local;
pub mod scalar;
mod stmt;
mod thir;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::{declare_idx, IndexVec},
    hir::BodyId,
    resolve::{builtin::Builtin, def::DefId},
    span::span::Span,
};

use self::scalar::Scalar;

pub use crate::typeck::ty::Ty;

declare_idx!(Local, u32, "_{}", Color::White);
declare_idx!(BB, u32, "bb{}", Color::White);

impl Local {
    pub fn lvalue(&self) -> LValue {
        LValue::new(*self)
    }

    pub fn return_local() -> Local {
        Local::new(0)
    }
}

pub const START_BB: BB = BB::new(0);

pub struct BBWith<T>(BB, T);

impl<T> BBWith<T> {
    pub fn mapped<U>(&self, mut map: impl FnMut(&T) -> U) -> BBWith<U> {
        BBWith(self.0, map(&self.1))
    }
}

impl BB {
    pub fn with<T>(&self, with: T) -> BBWith<T> {
        BBWith(*self, with)
    }

    pub fn unit(&self) -> BBWith<()> {
        BBWith(*self, ())
    }
}

#[derive(Clone, Copy)]
pub struct LValue {
    pub local: Local,
}

impl LValue {
    pub fn new(local: Local) -> Self {
        Self { local }
    }

    pub fn operand(&self) -> Operand {
        Operand::LValue(*self)
    }

    pub fn return_lvalue() -> Self {
        Self {
            local: Local::return_local(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum ConstKind {
    Scalar(Scalar),
    ZeroSized,
}

#[derive(Clone, Copy)]
pub struct Const {
    pub ty: Ty,
    pub kind: ConstKind,
}

impl Const {
    pub fn scalar(ty: Ty, scalar: Scalar) -> Self {
        Self {
            ty,
            kind: ConstKind::Scalar(scalar),
        }
    }

    pub fn zero_sized(ty: Ty) -> Self {
        Self {
            ty,
            kind: ConstKind::ZeroSized,
        }
    }

    pub fn operand(&self) -> Operand {
        Operand::Const(*self)
    }
}

#[derive(Clone, Copy)]
pub enum Operand {
    LValue(LValue),
    Const(Const),
}

impl Operand {
    pub fn rvalue(&self) -> RValue {
        RValue::Operand(*self)
    }

    pub fn as_lvalue(&self) -> &LValue {
        match self {
            Operand::LValue(lvalue) => lvalue,
            _ => panic!(),
        }
    }
}

/// Only builtin infix operators, all overloaded are function calls
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum InfixOp {
    AddInt,
    SubInt,
}

impl InfixOp {
    pub fn each() -> impl Iterator<Item = InfixOp> {
        [Self::AddInt, Self::SubInt].into_iter()
    }

    pub fn func_ty(&self) -> Ty {
        match self {
            InfixOp::AddInt | InfixOp::SubInt => Ty::func(
                None,
                Ty::default_int(),
                Ty::func(None, Ty::default_int(), Ty::default_int()),
            ),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            InfixOp::AddInt => "add_int",
            InfixOp::SubInt => "sub_int",
        }
    }
}

impl TryFrom<Builtin> for InfixOp {
    type Error = ();

    fn try_from(value: Builtin) -> Result<Self, Self::Error> {
        match value {
            Builtin::AddInt => Ok(InfixOp::AddInt),
            Builtin::SubInt => Ok(InfixOp::SubInt),
            _ => Err(()),
        }
    }
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InfixOp::AddInt => "+",
                InfixOp::SubInt => "-",
            }
        )
    }
}

#[derive(Clone, Copy)]
pub enum RValue {
    Operand(Operand),
    Infix(InfixOp),

    // TODO: Upvars
    Closure(DefId),

    FuncRef(DefId, Ty),
    ClosureRef(DefId),

    ValueRef(DefId),

    // TODO: Maybe move to terminator only when doing algebraic effects.
    Call {
        lhs: Operand,
        arg: Operand,
        // target: BB,
    },
}

#[derive(Clone, Copy)]
pub enum StmtKind {
    Assign(LValue, RValue),
}

#[derive(Clone, Copy)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Self { kind }
    }
}

#[derive(Clone, Copy)]
pub enum TerminatorKind {
    Goto(BB),
    Return,
}

#[derive(Clone, Copy)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

pub struct BasicBlock {
    pub stmts: Vec<Stmt>,
    pub terminator: Terminator,
}

pub struct LocalInfo {
    pub ty: Ty,
    pub span: Span,
}

impl LocalInfo {
    pub fn new(ty: Ty, span: Span) -> Self {
        Self { ty, span }
    }
}

pub struct Body {
    pub basic_blocks: IndexVec<BB, BasicBlock>,
    pub args: usize,
    pub locals: IndexVec<Local, LocalInfo>,
    pub referenced_bodies: HashSet<BodyId>,
}

impl Body {
    pub fn bb(&self, bb: BB) -> &BasicBlock {
        self.basic_blocks.get(bb).unwrap()
    }

    pub fn params(&self) -> impl Iterator<Item = (Local, &LocalInfo)> {
        self.locals.iter_enumerated().skip(1).take(self.args)
    }

    /// Get locals after return (0) and parameters
    pub fn inner_locals(&self) -> impl Iterator<Item = (Local, &LocalInfo)> {
        self.locals.iter_enumerated().skip(self.args + 1)
    }

    // pub fn local_name(&self, local: Local) ->

    pub fn args(&self) -> usize {
        self.args
    }
}

#[derive(Default)]
pub struct BasicBlockBuilder {
    stmts: Vec<Stmt>,
    terminator: Option<Terminator>,
}

impl BasicBlockBuilder {
    fn push_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    fn terminate(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator)
    }

    fn emit(self) -> BasicBlock {
        BasicBlock {
            stmts: self.stmts,
            terminator: self
                .terminator
                .expect("Cannot create BasicBlock without terminator"),
        }
    }
}

/// BODYBUILDER LOL
#[derive(Default)]
struct BodyBuilder {
    basic_blocks: IndexVec<BB, BasicBlockBuilder>,
    locals: IndexVec<Local, LocalInfo>,
    args: usize,
    referenced_bodies: HashSet<BodyId>,
}

impl BodyBuilder {
    pub fn begin_bb(&mut self) -> BB {
        self.basic_blocks.push(BasicBlockBuilder::default())
    }

    pub fn end_bb(&mut self, bb: BB, terminator: Terminator) {
        self.bb_mut(bb).terminate(terminator);
    }

    fn bb(&self, bb: BB) -> &BasicBlockBuilder {
        self.basic_blocks.get(bb).as_ref().unwrap()
    }

    fn bb_mut(&mut self, bb: BB) -> &mut BasicBlockBuilder {
        self.basic_blocks.get_mut(bb).unwrap()
    }

    pub fn push_stmt(&mut self, bb: BB, stmt: Stmt) {
        self.bb_mut(bb).push_stmt(stmt);
    }

    pub fn terminate(&mut self, bb: BB, terminator: Terminator) {
        self.bb_mut(bb).terminate(terminator);
    }

    pub fn terminate_return(&mut self, bb: BB) {
        self.terminate(
            bb,
            Terminator {
                kind: TerminatorKind::Return,
            },
        )
    }

    pub fn goto(&mut self, bb: BB, goto: BB) {
        self.terminate(
            bb,
            Terminator {
                kind: TerminatorKind::Goto(goto),
            },
        )
    }

    pub fn push_local(&mut self, is_param: bool, info: LocalInfo) -> Local {
        if is_param {
            self.args += 1;
        }
        self.locals.push(info)
    }

    pub fn local_info(&mut self, local: Local) -> &LocalInfo {
        self.locals.get(local).unwrap()
    }

    pub fn references_body(&mut self, body_id: BodyId) {
        self.referenced_bodies.insert(body_id);
    }

    pub fn emit(self) -> Body {
        // TODO: Check that emitted blocks vec is decent?
        Body {
            basic_blocks: self
                .basic_blocks
                .into_iter()
                .map(|bb| bb.emit())
                .collect::<IndexVec<_, _>>(),
            locals: self.locals,
            args: self.args,
            referenced_bodies: self.referenced_bodies,
        }
    }
}

#[derive(Default)]
pub struct MIR {
    pub bodies: HashMap<BodyId, Body>,
}

impl MIR {
    pub fn expect(&self, body_id: BodyId) -> &Body {
        self.bodies.get(&body_id).unwrap()
    }
}

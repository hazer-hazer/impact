mod block;
pub mod build;
mod expr;
mod local;
mod scalar;
mod stmt;
mod thir;

use std::{
    collections::{HashMap, HashSet},
    default,
    fmt::Display,
};

use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::{declare_idx, IndexVec},
    hir::BodyId,
    resolve::def::DefId,
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
}

/// Only builtin infix operators, all overloaded are function calls
#[derive(Clone, Copy)]
pub enum InfixOp {
    AddInt,
    SubInt,
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
    Infix(Operand, InfixOp, Operand),

    // TODO: Upvars
    Closure(DefId),

    // TODO: Maybe move to terminator only when doing algebraic effects.
    Call {
        lhs: Operand,
        arg: Operand,
        target: BB,
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
    pub locals: IndexVec<Local, LocalInfo>,
    pub referenced_bodies: HashSet<BodyId>,
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
            terminator: self.terminator.unwrap(),
        }
    }
}

/// BODYBUILDER LOL
#[derive(Default)]
struct BodyBuilder {
    basic_blocks: IndexVec<BB, BasicBlockBuilder>,
    locals: IndexVec<Local, LocalInfo>,
    referenced_bodies: HashSet<BodyId>,
}

impl BodyBuilder {
    pub fn begin_bb(&mut self) -> BB {
        self.basic_blocks.push(BasicBlockBuilder::default())
    }

    pub fn end_bb(&mut self, bb: BB, terminator: Terminator) {
        self.basic_blocks.get_mut(bb).unwrap().terminate(terminator);
    }

    pub fn push_stmt(&mut self, bb: BB, stmt: Stmt) {
        self.basic_blocks.get_mut(bb).unwrap().push_stmt(stmt);
    }

    pub fn terminate(&mut self, bb: BB, terminator: Terminator) {
        self.basic_blocks.get_mut(bb).unwrap().terminate(terminator);
    }

    pub fn push_local(&mut self, local_info: LocalInfo) -> Local {
        self.locals.push(local_info)
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
            referenced_bodies: self.referenced_bodies,
        }
    }
}

#[derive(Default)]
pub struct MIR {
    pub bodies: HashMap<BodyId, Body>,
}

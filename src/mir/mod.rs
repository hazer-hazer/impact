mod build;
mod scalar;

use std::iter::Map;

use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::{declare_idx, IndexVec},
};

use self::scalar::Scalar;

declare_idx!(Local, u32, "_{}", Color::White);
declare_idx!(BB, u32, "bb{}", Color::White);

impl Local {
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
    local: Local,
}

impl LValue {
    pub fn new(local: Local) -> Self {
        Self { local }
    }

    pub fn return_lvalue() -> Self {
        Self {
            local: Local::return_local(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Const {
    Scalar(Scalar),
}

#[derive(Clone, Copy)]
pub enum Operand {
    LValue(LValue),
    Const(Const),
}

/// Only builtin infix operators, all overloaded are function calls
#[derive(Clone, Copy)]
pub enum InfixOp {
    AddInt,
    SubInt,
}

#[derive(Clone, Copy)]
pub enum RValue {
    Operand(Operand),
    Infix(Operand, InfixOp, Operand),

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
    kind: StmtKind,
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
    kind: TerminatorKind,
}

pub struct BasicBlock {
    stmts: Vec<Stmt>,
    terminator: Terminator,
}

pub struct LocalInfo {}

pub struct Body {
    basic_blocks: IndexVec<BB, BasicBlock>,
    locals: IndexVec<Local, LocalInfo>,
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

    pub fn local(&mut self, local_info: LocalInfo) -> Local {
        self.locals.push(local_info)
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
        }
    }
}

pub mod build;

use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::{declare_idx, IndexVec},
    hir::{BodyId, BodyOwnerKind, HirId, OwnerId},
    resolve::builtin::Builtin,
    span::span::{Ident, Span, Symbol, WithSpan},
    typeck::ty::{FloatKind, IntKind, Ty},
};

declare_idx!(ExprId, u32, "{}", Color::Green);
declare_idx!(BlockId, u32, "{}", Color::Blue);
declare_idx!(StmtId, u32, "{}", Color::Yellow);
declare_idx!(ParamId, u32, "{}", Color::White);
declare_idx!(wrapper LocalVar, HirId, "{}", Color::White);

pub enum Stmt {
    Expr(ExprId),
    Local(Pat, ExprId),
}

pub struct Block {
    pub stmts: Vec<StmtId>,
    pub expr: Option<ExprId>,
    // pub span: Span,
}

pub enum Lit {
    Bool(bool),
    Int(u64, IntKind),
    Float(f64, FloatKind),
    String(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum ExprCategory {
    Const,
    LValue,
    RValue,
}

pub enum ExprKind {
    Lit(Lit),
    LocalRef(LocalVar),
    Block(BlockId),
    Call {
        func_ty: Ty,
        lhs: ExprId,
        arg: ExprId,
    },
    Lambda {
        body_id: BodyId,
    },
    Ty(ExprId, Ty),
    Builtin(Builtin),
}

pub struct Expr {
    pub ty: Ty,
    pub kind: ExprKind,
    pub span: Span,
}

impl WithSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

impl Expr {
    pub fn categorize(&self) -> ExprCategory {
        match self.kind {
            ExprKind::Lit(_) => ExprCategory::Const,
            ExprKind::LocalRef(_) => ExprCategory::LValue,
            // FIXME: Ascription is an lvalue?
            ExprKind::Ty(_, _) => ExprCategory::LValue,
            ExprKind::Call { .. } | ExprKind::Lambda { .. } | ExprKind::Block(_) => {
                ExprCategory::RValue
            },
            ExprKind::Builtin(_) => todo!(),
        }
    }
}

pub enum PatKind {
    Unit,
    Ident(Ident, LocalVar),
}

pub struct Pat {
    pub ty: Ty,
    pub kind: PatKind,
    pub span: Span,
}

pub struct Param {
    pub pat: Pat,
    // ty: Ty,
    // hir_id: HirId,
}

/// THIR is built temporarily for Body of a function when MIR is built.
pub struct THIR {
    body_owner: OwnerId,
    stmts: IndexVec<StmtId, Stmt>,
    exprs: IndexVec<ExprId, Expr>,
    blocks: IndexVec<BlockId, Block>,
    params: IndexVec<ParamId, Param>,
}

impl THIR {
    pub fn new(body_owner: OwnerId) -> Self {
        Self {
            body_owner,
            stmts: Default::default(),
            exprs: Default::default(),
            blocks: Default::default(),
            params: Default::default(),
        }
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt {
        self.stmts.get(id).unwrap()
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        self.exprs.get(id).unwrap()
    }

    pub fn block(&self, id: BlockId) -> &Block {
        self.blocks.get(id).unwrap()
    }

    pub fn param(&self, id: ParamId) -> &Param {
        self.params.get(id).unwrap()
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.stmts.push(stmt)
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        self.exprs.push(expr)
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        self.blocks.push(block)
    }

    pub fn add_param(&mut self, param: Param) -> ParamId {
        self.params.push(param)
    }

    pub fn body_owner(&self) -> OwnerId {
        self.body_owner
    }
}
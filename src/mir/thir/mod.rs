pub mod build;

use std::fmt::Display;

use crate::{
    cli::color::{Color, ColorizedStruct},
    dt::idx::{declare_idx, IndexVec},
    hir::{BodyId, HirId, OwnerId},
    resolve::{builtin::Builtin, def::DefId},
    span::{
        impl_with_span,
        sym::{Ident, Symbol},
        Span, WithSpan,
    },
    typeck::ty::{FieldId, FloatKind, IntKind, Ty, VariantId},
};

declare_idx!(ExprId, u32, "{}", Color::Green);
declare_idx!(BlockId, u32, "{}", Color::Blue);
declare_idx!(StmtId, u32, "{}", Color::Yellow);
declare_idx!(ParamId, u32, "{}", Color::White);
declare_idx!(wrapper LocalVar, HirId, "{}");

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

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Bool(val) => write!(f, "\"{val}\""),
            Lit::Int(val, kind) => write!(f, "{val}{kind}"),
            Lit::Float(val, kind) => write!(f, "{val}{kind}"),
            Lit::String(val) => write!(f, "{val}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprCategory {
    Const,
    LValue,
    StoreRValue,
    AsRValue,
}

impl Display for ExprCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprCategory::Const => "const",
            ExprCategory::LValue => "lvalue",
            ExprCategory::StoreRValue => "store_rvalue",
            ExprCategory::AsRValue => "as_rvalue",
        }
        .fmt(f)
    }
}

/// Non-overloaded infix operators, e.g. `1 + 1`
// #[derive(Clone, Copy)]
// pub enum InfixOp {
//     AddInt,
//     SubInt,
// }

pub enum ExprKind {
    Lit(Lit),
    LocalRef(LocalVar),
    Def(DefId, Ty),
    Block(BlockId),
    Ref(ExprId),
    Call {
        func_ty: Ty,
        lhs: ExprId,
        args: Vec<ExprId>,
    },
    Lambda {
        def_id: DefId,
        body_id: BodyId,
    },
    Ty(ExprId, Ty),
    FieldAccess(ExprId, FieldId, VariantId),
    Builtin(Builtin),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::LocalRef(local) => write!(f, "{local}"),
            ExprKind::Def(def_id, ty) => write!(f, "{def_id}: {ty}"),
            ExprKind::Block(block_id) => write!(f, "block_{block_id}"),
            ExprKind::Ref(expr) => write!(f, "ref {expr}"),
            ExprKind::Call { func_ty, lhs, args } => {
                write!(
                    f,
                    "({lhs}: {func_ty})({args})",
                    args = args
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            },
            ExprKind::Lambda { def_id, body_id } => write!(f, "λ{def_id} {{{body_id}}}"),
            ExprKind::Ty(expr, ty) => write!(f, "{expr}: {ty}"),
            ExprKind::FieldAccess(lhs, field, variant_id) => {
                write!(f, "{lhs}.{variant_id}.{field}")
            },
            ExprKind::Builtin(bt) => write!(f, "{bt}"),
        }
    }
}

pub struct Expr {
    pub ty: Ty,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn categorize(&self) -> ExprCategory {
        match self.kind {
            ExprKind::Lit(_) => ExprCategory::Const,
            ExprKind::FieldAccess(..) | ExprKind::LocalRef(_) => ExprCategory::LValue,

            ExprKind::Ref(_) | ExprKind::Call { .. } | ExprKind::Block(_) => {
                ExprCategory::StoreRValue
            },
            ExprKind::Def(..) | ExprKind::Lambda { .. } | ExprKind::Builtin(_) => {
                ExprCategory::AsRValue
            },

            // FIXME: Ascription is an lvalue or category of inner expression?
            ExprKind::Ty(..) => todo!(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl_with_span!(Expr);

#[derive(Clone, Copy)]
pub enum PatKind {
    Unit,
    Ident { name: Ident, var: LocalVar, ty: Ty },
}

#[derive(Clone, Copy)]
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

    pub fn params_count(&self) -> usize {
        self.params.len()
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

    pub fn is_builtin_expr(&self, id: ExprId, bt: Builtin) -> bool {
        match self.expr(id).kind {
            ExprKind::Builtin(bt_) => bt == bt_,
            _ => false,
        }
    }
}

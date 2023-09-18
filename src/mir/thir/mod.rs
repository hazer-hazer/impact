pub mod build;

use std::fmt::Display;

use inkwell::values;

use crate::{
    cli::color::Color,
    dt::idx::{declare_idx, IndexVec},
    hir::{BodyId, HirId, OwnerId, ValueDefKind},
    resolve::{builtin::ValueBuiltin, def::DefId},
    span::{
        impl_with_span,
        sym::{Ident, Symbol},
        Span, WithSpan,
    },
    typeck::ty::{FieldId, FloatKind, IntKind, Ty},
};

declare_idx!(ExprId, u32, "{}", Color::Green);
declare_idx!(BlockId, u32, "{}", Color::Blue);
declare_idx!(StmtId, u32, "{}", Color::Yellow);
declare_idx!(ParamId, u32, "{}", Color::White);
declare_idx!(PatId, u32, "pat#{}", Color::White);

declare_idx!(wrapper LocalVar, HirId, "{}");

pub enum Stmt {
    Expr(ExprId),
    Local(PatId, ExprId),
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

pub struct Arm {
    pub pat: PatId,
    pub body: ExprId,
}

impl std::fmt::Display for Arm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.pat, self.body)
    }
}

pub enum DecisionTree {
    Match {
        subject: ExprId,
        pat: PatId,
        then: Box<DecisionTree>,
    },
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
    Def(DefId, ValueDefKind, Ty),
    Block(BlockId),
    Ref(ExprId),
    Call {
        func_ty: Ty,
        lhs: ExprId,
        args: Vec<ExprId>,
    },
    Tuple(Vec<ExprId>),
    Lambda {
        def_id: DefId,
        body_id: BodyId,
    },
    Ty(ExprId, Ty),
    // FieldAccess(ExprId, VariantId, FieldId),
    Builtin(ValueBuiltin),
    Match(ExprId, Vec<Arm>),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{lit}"),
            ExprKind::LocalRef(local) => write!(f, "local{local}"),
            ExprKind::Def(def_id, kind, ty) => write!(f, "{kind} {def_id}: {ty}"),
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
            ExprKind::Tuple(values) => write!(
                f,
                "({})",
                values
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::Lambda { def_id, body_id } => write!(f, "λ{def_id} {{{body_id}}}"),
            ExprKind::Ty(expr, ty) => write!(f, "{expr}: {ty}"),
            // ExprKind::FieldAccess(lhs, variant, field) => {
            //     write!(f, "{lhs}.{variant}.{field}")
            // },
            ExprKind::Builtin(bt) => write!(f, "{bt}"),
            ExprKind::Match(subject, arms) => write!(
                f,
                "match {subject} {{{}}}",
                arms.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

pub struct Expr {
    pub ty: Ty,
    pub kind: ExprKind,
    pub span: Span,
}

impl_with_span!(Expr);

impl Expr {
    pub fn categorize(&self) -> ExprCategory {
        match self.kind {
            ExprKind::Lit(_) => ExprCategory::Const,
            // ExprKind::FieldAccess(..) => ExprCategory::LValue,
            ExprKind::LocalRef(_) => ExprCategory::LValue,

            ExprKind::Ref(_) | ExprKind::Call { .. } | ExprKind::Block(_) | ExprKind::Match(..) => {
                ExprCategory::StoreRValue
            },

            ExprKind::Tuple(_)
            | ExprKind::Def(..)
            | ExprKind::Lambda { .. }
            | ExprKind::Builtin(_) => ExprCategory::AsRValue,

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

#[derive(Clone)]
pub enum PatKind {
    Unit,
    Ident { name: Ident, var: LocalVar, ty: Ty },
    Struct(Ty, IndexVec<FieldId, Option<(Option<Ident>, PatId)>>),
    Or(PatId, PatId),
    Tuple(Vec<PatId>),
}

impl Display for PatKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatKind::Unit => write!(f, "()"),
            PatKind::Ident { name, var, ty } => write!(f, "{name}{var}: {ty}"),
            PatKind::Or(lpat, rpat) => write!(f, "{lpat} | {rpat}"),
            PatKind::Struct(ty, fields) => write!(
                f,
                "{ty} {}{}",
                fields
                    .iter()
                    .filter_map(|field| field.as_ref())
                    .map(|(name, pat)| format!(
                        "{}{pat}",
                        name.map_or("".to_string(), |name| format!("{name}: "))
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                if fields.iter().any(|f| f.is_none()) {
                    ", ..."
                } else {
                    ""
                }
            ),
            PatKind::Tuple(..) => todo!(),
        }
    }
}

pub struct Pat {
    pub ty: Ty,
    pub kind: PatKind,
    pub span: Span,
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.ty)
    }
}

pub struct Param {
    pub pat: PatId,
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
    pats: IndexVec<PatId, Pat>,
}

impl THIR {
    pub fn new(body_owner: OwnerId) -> Self {
        Self {
            body_owner,
            stmts: Default::default(),
            exprs: Default::default(),
            blocks: Default::default(),
            params: Default::default(),
            pats: Default::default(),
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

    pub fn pat(&self, id: PatId) -> &Pat {
        self.pats.get(id).unwrap()
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

    pub fn add_pat(&mut self, pat: Pat) -> PatId {
        self.pats.push(pat)
    }

    pub fn body_owner(&self) -> OwnerId {
        self.body_owner
    }

    pub fn is_builtin_expr(&self, id: ExprId, bt: ValueBuiltin) -> bool {
        match self.expr(id).kind {
            ExprKind::Builtin(bt_) => bt == bt_,
            _ => false,
        }
    }
}

//! Session is a compilation context passed through all stages of compilation.

use std::fmt::Display;

use crate::{
    ast::{AstMetadata, NodeId},
    config::config::Config,
    hir::HIR,
    interface::writer::Writer,
    message::{
        message::{Message, MessageStorage},
        term_emitter::TermEmitter,
        MessageEmitter,
    },
    resolve::{
        def::{DefId, DefKind, DefTable},
        res::Resolutions,
    },
    span::{
        source::SourceMap,
        sym::{Ident, Internable},
    },
    typeck::tyctx::TyCtx,
};

pub struct Session {
    config: Config,
    pub writer: Writer,
    pub source_map: SourceMap,
    pub ast_metadata: AstMetadata,
    pub def_table: DefTable,
    pub res: Resolutions,
    pub tyctx: TyCtx,
    pub hir: HIR,
}

impl Session {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            writer: Default::default(),
            source_map: Default::default(),
            ast_metadata: AstMetadata::new(),
            def_table: Default::default(),
            res: Resolutions::default(),
            tyctx: TyCtx::new(),
            hir: HIR::new(),
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    // IDs Synthesis //
    pub fn next_node_id(&mut self) -> NodeId {
        self.ast_metadata.next_node_id()
    }

    pub fn synth_anon_lambda(&mut self) -> (NodeId, DefId) {
        let node_id = self.next_node_id();
        let def_id = self.def_table.define(
            node_id,
            DefKind::Lambda,
            &Ident::synthetic(format!("lambda{}", node_id).intern()),
        );
        (node_id, def_id)
    }
}

pub trait WithSession {
    fn sess(&self) -> &Session;
}

impl WithSession for Session {
    fn sess(&self) -> &Session {
        self
    }
}

pub trait SessionHolder {
    fn sess(&self) -> &Session;
    fn sess_mut(&mut self) -> &mut Session;

    fn hir(&self) -> &HIR {
        &self.sess().hir
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterruptionReason {
    ConfiguredStop,
    ErrorMessage,
}

impl InterruptionReason {
    pub fn from_str(str: &str) -> Self {
        match str {
            "configured" => Self::ConfiguredStop,
            "error" => Self::ErrorMessage,
            _ => panic!("Invalid interruption reason name"),
        }
    }
}

impl Display for InterruptionReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InterruptionReason::ConfiguredStop => "configured",
                InterruptionReason::ErrorMessage => "error",
            }
        )
    }
}

pub type StageResult<T, Ctx = Session> = Result<(T, Ctx), (InterruptionReason, Ctx)>;

pub struct StageOutput<T, Ctx = Session> {
    pub ctx: Ctx,
    pub data: T,
    pub messages: Vec<Message>,
}

impl<T, Ctx> StageOutput<T, Ctx> {
    pub fn new(ctx: Ctx, data: T, messages: MessageStorage) -> Self {
        Self {
            ctx,
            data,
            messages: messages.extract(),
        }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn ctx(&self) -> &Ctx {
        &self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut Ctx {
        &mut self.ctx
    }
}

impl<T, Ctx> StageOutput<T, Ctx>
where
    Ctx: WithSession,
{
    pub fn emit(self, stop_on_error: bool) -> StageResult<T, Ctx> {
        TermEmitter::new().emit(self, stop_on_error)
    }
}

pub trait Stage<T, Ctx = Session>
where
    Self: Sized,
{
    fn run(self) -> StageOutput<T, Ctx>;

    fn run_and_emit(self, stop_on_error: bool) -> StageResult<T, Ctx>
    where
        Ctx: WithSession,
    {
        let output = self.run();
        output.emit(stop_on_error)
    }
}

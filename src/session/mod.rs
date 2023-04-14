//! Session is a compilation context passed through all stages of compilation.

use crate::{
    ast::{AstMetadata, NodeId},
    config::config::Config,
    hir::HIR,
    interface::{interface::InterruptionReason, writer::Writer},
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

    pub fn with_sess<'a, T>(&'a self, val: &'a T) -> WithSession<'a, T> {
        WithSession { sess: self, val }
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

pub struct WithSession<'a, T> {
    sess: &'a Session,
    val: &'a T,
}

pub trait SessionHolder {
    fn sess(&self) -> &Session;
    fn sess_mut(&mut self) -> &mut Session;

    fn hir(&self) -> &HIR {
        &self.sess().hir
    }
}

pub struct StageOutput<T> {
    pub sess: Session,
    pub data: T,
    pub messages: Vec<Message>,
}

pub type StageResult<T> = Result<(T, Session), (InterruptionReason, Session)>;

impl<'ast, T> StageOutput<T> {
    pub fn new(sess: Session, data: T, messages: MessageStorage) -> Self {
        Self {
            sess,
            data,
            messages: messages.extract(),
        }
    }

    pub fn emit(self, stop_on_error: bool) -> StageResult<T> {
        TermEmitter::new().emit(self, stop_on_error)
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn sess(&self) -> &Session {
        &self.sess
    }

    pub fn sess_mut(&mut self) -> &mut Session {
        &mut self.sess
    }
}

pub trait Stage<T>
where
    Self: Sized,
{
    fn run(self) -> StageOutput<T>;

    fn run_and_emit(self, stop_on_error: bool) -> StageResult<T> {
        let output = self.run();
        output.emit(stop_on_error)
    }
}

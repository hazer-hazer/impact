use crate::{
    ast::{AstMetadata, NodeId},
    config::config::Config,
    hir::HIR,
    interface::writer::Writer,
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

pub trait SessionHolder {
    fn sess(&self) -> &Session;
    fn sess_mut(&mut self) -> &mut Session;

    fn hir(&self) -> &HIR {
        &self.sess().hir
    }
}

impl SessionHolder for Session {
    fn sess(&self) -> &Session {
        self
    }

    fn sess_mut(&mut self) -> &mut Session {
        self
    }

    fn hir(&self) -> &HIR {
        &self.hir
    }
}

macro_rules! impl_session_holder {
    ($ty: ident $(<$($gen: tt),*>)?; $($path: tt)+) => {
        impl<$($($gen),*)?> crate::session::sess::SessionHolder for $ty<$($($gen),*)?> {
            fn sess(&self) -> &crate::session::sess::Session {
                &self.$($path)+
            }

            fn sess_mut(&mut self) -> &mut crate::session::sess::Session {
                &mut self.$($path)+
            }
        }
    };

    ($ty: ident $(<$($gen: tt),*>)?) => {
        impl_session_holder!($ty $(<$($gen),*>)?; sess);
    };
}

pub(crate) use impl_session_holder;

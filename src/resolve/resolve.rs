use std::collections::HashMap;

use crate::{
    ast::{NodeId, Path},
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::res::ResKind,
    session::{Session, Stage, StageOutput},
    span::span::Symbol,
};

use super::{def::ModuleId, res::Res};

struct Rib {
    locals: HashMap<Symbol, NodeId>,
    bound_module: Option<ModuleId>,
}

impl Rib {
    pub fn new(bound_module: Option<ModuleId>) -> Self {
        Self {
            locals: Default::default(),
            bound_module,
        }
    }
}

pub struct NameResolver {
    ribs: Vec<Rib>,
    nearest_mod: ModuleId,
    msg: MessageStorage,
    sess: Session,
}

impl NameResolver {
    fn rib(&self) -> &Rib {
        &self.ribs.last().unwrap()
    }

    fn resolve_path(&mut self, path: &Path) -> Res {
        let segments = path.segments();

        if segments.get(0).unwrap().is_var() && segments.len() == 0 {
            let seg = segments.get(0).unwrap();
            if let Some(local) = self.rib().locals.get(&seg.sym()) {
                return Res::local(*local);
            }
        }

        // Path is not a path if it is not a path ☝️
        assert!(segments.get(0).unwrap().is_ty());

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod);

        for seg_index in 0..segments.len() {
            let seg = segments[seg_index];
            let seg_name = seg.sym();
            let is_target = seg_index == segments.len() - 1;

            if !is_target && !seg.is_ty() {
                MessageBuilder::error()
                    .span(path.prefix_span(seg_index))
                    .text(format!("Invalid path `{}`", path))
                    .label(
                        seg.span(),
                        format!("{} must be a type or module name", seg_name),
                    )
                    .emit(self);
                return Res::error();
            }

            let def = match search_mod.get_by_ident(&seg) {
                Some(def) => def,
                None => {
                    MessageBuilder::error()
                        .span(path.prefix_span(seg_index))
                        .text(format!(
                            "Cannot find {} in {}",
                            seg_name,
                            path.prefix_str(seg_index)
                        ))
                        .emit_single_label(self);
                    return Res::error();
                }
            };

            if is_target {
                return Res::def(*def);
            } else {
                search_mod = self.sess.def_table.get_module(ModuleId::Module(*def))
            }
        }

        unreachable!()
    }
}

impl MessageHolder for NameResolver {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl Stage<()> for NameResolver {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}

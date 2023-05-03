use super::AstLikePP;
use crate::{
    parser::token::Punct,
    resolve::def::{Def, DefKind, Module, ModuleId, ROOT_MODULE_ID},
};

pub trait DefPrinter {
    fn pp_defs(&mut self);
    fn pp_mod(&mut self, module_id: ModuleId);
    fn _pp_mod(&mut self, module: &Module);
}

impl<'a> DefPrinter for AstLikePP<'a, ()> {
    fn pp_defs(&mut self) {
        self.line("== Definitions ==");
        self.sess
            .def_table
            .defs()
            .iter()
            .for_each(|Def { def_id, kind, name }| {
                self.string(format!("{def_id}: {kind} {name}")).nl();
            });

        self.line("== Blocks ==");
        self.sess
            .def_table
            .blocks()
            .iter_enumerated_flat()
            .for_each(|(node_id, module)| {
                self.string(format!("Block{}:\n", node_id));
                self.indent();
                self._pp_mod(module);
                self.dedent();
            });

        self.line("== Module tree ==");
        self.pp_mod(ROOT_MODULE_ID);
    }

    fn pp_mod(&mut self, module_id: ModuleId) {
        let module = self.sess.def_table.get_module(module_id);
        self._pp_mod(module);
    }

    fn _pp_mod(&mut self, module: &Module) {
        for (ns, defs) in module.namespaces().iter() {
            for (sym, def_id) in defs {
                self.out_indent();
                self.string(sym.original_string());
                self.string(def_id);
                self.punct(Punct::Colon);

                let def = self.sess.def_table.def(*def_id);

                self.string(def.kind());

                match def.kind() {
                    // TODO: Review Variant as module
                    DefKind::Variant | DefKind::Adt | DefKind::Root | DefKind::Mod => {
                        self.nl();
                        self.indent();
                        self.pp_mod(ModuleId::Def(def.def_id()));
                        self.dedent();
                    },
                    DefKind::Lambda
                    | DefKind::DeclareBuiltin
                    | DefKind::Value
                    | DefKind::TyAlias
                    | DefKind::Local
                    | DefKind::External
                    | DefKind::Ctor
                    | DefKind::FieldAccessor
                    | DefKind::TyParam
                    | DefKind::Func => {
                        self.nl();
                    },
                }
            }
        }
    }
}

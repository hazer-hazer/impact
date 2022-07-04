use crate::{
    parser::token::Punct,
    resolve::def::{Def, DefKind, ModuleId, ROOT_DEF_ID},
};

use super::AstLikePP;

pub trait DefPrinter {
    fn pp_defs(&mut self);
    fn pp_mod(&mut self, module_id: ModuleId);
}

impl<'a> DefPrinter for AstLikePP<'a> {
    fn pp_defs(&mut self) {
        self.line("= Definitions =");
        self.sess
            .def_table
            .defs()
            .iter()
            .for_each(|Def { def_id, kind, name }| {
                self.string(format!("{def_id}: {kind} {name}")).nl();
            });

        self.line("= Module tree =");
        self.pp_mod(ModuleId::Module(ROOT_DEF_ID));
    }

    fn pp_mod(&mut self, module_id: ModuleId) {
        let module = self.sess.def_table.get_module(module_id);

        for ns in module.namespaces().iter() {
            for (sym, def_id) in ns {
                self.out_indent();
                self.string(sym);
                self.punct(Punct::Colon);

                let def = self.sess.def_table.get_def(*def_id).unwrap();

                self.string(def);

                match def.kind() {
                    DefKind::Mod => {
                        self.nl();
                        self.indent();
                        self.pp_mod(ModuleId::Module(def.def_id()));
                        self.dedent();
                    }
                    DefKind::Type | DefKind::Decl => {
                        self.nl();
                    }
                }
            }
        }
    }
}

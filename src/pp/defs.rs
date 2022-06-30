use crate::resolve::def::{Def, DefKind, ModuleId, ROOT_DEF_ID};

use super::AstLikePP;

pub trait DefPrinter {
    fn pp_defs(&mut self) -> String;
    fn pp_mod(&mut self, module_id: ModuleId) -> String;
}

impl<'a> DefPrinter for AstLikePP<'a> {
    fn pp_defs(&mut self) -> String {
        format!(
            "Definitions\n{}\nModule tree:{}",
            self.sess
                .def_table
                .defs()
                .iter()
                .map(|Def { def_id, kind, name }| format!("{def_id}: {kind} {name}"))
                .collect::<Vec<_>>()
                .join("\n"),
            self.pp_mod(ModuleId::Module(ROOT_DEF_ID))
        )
    }

    fn pp_mod(&mut self, module_id: ModuleId) -> String {
        let module = self.sess.def_table.get_module(module_id);

        // Print name bindings from each namespace of the module
        module
            .namespaces()
            .iter()
            .map(|ns| {
                // Print a single name binding as `name: Def` with indentation,
                //  where `Def` PP is defined by its display impl,
                //  possible printing nested items in module definitions
                ns.iter()
                    .map(|(sym, def_id)| {
                        format!("{}{}: {}", self.cur_indent(), sym, {
                            let def = self.sess.def_table.get_def(*def_id).unwrap();
                            let nested_module = match def.kind() {
                                DefKind::Mod => {
                                    self.indent();
                                    let mod_string =
                                        format!("{}", self.pp_mod(ModuleId::Module(def.def_id())));
                                    self.dedent();
                                    mod_string
                                }
                                DefKind::Type | DefKind::Decl => "".to_string(),
                            };

                            format!("{}{}", def, nested_module)
                        })
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

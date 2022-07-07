use std::collections::HashMap;

use crate::{
    ast::{
        item::{Item, ItemKind},
        visitor::AstVisitor,
        NodeId, NodeMap, Path, WithNodeId,
    },
    cli::{
        color::{Color, Colorize},
        verbose,
    },
    resolve::res::{NamePath, ResKind},
};

use super::AstLikePP;

pub struct ResPrinter<'a> {
    last_color_index: usize,
    names_colors: NodeMap<Color>,
    pp: AstLikePP<'a>,
}

const ALLOWED_COLORS: &[Color] = &[
    Color::Red,
    Color::Green,
    Color::Yellow,
    Color::Blue,
    Color::Magenta,
    Color::Cyan,
];

impl<'a> ResPrinter<'a> {
    pub fn new(pp: AstLikePP<'a>) -> Self {
        Self {
            last_color_index: 0,
            names_colors: Default::default(),
            pp,
        }
    }

    fn name_color(&mut self, node_id: Option<NodeId>) -> Color {
        const ERROR_COLOR: Color = Color::Black;

        if node_id.is_none() {
            return ERROR_COLOR;
        }

        let node_id = node_id.unwrap();

        if let Some(color) = self.names_colors.get(&node_id) {
            return *color;
        }

        let color = ALLOWED_COLORS[self.last_color_index];
        self.last_color_index = (self.last_color_index + 1) % ALLOWED_COLORS.len();

        assert!(self.names_colors.insert(node_id, color).is_none());

        color
    }
}

impl<'a> AstVisitor for ResPrinter<'a> {
    fn visit_err(&mut self, _: &crate::ast::ErrorNode) {
        self.pp.str("[ERR]");
    }

    fn visit_item(&mut self, item: &Item) {
        // Note: We aren't colorizing actual names as it is much changes in visitor structure,
        //  just adding a marker before item

        let color = self.name_color(Some(item.id()));
        self.pp.string("@".fg_color(color));

        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_path(&mut self, path: &Path) {
        let res = self
            .pp
            .sess
            .res
            .get(NamePath::new(path.id()))
            .expect("No unresolved paths must exist after name resolution");

        let node_id = match res.kind() {
            ResKind::Def(def_id) => Some(self.pp.sess.def_table.get_node_id(*def_id).unwrap()),
            ResKind::Local(node_id) => Some(*node_id),
            ResKind::Error => None,
        };

        let color = self.name_color(node_id);
        self.pp.string(path.to_string().fg_color(color));
    }
}

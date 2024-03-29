use crate::{
    ast::{
        expr::{InfixOp, InfixOpKind, PrefixOp, PrefixOpKind},
        NodeId, NodeMap, Path, WithNodeId,
    },
    cli::color::{Color, Colorize},
    parser::token::Punct,
    resolve::res::{NamePath, ResKind},
    session::Session,
    span::span::{Ident, Kw},
};

pub mod ast;
pub mod defs;
pub mod hir;

#[derive(PartialEq)]
pub enum AstPPMode {
    Normal,
    NameHighlighter,
}

const ALLOWED_COLORS: &[Color] = &[
    Color::Green,
    Color::Yellow,
    Color::Blue,
    Color::Magenta,
    Color::Cyan,
    Color::BrightRed,
    Color::BrightCyan,
    Color::BrightGreen,
    Color::BrightYellow,
    Color::BrightBlue,
    Color::BrightMagenta,
];

pub struct AstLikePP<'a> {
    out: String,
    indent_level: u32,
    sess: &'a Session,
    mode: AstPPMode,

    // Name highlighter mode //
    last_color_index: usize,
    /// NodeId of item -> Color
    /// Resolution gives us item NodeId
    names_colors: NodeMap<Color>,
}

impl<'a> AstLikePP<'a> {
    pub fn new(sess: &'a Session, mode: AstPPMode) -> Self {
        Self {
            out: String::new(),
            indent_level: 0,
            sess,
            mode,
            last_color_index: 0,
            names_colors: Default::default(),
        }
    }

    pub fn get_string(self) -> String {
        self.out
    }

    fn ch(&mut self, ch: char) -> &mut Self {
        self.out.push(ch);
        self
    }

    fn sp(&mut self) -> &mut Self {
        self.ch(' ')
    }

    fn nl(&mut self) -> &mut Self {
        self.ch('\n')
    }

    fn str(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self
    }

    fn string<T>(&mut self, value: T) -> &mut Self
    where
        T: ToString,
    {
        self.str(&value.to_string());
        self
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
    }

    fn cur_indent(&self) -> String {
        "  ".repeat(self.indent_level as usize)
    }

    fn out_indent(&mut self) -> &mut Self {
        self.str(&self.cur_indent())
    }

    fn line(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self.nl()
    }

    fn kw(&mut self, kw: Kw) -> &mut Self {
        let (pre, post) = match kw {
            Kw::In => (" ", " "),
            Kw::Mod => ("", " "),
            Kw::Let | Kw::Type | Kw::Root | Kw::M | Kw::Unknown => ("", ""),
        };

        self.str(pre);
        self.str(&kw.to_string());
        self.str(post)
    }

    fn punct(&mut self, punct: Punct) -> &mut Self {
        let (pre, post) = match punct {
            Punct::Assign | Punct::Arrow => (" ", " "),
            Punct::Colon => ("", " "),
            Punct::Dot | Punct::LParen | Punct::RParen | Punct::Backslash => ("", ""),
        };

        self.str(pre);
        self.str(&punct.to_string());
        self.str(post)
    }

    fn infix(&mut self, infix: &InfixOp) -> &mut Self {
        let (pre, post) = match infix.node() {
            InfixOpKind::Plus
            | InfixOpKind::Minus
            | InfixOpKind::Mul
            | InfixOpKind::Div
            | InfixOpKind::Mod => (" ", " "),
        };

        self.str(pre);
        self.str(&infix.node().to_string());
        self.str(post)
    }

    fn prefix(&mut self, prefix: &PrefixOp) -> &mut Self {
        let post = match prefix.node() {
            PrefixOpKind::Not => "",
        };

        self.str(&prefix.node().to_string());
        self.str(post)
    }

    // Name highlighting mode //
    fn name_color(&mut self, node_id: Option<NodeId>) -> Color {
        const ERROR_COLOR: Color = Color::Red;

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

    fn def_name(&mut self, ident: &Ident, item_node_id: NodeId) {
        if self.mode == AstPPMode::Normal {
            self.string(ident);
            return;
        }

        let color = self.name_color(Some(item_node_id));
        self.string(ident.sym().to_string().fg_color(color));
    }

    fn path(&mut self, path: &Path) {
        if self.mode == AstPPMode::Normal {
            self.string(path);
            return;
        }

        let res = self
            .sess
            .res
            .get(NamePath::new(path.id()))
            .expect("No unresolved paths must exist after name resolution");

        let node_id = match res.kind() {
            ResKind::Def(def_id) => Some(self.sess.def_table.get_node_id(*def_id).unwrap()),
            ResKind::Local(node_id) => Some(*node_id),
            ResKind::Error => None,
        };

        let color = self.name_color(node_id);
        if let Some(_) = node_id {
            self.string(path.to_string().fg_color(color));
        } else {
            // Highlight as error
            self.string(format!("[ERR:{}]", path).fg_color(color));
        };
    }
}

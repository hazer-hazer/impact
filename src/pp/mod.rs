use crate::{
    ast::{NodeId, NodeMap, Path, WithNodeId},
    cli::color::{Color, Colorize, ColorizedStruct},
    hir::{item::ItemId, HirId, WithHirId, WithNodeKind},
    parser::token::{Op, Punct},
    resolve::{
        def::DefId,
        res::{NamePath, ResKind},
    },
    session::Session,
    span::sym::{Ident, Kw},
};

pub mod ast;
pub mod defs;
pub mod hir;
pub mod mir;
pub mod thir;

#[derive(PartialEq)]
pub enum AstPPMode {
    Normal,
    NameHighlighter,
    TyAnno,
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

const BUILTIN_COLOR: Color = Color::Cyan;

pub struct AstLikePP<'a, D = ()> {
    out: String,
    indent_level: u32,
    sess: &'a Session,
    mode: AstPPMode,

    // Name highlighter mode //
    last_color_index: usize,
    /// NodeId of item -> Color
    /// Resolution gives us item NodeId
    names_colors: NodeMap<Color>,

    data: D,
}

impl<'a> AstLikePP<'a, ()> {
    pub fn new(sess: &'a Session, mode: AstPPMode) -> Self {
        Self {
            out: String::new(),
            indent_level: 0,
            sess,
            mode,
            last_color_index: 0,
            names_colors: Default::default(),
            data: (),
        }
    }
}

impl<'a, D> AstLikePP<'a, D> {
    pub fn with_data(sess: &'a Session, mode: AstPPMode, data: D) -> Self {
        Self {
            out: String::new(),
            indent_level: 0,
            sess,
            mode,
            last_color_index: 0,
            names_colors: Default::default(),
            data,
        }
    }

    pub fn node_id(&mut self, with_node_id: &impl WithNodeId) -> &mut Self {
        if self.sess.config().pp_ast_ids() {
            self.string(with_node_id.id());
        }
        self
    }

    pub fn def_id(&mut self, def_id: &DefId) -> &mut Self {
        if self.sess.config().pp_ast_ids() {
            self.string(def_id);
        }
        self
    }

    pub fn hir_id(&mut self, node: &(impl WithHirId + WithNodeKind)) -> &mut Self {
        if self.sess.config().pp_ast_ids() {
            self.string(format!(
                "[{}:{}:{}]",
                node.kind().to_string().fg_color(Color::BrightBlack),
                node.id().owner(),
                node.id().child_id()
            ));
        }
        self
    }

    pub fn item_id(&mut self, item_id: ItemId) -> &mut Self {
        if self.sess.config().pp_ast_ids() {
            self.string(item_id);
        }
        self
    }

    pub fn ty<TyId>(&mut self, id: TyId) -> &mut Self
    where
        TyId: Into<HirId>,
    {
        if self.mode == AstPPMode::TyAnno {
            self.string(
                self.sess
                    .tyctx
                    .pp_typed_node(id.into())
                    .fg_color(Color::BrightBlue),
            );
        }
        self
    }

    pub fn ty_anno<Id>(&mut self, id: Id) -> &mut Self
    where
        Id: Into<HirId>,
    {
        if self.mode == AstPPMode::TyAnno {
            self.punct(Punct::Colon);
            self.ty(id);
        }
        self
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

    fn colorized<T: ColorizedStruct>(&mut self, value: T) -> &mut Self {
        self.string(value.colorized())
    }

    fn join<T>(&mut self, values: impl Iterator<Item = T>, sep: &str) -> &mut Self
    where
        T: ToString,
    {
        // TODO: Prettify
        self.string(
            values
                .map(|val| val.to_string())
                .collect::<Vec<String>>()
                .join(sep),
        )
    }

    fn indent(&mut self) -> &mut Self {
        self.indent_level += 1;
        self
    }

    fn dedent(&mut self) -> &mut Self {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
        self
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
            Kw::Data | Kw::Extern | Kw::Type | Kw::Mod => ("", " "),
            Kw::Unit | Kw::Underscore | Kw::Let | Kw::Root | Kw::Unknown => ("", ""),
        };

        self.str(pre);
        self.str(&kw.to_string());
        self.str(post)
    }

    fn punct(&mut self, punct: Punct) -> &mut Self {
        let (pre, post) = match punct {
            Punct::Arrow => (" ", " "),
            Punct::Colon => ("", " "),
            Punct::Comma
            | Punct::Semi
            | Punct::Dot
            | Punct::LParen
            | Punct::RParen
            | Punct::Backslash => ("", ""),
        };

        self.str(pre);
        self.string(punct);
        self.str(post)
    }

    fn op(&mut self, op: Op) -> &mut Self {
        let (pre, post) = match op {
            Op::Plus | Op::Minus | Op::Mul | Op::Div | Op::Mod | Op::BitOr | Op::Assign => {
                (" ", " ")
            },
        };

        self.str(pre);
        self.string(op);
        self.str(post)
    }

    fn ident(&mut self, ident: &Ident) -> &mut Self {
        if ident.is_op() {
            self.str("(");
            self.string(ident);
            self.str(")");
        } else {
            self.string(ident);
        }
        self
    }

    // fn infix(&mut self, infix: &Path) -> &mut Self {
    //     self.str(" ");
    //     self.str(&infix.to_string());
    //     self.str(" ")
    // }

    // Name highlighting mode //
    fn name_color(&mut self, node_id: Option<NodeId>) -> Color {
        const ERROR_COLOR: Color = Color::Red;

        if node_id.is_none() {
            return ERROR_COLOR;
        }

        let node_id = node_id.unwrap();

        if let Some(color) = self.names_colors.get_flat(node_id) {
            return *color;
        }

        let color = ALLOWED_COLORS[self.last_color_index];
        self.last_color_index = (self.last_color_index + 1) % ALLOWED_COLORS.len();

        assert!(self.names_colors.insert(node_id, color).is_none());

        color
    }

    fn name(&mut self, name: &Ident, item_node_id: NodeId, is_def: bool) {
        if self.mode == AstPPMode::Normal {
            if is_def {
                self.string(name.original_string());
            } else {
                self.string(name);
            }
            return;
        }

        let color = self.name_color(Some(item_node_id));
        self.string(
            if is_def {
                name.original_string()
            } else {
                name.to_string()
            }
            .fg_color(color),
        );
    }

    fn path(&mut self, path: &Path) {
        if self.mode == AstPPMode::Normal {
            self.string(path);
            return;
        }

        let res = self.sess.res.get(NamePath::new(path.id())).expect(&format!(
            "No unresolved paths must exist after name resolution. Unresolved path `{}`",
            path
        ));

        let node_id = match res.kind() {
            ResKind::Local(node_id) => Some(*node_id),
            ResKind::Def(def_id) => {
                Some(self.sess.def_table.get_node_id(*def_id).expect(&format!(
                    "Name resolution to def {} by name {}",
                    def_id,
                    path.to_string()
                )))
            },
            ResKind::DeclareBuiltin => {
                self.string("[`builtin`]".fg_color(BUILTIN_COLOR));
                return;
            },
            ResKind::Error => None,
        };

        let color = self.name_color(node_id);
        if let Some(_) = node_id {
            self.string(path.to_string().fg_color(color));
        } else {
            // Highlight as error
            self.string(format!("[ERR:{}]", path).fg_color(color));
        };

        self.node_id(path);
    }
}

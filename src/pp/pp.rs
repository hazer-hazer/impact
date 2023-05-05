use crate::{
    cli::color::ColorizedStruct,
    parser::token::{Op, Punct},
    span::sym::{Ident, Kw},
};

macro_rules! pp {
    ($pp: expr) => {
        $pp
    };

    ($pp: expr, $($arg:tt)+) => {
        pp!(@inner $pp, $($arg)+)
    };

    (@inner $pp: expr, $($rest: expr)*) => {
        $pp$(.string($rest))*
    };

    (@inner $pp: expr, {$fmt: ident?: $expr: expr}) => {
        if let Some(expr) = $expr {
            pp.$fmt(expr)
        } else {
            $pp
        }
    };

    (@inner $pp: expr, {$fmt: ident: $expr: expr}) => {
        pp.$fmt($expr)
    };
}

pub(crate) use pp;

pub struct PP {
    out: String,
    indent_level: u32,
}

impl PP {
    pub fn new() -> Self {
        Self {
            out: String::new(),
            indent_level: 0,
        }
    }
}

impl PP {
    pub fn get_string(self) -> String {
        self.out
    }

    pub fn ch(&mut self, ch: char) -> &mut Self {
        self.out.push(ch);
        self
    }

    pub fn sp(&mut self) -> &mut Self {
        self.ch(' ')
    }

    pub fn nl(&mut self) -> &mut Self {
        self.ch('\n')
    }

    pub fn str(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self
    }

    pub fn string<T>(&mut self, value: T) -> &mut Self
    where
        T: ToString,
    {
        self.str(&value.to_string());
        self
    }

    pub fn colorized<T: ColorizedStruct>(&mut self, value: T) -> &mut Self {
        self.string(value.colorized())
    }

    pub fn join<T>(&mut self, values: impl Iterator<Item = T>, sep: &str) -> &mut Self
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

    pub fn indent(&mut self) -> &mut Self {
        self.indent_level += 1;
        self
    }

    pub fn dedent(&mut self) -> &mut Self {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
        self
    }

    pub fn cur_indent(&self) -> String {
        "  ".repeat(self.indent_level as usize)
    }

    pub fn out_indent(&mut self) -> &mut Self {
        self.str(&self.cur_indent())
    }

    pub fn line(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self.nl()
    }

    pub fn kw(&mut self, kw: Kw) -> &mut Self {
        let (pre, post) = match kw {
            Kw::In => (" ", " "),
            Kw::Data | Kw::Extern | Kw::Type | Kw::Mod => ("", " "),
            Kw::Unit | Kw::Underscore | Kw::Let | Kw::Root | Kw::Unknown => ("", ""),
        };

        self.str(pre);
        self.str(&kw.to_string());
        self.str(post)
    }

    pub fn punct(&mut self, punct: Punct) -> &mut Self {
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

    pub fn op(&mut self, op: Op) -> &mut Self {
        let (pre, post) = match op {
            Op::Plus | Op::Minus | Op::Mul | Op::Div | Op::Mod | Op::BitOr | Op::Assign => {
                (" ", " ")
            },
        };

        self.str(pre);
        self.string(op);
        self.str(post)
    }

    pub fn ident(&mut self, ident: &Ident) -> &mut Self {
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
}
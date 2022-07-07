use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,

    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
}

impl Color {
    pub fn fg_code_str(&self) -> &str {
        match self {
            Color::Black => "30",
            Color::Red => "31",
            Color::Green => "32",
            Color::Yellow => "33",
            Color::Blue => "34",
            Color::Magenta => "35",
            Color::Cyan => "36",
            Color::White => "37",
            Color::BrightBlack => "90",
            Color::BrightRed => "91",
            Color::BrightGreen => "92",
            Color::BrightYellow => "93",
            Color::BrightBlue => "94",
            Color::BrightMagenta => "95",
            Color::BrightCyan => "96",
            Color::BrightWhite => "97",
        }
    }

    pub fn bg_code_str(&self) -> &str {
        match self {
            Color::Black => "40",
            Color::Red => "41",
            Color::Green => "42",
            Color::Yellow => "43",
            Color::Blue => "44",
            Color::Magenta => "45",
            Color::Cyan => "46",
            Color::White => "47",
            Color::BrightBlack => "100",
            Color::BrightRed => "101",
            Color::BrightGreen => "102",
            Color::BrightYellow => "103",
            Color::BrightBlue => "104",
            Color::BrightMagenta => "105",
            Color::BrightCyan => "106",
            Color::BrightWhite => "107",
        }
    }
}

pub struct ColorizedString {
    string: String,
    bg: Option<Color>,
    fg: Option<Color>,
}

impl ColorizedString {
    pub fn get_escape_seq(&self) -> String {
        format!(
            "\x1B[{}{}{}m",
            self.bg.as_ref().map(|bg| bg.bg_code_str()).unwrap_or(""),
            if self.bg.as_ref().and(self.fg.as_ref()).is_some() {
                ":"
            } else {
                ""
            },
            self.fg.as_ref().map(|fg| fg.fg_code_str()).unwrap_or("")
        )
    }
}

impl Display for ColorizedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}\x1B[0m", self.get_escape_seq(), self.string)
    }
}

pub trait Colorize {
    fn fg_color(self, color: Color) -> ColorizedString;
    fn bg_color(self, color: Color) -> ColorizedString;

    fn black(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Black)
    }
    fn red(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Red)
    }
    fn green(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Green)
    }
    fn yellow(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Yellow)
    }
    fn blue(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Blue)
    }
    fn magenta(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Magenta)
    }
    fn cyan(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::Cyan)
    }
    fn white(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::White)
    }

    fn bright_black(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightBlack)
    }
    fn bright_red(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightRed)
    }
    fn bright_green(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightGreen)
    }
    fn bright_yellow(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightYellow)
    }
    fn bright_blue(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightBlue)
    }
    fn bright_magenta(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightMagenta)
    }
    fn bright_cyan(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightCyan)
    }
    fn bright_white(self) -> ColorizedString
    where
        Self: Sized,
    {
        self.fg_color(Color::BrightWhite)
    }
}

impl<'a> Colorize for &'a str {
    fn fg_color(self, color: Color) -> ColorizedString {
        ColorizedString {
            string: String::from(self),
            bg: None,
            fg: Some(color),
        }
    }

    fn bg_color(self, color: Color) -> ColorizedString {
        ColorizedString {
            string: String::from(self),
            bg: Some(color),
            fg: None,
        }
    }
}

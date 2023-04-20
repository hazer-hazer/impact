use crate::utils::macros::concat_string;

macro_rules! define_sections {
    ($($name: ident),*) => {
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        pub enum WriterSection {
            $($name),*
        }

        impl std::fmt::Display for WriterSection {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => stringify!($name),)*
                }.fmt(f)
            }
        }

        #[derive(Default)]
        pub struct Writer {
            $($name: String),*
        }

        impl Writer {
            fn section_mut(&mut self, section: WriterSection) -> &mut String {
                match section {
                    $(WriterSection::$name => &mut self.$name),*
                }
            }

            pub fn data(&self, section: WriterSection) -> &String {
                match section {
                    $(WriterSection::$name => &self.$name),*
                }
            }

            pub fn iter_sections(&self) -> impl Iterator<Item = (WriterSection, &String)> {
                [$((WriterSection::$name, &self.$name)),*].into_iter()
            }

            pub fn merge(&mut self, other: Writer) {
                $(self.$name = concat_string!(self.$name, other.$name));*
            }

            pub fn clear(&mut self) {
                $(self.$name.clear());*
            }

            pub fn take_away_from(&mut self, other: &mut Writer) {
                $(self.$name = concat_string!(self.$name, other.$name);)*
                other.clear();
            }
        }
    };
}

define_sections!(msg, dbg);

impl Writer {
    pub fn write(&mut self, section: WriterSection, add: String) {
        self.section_mut(section).push_str(&add)
    }

    pub fn writeln(&mut self, section: WriterSection, mut add: String) {
        add.push('\n');
        self.section_mut(section).push_str(&add);
    }

    pub fn nl(&mut self, section: WriterSection) {
        self.section_mut(section).push('\n')
    }
}

macro_rules! out {
    ($section: ident, $writer: expr, $($args: tt)*) => {
        {
            use crate::interface::writer::WriterSection;
            $writer.write(WriterSection::$section, std::format!($($args)*))
        }
    };
}

pub(crate) use out;

macro_rules! outln {
    ($section: ident, $writer: expr) => {
        {
            use crate::interface::writer::WriterSection;
            $writer.nl(WriterSection::$section)
        }
    };

    ($section: ident, $writer: expr, $($args: tt)*) => {
        {
            use crate::interface::writer::WriterSection;
            $writer.writeln(WriterSection::$section, std::format!($($args)*))
        }
    };
}

pub(crate) use outln;

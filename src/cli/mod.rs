pub mod cli;
pub mod color;
pub mod command;

macro_rules! verbose {
    ($($arg:tt)*) => {{
        if cfg!(feature = "verbose_debug") {
            println!($($arg)*)
        } else {
        }
    }};
}

pub(crate) use verbose;

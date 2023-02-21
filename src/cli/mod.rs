pub mod cli;
pub mod color;

macro_rules! verbose {
    ($($arg:tt)*) => {{
        if cfg!(feature = "verbose_debug") {
            println!($($arg)*)
        } else {
        }
    }};
}

pub(crate) use verbose;

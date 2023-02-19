pub mod cli;
pub mod color;

macro_rules! verboseln {
    ($($arg:tt)*) => {{
        if cfg!(feature = "verbose_debug") {
            println!($($arg)*)
        } else {
        }
    }};
}

macro_rules! verbose {
    ($($arg:tt)*) => {
        if cfg!(feature = "verbose_debug") {
            print!($($arg)*)
        } else {
        }
    };
}

pub(crate) use verbose;
pub(crate) use verboseln;

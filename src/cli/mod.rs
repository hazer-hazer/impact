pub mod cli;
pub mod color;

macro_rules! verbose {
    ($msg: expr) => {{
        if cfg!(feature = "verbose_debug") {
            println!("{}", $msg)
        } else {
        }
    }};
}

pub(crate) use verbose;

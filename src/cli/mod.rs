pub mod cli;

#[cfg(feature = "verbose_debug")]
pub fn verbose(msg: String) {
    println!("{}", msg)
}

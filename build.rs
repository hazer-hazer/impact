use std::env;

use clap::{CommandFactory, Error};
use clap_complete::{generate_to, Shell};

include!("src/cli/command.rs");

fn main() -> Result<(), Error> {
    println!("cargo:rerun-if-changed=src/cli/command.rs");
    println!("cargo:rerun-if-changed=build.rs");

    let outdir = match env::var_os("OUT_DIR") {
        Some(outdir) => outdir,
        None => return Ok(()),
    };

    println!("outdir {:?}", outdir);

    let mut cmd = Args::command();

    let bin_name = "impact";
    for generator in Shell::value_variants() {
        let path = generate_to(*generator, &mut cmd, bin_name, outdir.clone())?;
        println!("Generated completions {}", path.to_str().unwrap());
    }

    Ok(())
}

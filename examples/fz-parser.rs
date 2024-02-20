use anyhow::Result;
use clap::Parser;
use flatzinc::Stmt;
use log::error;
use std::path::PathBuf;
use stderrlog;

/// flatzinc parser
#[derive(Parser, Debug)]
#[clap(name = "fz-parser")]
struct Opt {
    /// Input in flatzinc format
    #[clap(short = 'i', long = "input")]
    file: PathBuf,
}

fn main() {
    if let Err(err) = run() {
        error!("Error: {:?}", err);
        std::process::exit(1);
    }
}
fn run() -> Result<()> {
    stderrlog::new()
        .module(module_path!())
        .verbosity(2)
        .init()
        .unwrap();

    let opt = Opt::parse();
    let buf = std::fs::read_to_string(opt.file)?;
    for mut line in buf.lines() {
        match <Stmt as std::str::FromStr>::from_str(&mut line) {
            Ok(result) => println!("{:#?}", result),
            Err(e) => {
                error!("Failed to parse flatzinc statement:\n{}", e);
            }
        }
    }
    Ok(())
}

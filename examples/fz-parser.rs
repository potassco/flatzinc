use anyhow::Result;
use clap::Parser as clapParser;
use log::error;
use std::path::PathBuf;
use stderrlog;
use winnow::{error::ContextError, Parser};

/// flatzinc parser
#[derive(clapParser, Debug)]
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
    let mut parser = flatzinc::statement::<ContextError<&str>, &str>();
    for line in buf.lines() {
        match parser.parse(line) {
            Ok(result) => println!("{:#?}", result),
            Err(e) => {
                error!("Failed to parse flatzinc!\n{:?}", e)
            }
        }
    }
    Ok(())
}

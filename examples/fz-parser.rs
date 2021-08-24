use anyhow::Result;
use log::error;
use nom::{
    error::{convert_error, VerboseError},
    Err,
};
use std::path::PathBuf;
use stderrlog;
use structopt::StructOpt;

/// flatzinc parser
#[derive(StructOpt, Debug)]
#[structopt(name = "fz-parser")]
struct Opt {
    /// Input in flatzinc format
    #[structopt(short = "i", long = "input", parse(from_os_str))]
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

    let opt = Opt::from_args();
    let buf = std::fs::read_to_string(opt.file)?;
    for line in buf.lines() {
        match flatzinc::statement::<VerboseError<&str>>(&line) {
            Ok((_, result)) => println!("{:#?}", result),
            Err(Err::Error(e)) => {
                let bla = convert_error(buf.as_str(), e);
                error!("Failed to parse flatzinc!\n{}", bla)
            }
            Err(e) => error!("Failed to parse flatzinc: {:?}", e),
        }
    }
    Ok(())
}

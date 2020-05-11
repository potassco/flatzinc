use log::error;
use nom::error::{convert_error, VerboseError};
use nom::Err;
use std::path::PathBuf;
use stderrlog;

use anyhow::Result;
use structopt::StructOpt;

/// flatzinc parser
#[derive(StructOpt, Debug)]
#[structopt(name = "fz-parser")]
struct Opt {
    /// Input in flatzinc format
    #[structopt(short = "i", long = "input", parse(from_os_str))]
    file: PathBuf,
}

fn main() -> Result<()> {
    stderrlog::new()
        .module(module_path!())
        .verbosity(2)
        .init()
        .unwrap();

    let opt = Opt::from_args();
    let buf = std::fs::read_to_string(opt.file)?;
    match flatzinc::model::<VerboseError<&str>>(&buf) {
        Ok((_, result)) => println!("{:#?}", result),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            error!("Failed to parse flatzinc!\n{}", convert_error(&buf, e))
        }
        Err(e) => error!("Failed to parse flatzinc: {:?}", e),
    }
    Ok(())
}

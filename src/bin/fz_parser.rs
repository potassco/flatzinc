use nom::error::{convert_error, VerboseError};
use nom::Err;
use std::path::Path;

fn main() {
    let file = Path::new("jobshop.fzn");
    let buf = std::fs::read_to_string(file).unwrap();
    match flatzinc::model::<VerboseError<&str>>(&buf) {
        Ok((_, result)) => println!("{:?}", result),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            println!("Failed to parse flatzinc!\n{}", convert_error(&buf, e))
        }
        Err(e) => println!("Failed to parse flatzinc: {:?}", e),
    }
}

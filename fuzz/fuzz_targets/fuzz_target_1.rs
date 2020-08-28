#![no_main]
extern crate nom;
use libfuzzer_sys::fuzz_target;
pub use nom::{
    error::{convert_error, VerboseError},
    Err,
};
use std::str;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(utf8_str) = str::from_utf8(data) {
        match flatzinc::model::<VerboseError<&str>>(utf8_str) {
            Ok((_, result)) => println!("{:#?}", result),
            Err(Err::Error(_e)) | Err(Err::Failure(_e)) => {
                // print!("Failed to parse flatzinc!\n{}", convert_error(utf8_str, _e))
            }
            Err(e) => print!("Failed to parse flatzinc: {:?}", e),
        }
    } else {
        // print!("No valid utf8!")
    }
});

#![no_main]
extern crate winnow;
use libfuzzer_sys::fuzz_target;
pub use winnow::error::ContextError;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(mut utf8_str) = std::str::from_utf8(data) {
        match flatzinc::statement::<ContextError>(&mut utf8_str) {
            Ok(result) => println!("{:#?}", result),
            Err(e) => print!("Failed to parse flatzinc: {:?}", e),
        }
    } else {
        // print!("No valid utf8!")
    }
});

#![no_main]
extern crate winnow;
use flatzinc::Stmt;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(utf8_str) = std::str::from_utf8(data) {
        match <Stmt as std::str::FromStr>::from_str(utf8_str) {
            Ok(result) => println!("{}\n{:#?}", utf8_str, result),
            Err(_) => print!(" Failed to parse test data."),
        }
    } else {
        // print!("No valid utf8!")
    }
});

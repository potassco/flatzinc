# flatzinc parser

A parser for the [FlatZinc](https://www.minizinc.org/doc-2.4.1/en/fzn-spec.html#specification-of-flatzinc) modelling language.

## Compile

```text
❯ cargo build --release
```

## Usage

In your Cargo.toml

```toml
[dependencies]
flatzinc = "0.1"
```

In your code:

```rust
use nom::error::{convert_error, VerboseError};
use nom::Err;
use flatzinc

match flatzinc::model::<VerboseError<&str>>(&buf) {
    Ok((_, result)) => println!("{:#?}", result),
    Err(Err::Error(e)) | Err(Err::Failure(e)) => {
        println!("Failed to parse flatzinc!\n{}", convert_error(&buf, e))
    }
    Err(e) => println!("Failed to parse flatzinc: {:?}", e),
}
```

## fz-parser

An example parser can be found in the `examples/fz-parser.rs`

To run the parser call:

```text
❯ cargo run --example fz-parser -- -i jobshop.fzn
```

The binary can be found under `target/release/examples/fz-parser`

## Other

- [Where is mzn2fzn?](https://github.com/MiniZinc/libminizinc/issues/342)

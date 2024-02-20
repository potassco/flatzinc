# flatzinc [![Build Status](https://github.com/potassco/flatzinc/workflows/CI%20test/badge.svg)](https://github.com/potassco/flatzinc) [![Latest Version](https://img.shields.io/crates/v/flatzinc.svg)](https://crates.io/crates/flatzinc) [![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/flatzinc)

A parser for the [FlatZinc](https://www.minizinc.org/doc-2.4.3/en/fzn-spec.html#specification-of-flatzinc) modelling language version 2.4.3.

## Compile

```text
❯ cargo build --release
```

## Usage

In your `Cargo.toml`:

```toml
[dependencies]
flatzinc = "0.3.20-dev"
```

In your code:

```rust
use flatzinc::*;

match flatzinc::model::<InputError<&str>>(&buf) {
    Ok(result) => println!("{:#?}", result),
    Err(e) => {
        error!("Failed to parse flatzinc!\n{}", e)
    }
}
```

## fz-parser

An example parser can be found in the `examples/fz-parser.rs`.

To run the parser call:

```text
❯ cargo run --example fz-parser -- -i jobshop.fzn
```

The binary can be found under `target/release/examples/fz-parser`.

## FAQ

- [How to create flatzinc from minizinc?](https://github.com/MiniZinc/libminizinc/issues/342)

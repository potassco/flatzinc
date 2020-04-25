# flatzinc parser

[flatzinc specification](https://www.minizinc.org/doc-2.4.1/en/fzn-spec.html#specification-of-flatzinc)

[Where is mzn2fzn?](https://github.com/MiniZinc/libminizinc/issues/342)

## Compile

    cargo build --release

## Usage

```rust
match flatzinc::model::<VerboseError<&str>>(&buf) {
    Ok((_, result)) => println!("{:#?}", result),
    Err(Err::Error(e)) | Err(Err::Failure(e)) => {
        println!("Failed to parse flatzinc!\n{}", convert_error(&buf, e))
    }
    Err(e) => println!("Failed to parse flatzinc: {:?}", e),
}
```

## fz-parser

An example parser can be found in the `target/release/fz-parser`

### Usage

    ❯ fz-parser --help
    fz-parser 0.1.0
    flatzinc parser

    USAGE:
        fz_parser --input <file>

    FLAGS:
        -h, --help       Prints help information
        -V, --version    Prints version information

    OPTIONS:
        -i, --input <file>    Input in flatzinc format

### Example

    ❯ fz-parser -f jobshop.fz

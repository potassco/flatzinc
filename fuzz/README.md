# How to run fuzz tests

1. Install [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz)

    ```sh
    cargo install cargo-fuzz
    ```

2. Run a fuzzing target and find bugs! (needs nighly toolchain)

    ```sh
    cargo +nightly fuzz run fuzz_target_1`
    ```

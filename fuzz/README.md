## Setup

`rustup toolchain install nightly`
`cargo install cargo-fuzz`

## Run the fuzzer

```
cd fuzz
cargo +nightly fuzz run main
```

## Help the fuzzer with initial input

put files inside `fuzz/corpus/main` (clear it before)

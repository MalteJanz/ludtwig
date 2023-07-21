## Setup
`cargo install cargo-fuzz`

## Run the fuzzer
```
cd fuzz
cargo fuzz run main
```

## Help the fuzzer with initial input
put files inside `fuzz/corpus/main` (clear it before)

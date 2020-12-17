# Ludtwig
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig?color=blue&style=flat-square)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/MalteJanz/ludtwig/CI/main?label=CI&logo=GitHub%20Actions&logoColor=%23FFFFFF&style=flat-square)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/MalteJanz/ludtwig?include_prereleases&logo=GitHub&style=flat-square)
![GitHub all releases](https://img.shields.io/github/downloads/MalteJanz/ludtwig/total?logo=GitHub&style=flat-square)

A CLI tool for developers that tries to speedup the development workflow while working with templating (`.twig`) files.
It focuses mainly on formatting the files with a uniform code style and detecting mistakes.
It is only a "formatter" that can be run locally or in the pipeline, it will not report anything to your IDE.

## Status / Disclaimer
Ludtwig is currently in an early development state.
Please use the tool with caution and validate the changes that is has made (backup your files before running this).
Feel free to create new issues and help to steer this project in the right direction.

## Current Features
- Fast / Highly concurrent execution
- Parsing of idiomatic Html / Twig files
  - It does support Vue syntax (including output blocks)
  - It will not parse non idiomatic Html where for example closing tags are forgotten (that otherwise could still work in the browser)
  - Currently only a very limited amount of Twig syntax is supported (Blocks, Parent calls, Comments)
- Write the AST (abstract syntax tree) in a formatted way back into files
- Analyse the AST for common mistakes / suggestions

## Current Use cases
- It is best suited for template files which rely heavily on the block feature of twig (for extensibility)
  - For example the Shopware Administration (uses Vue and Twig.js): https://github.com/shopware/platform
  - Support for more twig syntax is planned
- There is no customization of the formatting or analyzing yet - you should be fine with the provided code style for now (or make the changes for yourself and recompile)

## How to use Ludtwig?
Basically download the latest release build and run the executable from the command line.
Have a look at `ludtwig --help` for more information.

## Basic Concepts
- Every file is parsed concurrently and independent from each other into an AST (abstract syntax tree)
  - Parsing errors will result in not writing / analysing the file
- After the parsing was successful the following happens concurrently with the AST:
  - the writer prints the AST in a formatted way back into a file
  - the analyzer checks the AST for warnings and report them back to the user
  - the analyzer can not influence the writer or the other way around
- After this is done for all files the output is presented to the user

## Structure (subject to change)
- Setup from the CLI arguments happens in `src/main.rs`
- The main function for processing a file is in `src/process.rs`
- Parsing happens inside of the `lib/twig` crate and does happen in serial for each file using parsing combinators.
- Writing logic can be found in `src/writer.rs`
- Analyzing logic can be found in `src/analyzer.rs`
- User CLI Output logic can be found in `src/output.rs`

## Development setup
Make sure you have [Rust](https://www.rust-lang.org/) installed on your system.  
Clone this repository.

Run the project with parameters: `cargo run -- filaA.twig`  
Build the project for production with (output in `target/release/ludtwig`): `cargo build --release`  
Run tests with (only of the application not it's dependencies like twig parser): `cargo test`  

If you want to benchmark the performance of the release build you could use [Hyperfine](https://github.com/sharkdp/hyperfine)
and run it like so:  
`hyperfine -i 'ludtwig -o ./output ./my-template-folder'`

## License
Copyright (c) 2020 Malte Janz  
`ludtwig` is distributed under the terms of the MIT License.  
See the [LICENSE](./LICENSE) file for details.

### Dependencies / License notices*
If you build this project locally or use the distributed binary keep also the following licenses in mind:
- [nom](https://github.com/Geal/nom) - [MIT](https://github.com/Geal/nom/blob/master/LICENSE)
- [tokio](https://github.com/tokio-rs/tokio) - [MIT](https://github.com/tokio-rs/tokio/blob/master/LICENSE)
- [clap](https://github.com/clap-rs/clap) - [MIT](https://github.com/clap-rs/clap/blob/master/LICENSE-MIT) / [Apache 2.0](https://github.com/clap-rs/clap/blob/master/LICENSE-APACHE)
- [ansi_term](https://github.com/ogham/rust-ansi-term) - [MIT](https://github.com/ogham/rust-ansi-term/blob/master/LICENCE)
- [walkdir](https://github.com/BurntSushi/walkdir) - [MIT](https://github.com/BurntSushi/walkdir/blob/master/LICENSE-MIT) / [UNLICENSE](https://github.com/BurntSushi/walkdir/blob/master/UNLICENSE)

If you run the tests / benchmarks locally you also should keep these licenses in mind (not included in distributed binary):
- [criterion](https://github.com/bheisler/criterion.rs) - [MIT](https://github.com/bheisler/criterion.rs/blob/master/LICENSE-MIT) / [Apache 2.0](https://github.com/bheisler/criterion.rs/blob/master/LICENSE-APACHE)

For testing purposes this repository also includes code from the following sources (not included in distributed binary):
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)
- [SwagMigrationAssistant](https://github.com/shopware/SwagMigrationAssistant) - [MIT](https://github.com/shopware/SwagMigrationAssistant/blob/master/LICENSE)

Special thanks goes to the authors of these dependencies.  
*This list and the links may not be up to date and you should do your own research

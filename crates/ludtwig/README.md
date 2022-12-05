# Ludtwig
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig?color=blue&style=flat-square)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/MalteJanz/ludtwig/CI/main?label=CI&logo=GitHub%20Actions&logoColor=%23FFFFFF&style=flat-square)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/MalteJanz/ludtwig?include_prereleases&logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/v/ludtwig?style=flat-square)
![GitHub all releases](https://img.shields.io/github/downloads/MalteJanz/ludtwig/total?logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/d/ludtwig?label=downloads%20crates.io&style=flat-square)

A CLI tool for developers that tries to speed up the development workflow while working with templating (`.twig`) files.
It does so by parsing the files and running a set of rules on them, which report errors and make suggestions
(these can also be applied automatically).

It is only a CLI-Tool that can be run locally or in the pipeline, it will not report anything to your IDE (for now).
It is easy to configure with a config file or environment variables.

## Status / Disclaimer
Ludtwig is currently in an early development state.
Please use the tool with caution and validate the changes that is has made 
(backup your files before running it with the `--fix` option).
Feel free to create new issues and help to steer this project in the right direction.

## Overview
- [Example](https://github.com/MalteJanz/ludtwig#example)
- [Current features](https://github.com/MalteJanz/ludtwig#current-features)
- [Current limitations](https://github.com/MalteJanz/ludtwig#current-limitations)
- [Installation](https://github.com/MalteJanz/ludtwig#installation)
- [How to use Ludtwig?](https://github.com/MalteJanz/ludtwig#how-to-use-ludtwig)
- [Basic Concepts](https://github.com/MalteJanz/ludtwig#basic-concepts)
- [Development setup](https://github.com/MalteJanz/ludtwig#development-setup)
- [License](https://github.com/MalteJanz/ludtwig#license)

## Example
TODO: add after rewrite

## Current features
- Fast + concurrent execution
- Parsing Html / Twig templating files
  - It will report non-idiomatic Html where for example closing tags are forgotten
    (that otherwise could still work in the browser).
    In this case ludtwig produces a helpful error message and tries to recover and parse the remaining template.
- Rule system for analyzing the parsed syntax tree
  - Reports helpfull / rich error messages
  - Allow making text-replace based suggestions which ludtwig can apply automatically
- Configurable by
  - config file
  - environment variables
  - inline comments

## Current limitations
- Twig syntax is still not fully supported (but almost)
- You may encounter edge cases that result in overwhelming amount of parsing errors. Please create issues for them.
- There are not that many rules yet

## Installation
### Cargo (rust toolchain)
Run the following cargo command to install or update ludtwig:
`cargo install ludtwig`  
You can install cargo here: https://www.rust-lang.org/learn/get-started  
If you don't want to install the rust toolchain / cargo you can still use the manual installation below.

### Manual
Download the latest [release binary](https://github.com/MalteJanz/ludtwig/releases) for your operating system and put it in your PATH for easy access.

## How to use Ludtwig?
After the installation have a look at `ludtwig --help` for more information. It should be self-explanatory.
Also have a look at the default config file if you want to customize the way how ludtwig formats your files.

To create it in your current working directory run `ludtwig -C`.
It also contains some documentation for environment and inline configuration,

## Basic Concepts
- TODO: add documentation after the rewrite
- Every file is parsed concurrently and independent of each other into a syntax tree
  - Parsing is completely lossless (all input is preserved)

## Development setup
Make sure you have [Rust](https://www.rust-lang.org/) installed on your system.  
Clone this repository.

Run the project with parameters: `cargo run -- filaA.twig`  
Build the project for production with: `cargo build --release`  
(the produced binary will be here: `target/release/ludtwig`)  
Run tests with: `cargo test`  

If you want to benchmark the performance of the release build you could use [Hyperfine](https://github.com/sharkdp/hyperfine)
and run it like so:  
`hyperfine -i 'ludtwig ./my-template-folder'`

## License
`ludtwig` is distributed under the terms of the MIT License.  
See the [LICENSE](./LICENSE) file for details.

### License notices
For testing purposes this repository may also include code from the following sources (not included in distributed binary):
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)

# Ludtwig
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig?color=blue&style=flat-square)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/MalteJanz/ludtwig/CI/main?label=CI&logo=GitHub%20Actions&logoColor=%23FFFFFF&style=flat-square)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/MalteJanz/ludtwig?include_prereleases&logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/v/ludtwig?style=flat-square)
![GitHub all releases](https://img.shields.io/github/downloads/MalteJanz/ludtwig/total?logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/d/ludtwig?label=downloads%20crates.io&style=flat-square)

A CLI tool for developers that tries to speedup the development workflow while working with templating (`.twig`) files.
It focuses mainly on formatting the files with a uniform code style and detecting mistakes.
It is only a "formatter" that can be run locally or in the pipeline, it will not report anything to your IDE.

## Status / Disclaimer
Ludtwig is currently in an early development state.
Please use the tool with caution and validate the changes that is has made (backup your files before running this).
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
### Before
```twig
{% block my_component %}
    <div class="my-component">
        {% block my_component_header %}
        <h2 :style="style"          class   =   "whitespace-sensitive">{{ header }}</h2>
        {% endblock %}
        {% block my_component_body %}
            <my-component-body class="component-body" v-model="body" @change="if a > 0 { onChange(); }"
                {% if isDisabled %}
                    disabled
                {% elseif hasCustomAttribute %}
                    {{ customAttrName }}="{{ customAttrValue }}"
                {% else %}
                    {{ completeCustomAttr }}
                {% endif %}>
                <charlong9 some="custom" attributes="that" are="at least 3 or make the line long enough"> (whitespaces around this) </charlong9>
                <span>(but here are no whitespaces)</span>
                <p>
                    This is a paragraph.       And the browser sees only whitespace and doesn't care about many spaces.
                    Linebreaks are also no problem. they are kept.

                    The browser also does not care about empty lines.
                    So why not format text like this how the browser displays it (but keep single line breaks for visibility)?
                </p>
            </my-component-body>
        {% endblock %}
    </div>
{% endblock %}
```

### After running ludtwig
```twig
{% block my_component %}
    <div class="my-component">

        {% block my_component_header %}
            <h2 :style="style"
                class="whitespace-sensitive">{{ header }}</h2>
        {% endblock %}

        {% block my_component_body %}
            <my-component-body
                    class="component-body"
                    v-model="body"
                    @change="if a > 0 { onChange(); }"
                    {% if isDisabled %}
                        disabled
                    {% elseif hasCustomAttribute %}
                        {{ customAttrName }}="{{ customAttrValue }}"
                    {% else %}
                        {{ completeCustomAttr }}
                    {% endif %}>
                <charlong9
                        some="custom"
                        attributes="that"
                        are="at least 3 or make the line long enough">
                    (whitespaces around this)
                </charlong9>
                <span>(but here are no whitespaces)</span>
                <p>
                    This is a paragraph.
                    And the browser sees only whitespace and doesn't care about many spaces.
                    Linebreaks are also no problem. they are kept.
                    The browser also does not care about empty lines.
                    So why not format text like this how the browser displays it (but keep single line breaks for visibility)?
                </p>
            </my-component-body>
        {% endblock %}

    </div>
{% endblock %}
```

### It also catches errors / problems during parsing
```text
Parsing files...

File: "./fixtures/showcase.html.twig"
[Error] Parsing goes wrong in line 7 and column 38 :
            <my-component-body class=component-body v-model="body" @change="if a > 0 { onChange(); }"
                                     ^
                                     |
missing '"' quote

Files scanned: 1, Errors: 1, Warnings: 0
Happy bug fixing ;)
```

### And if the parsing succeeds it checks the AST (abstract syntax tree) for mistakes / best practices
```text
Parsing files...

File: "./fixtures/showcase.html.twig"
[Warning] Duplicate twig block name found: 'my_component'

Files scanned: 1, Errors: 0, Warnings: 1
Happy bug fixing ;)
```

## Current features
- Fast / Highly concurrent execution
- Parsing of idiomatic Html / Twig / Vue.js templating files
  - It will not parse non idiomatic Html where for example closing tags are forgotten (that otherwise could still work in the browser).
    In that case ludtwig will produce a helpful error message.
- Write the AST (abstract syntax tree) in a formatted way back into files
- Analyse the AST for common mistakes / suggestions

## Current limitations
- Twig syntax is still not fully supported ([Some hierarchical syntax is missing](https://github.com/MalteJanz/ludtwig/issues/17))
- There is no customization of the formatting or analyzing yet - you should be fine with the provided code style for now (or make the changes for yourself and recompile)
- You may encounter edge cases that result in parsing errors. Please create issues for them.

## Installation
### Cargo (rust toolchain)
Run the following cargo command to install or update ludtwig:
`cargo install ludtwig`  
You can install cargo here: https://www.rust-lang.org/learn/get-started  
If you don't want to install the rust toolchain / cargo you can still use the manual installation below.

### Manual
Download the latest [release binary](https://github.com/MalteJanz/ludtwig/releases) for your operating system and put it in your PATH for easy access.

## How to use Ludtwig?
After installation have a look at `ludtwig --help` for more information. It should be self explanatory.

## Basic Concepts
- Every file is parsed concurrently and independent from each other into an AST (abstract syntax tree)
  - Parsing errors will result in not writing / analysing the file
- After the parsing was successful the following happens concurrently with the AST:
  - the writer prints the AST in a formatted way back into a file
  - the analyzer checks the AST for warnings and report them back to the user
  - the analyzer can not influence the writer or the other way around
- After this is done for all files the output is presented to the user

## Development setup
Make sure you have [Rust](https://www.rust-lang.org/) installed on your system.  
Clone this repository.

Run the project with parameters: `cargo run -- filaA.twig`  
Build the project for production with: `cargo build --release`  
(the produced binary will be here: `target/release/ludtwig`)  
Run tests with: `cargo test`  

If you want to benchmark the performance of the release build you could use [Hyperfine](https://github.com/sharkdp/hyperfine)
and run it like so:  
`hyperfine -i 'ludtwig -o ./output ./my-template-folder'`

## License
Copyright (c) 2020 Malte Janz  
`ludtwig` is distributed under the terms of the MIT License.  
See the [LICENSE](./LICENSE) file for details.

### Dependencies / License notices*
If you build this project locally or use the distributed binary keep also the following licenses in mind:
- [ludtwig-parser](https://github.com/MalteJanz/ludtwig-parser) - [MIT](https://github.com/MalteJanz/ludtwig-parser/blob/main/LICENSE)
- [async-std](https://github.com/async-rs/async-std) - [MIT](https://github.com/async-rs/async-std/blob/master/LICENSE-MIT) / [Apache 2.0](https://github.com/async-rs/async-std/blob/master/LICENSE-APACHE)
- [clap](https://github.com/clap-rs/clap) - [MIT](https://github.com/clap-rs/clap/blob/master/LICENSE-MIT) / [Apache 2.0](https://github.com/clap-rs/clap/blob/master/LICENSE-APACHE)
- [ansi_term](https://github.com/ogham/rust-ansi-term) - [MIT](https://github.com/ogham/rust-ansi-term/blob/master/LICENCE)
- [walkdir](https://github.com/BurntSushi/walkdir) - [MIT](https://github.com/BurntSushi/walkdir/blob/master/LICENSE-MIT) / [UNLICENSE](https://github.com/BurntSushi/walkdir/blob/master/UNLICENSE)
- [async-trait](https://github.com/dtolnay/async-trait) - [MIT](https://github.com/dtolnay/async-trait/blob/master/LICENSE-MIT) / [Apache 2.0](https://github.com/dtolnay/async-trait/blob/master/LICENSE-APACHE)

For testing purposes this repository also includes code from the following sources (not included in distributed binary):
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)
- [SwagMigrationAssistant](https://github.com/shopware/SwagMigrationAssistant) - [MIT](https://github.com/shopware/SwagMigrationAssistant/blob/master/LICENSE)

Special thanks goes to the authors of these dependencies.  
*This list and the links may not be up to date and you should do your own research. Also deeper dependencies are not listed.

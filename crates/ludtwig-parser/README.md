# Ludtwig-Parser
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig-parser?color=blue&style=flat-square)
![Crates.io](https://img.shields.io/crates/v/ludtwig-parser?style=flat-square)
![Crates.io](https://img.shields.io/crates/d/ludtwig-parser?style=flat-square)

Rust crate that parses twig / html templating syntax into a syntax tree.
It does not conform to any spec and the input is required to be as idiomatic as possible.
For example missing closing tags in html result in a parsing error (even if browsers can interpret the html and reconstruct the closing tag).
This makes it possible to represent the template in a hierarchical untyped tree which is easy to navigate.

## Disclaimer
This crate is still in early development and the API can break with any release until it reaches a stable v1.0.0 version (semantic versioning).
It is developed together with the [ludtwig](https://github.com/MalteJanz/ludtwig) CLI application for formatting and analyzing template files.

## License
MIT - see [LICENSE](https://github.com/MalteJanz/ludtwig-parser/blob/main/LICENSE) file.

### License notices
For testing purposes this repository also may include code from the following sources:
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)

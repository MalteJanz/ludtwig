# Ludtwig
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig?color=blue&style=flat-square)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/MalteJanz/ludtwig/CI/main?label=CI&logo=GitHub%20Actions&logoColor=%23FFFFFF&style=flat-square)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/MalteJanz/ludtwig?include_prereleases&logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/v/ludtwig?style=flat-square)
![GitHub all releases](https://img.shields.io/github/downloads/MalteJanz/ludtwig/total?logo=GitHub&style=flat-square)
![Crates.io](https://img.shields.io/crates/d/ludtwig?label=downloads%20crates.io&style=flat-square)

Linter / Formatter for Twig template files which respects HTML and your time.

## Features
- Fast
  - written in Rust
  - works on files concurrently
- Helpful
  - provides rich error messages and suggestions most of the time
- Rule based
  - Rules can annotate syntax and make suggestions
  - Suggestions can be applied automatically with `--fix`
- Configurable
  - Rules can be ignored for the whole file or next line (which ignores the whole next SyntaxNode)
  - `.ludtwig-ignore` to ignore whole files completely (like your `.gitignore`)
  - `ludtwig-config.toml` (use `-C` to create one) to configure the rules for your project / adjust the code style
  - Environment variables can override config values
- The Parser is not HTML Spec compliant, but
  - Almost all Twig syntax is supported
  - all input is parsed into a Syntax tree
  - even invalid syntax does not stop the parsing, and it tries to parse as much valid syntax as possible

## Current limitations
- Twig syntax is still not fully supported
  - `{%- ... -%}` whitespace removal braces are not yet supported
  - Usage of PHP closures as function arguments is currently not supported
- You may encounter other edge cases that result in parsing errors. Please create issues for them.
- The list of rules is still quite small so many things besides the syntax aren't checked / suggested

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
Also have a look at the default config file if you want to customize the way how ludtwig analyses your files.
To create it in your current working directory run `ludtwig -C`.

## Allowed syntax
To prevent the creation of invalid / dirty HTML by Twig ludtwig only allows the Twig syntax in certain places.
Without this restriction it wouldn't be possible to parse the combined syntax in a single hierarchical syntax tree.
Have a look at the example below to get the general idea where Twig syntax is allowed:
```twig
{% block my_component %}
    {% set isActive = true %}
    <div id="my-component"
         class="my-component {% if isLarge %}large{% endif %}"
         {{ dataAttribute }}="data"
         {# Single word attributes don't strictly require quotes #}
         data-active={{ isActive }}
    >
        {% block my_component_inner %}
            <span id="my-span"
                  {% if isActive %}
                      style="color: red"
                  {% endif %}
            >
                hello {{ name }}
            </span>
        {% endblock %}
    </div>
{% endblock %}
```

## License
Copyright (c) 2023 Malte Janz  
`ludtwig` is distributed under the terms of the MIT License.  
See the [LICENSE](./LICENSE) file for details.

For testing purposes this repository also includes code from the following sources (not included in distributed binary):
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)

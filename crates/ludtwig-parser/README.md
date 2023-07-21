# Ludtwig-Parser
![GitHub](https://img.shields.io/github/license/MalteJanz/ludtwig-parser?color=blue&style=flat-square)
![Crates.io](https://img.shields.io/crates/v/ludtwig-parser?style=flat-square)
![Crates.io](https://img.shields.io/crates/d/ludtwig-parser?style=flat-square)

Rust crate that parses Twig / HTML templating syntax into a lossless syntax tree.
It does not conform to the official HTML spec and the input is required to be as idiomatic as possible (but the parser still tries to recover from errors / parse as much as possible).
For example missing closing tags in HTML result in a parsing error (even if browsers can interpret the HTML and reconstruct the closing tag).
This makes it possible to represent the template in a hierarchical untyped tree which is easy to navigate and contains both Twig and HTML syntax.

## Accepted syntax example
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

## Disclaimer
It is developed together with the [ludtwig](https://github.com/MalteJanz/ludtwig) CLI application for formatting and analyzing Twig template files.

## License
MIT - see [LICENSE](https://github.com/MalteJanz/ludtwig/blob/main/crates/ludtwig-parser/LICENSE) file.

### License notices
For testing purposes this repository also may include code from the following sources:
- [Shopware](https://github.com/shopware/platform) - [MIT](https://github.com/shopware/platform/blob/master/LICENSE)

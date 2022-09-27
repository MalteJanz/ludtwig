# Twig code style (shopware storefront)
Author: Tobias Berge @tobiasberge

## Prefer sw_include

```diff
- {% include '@Storefront/storefront/utilities/alert.html.twig' %}
+ {% sw_include '@Storefront/storefront/utilities/alert.html.twig' %}
```

## Prefer sw_extends

```diff
- {% extends '@Storefront/storefront/component/product/card/box-standard.html.twig' %}
+ {% sw_extends '@Storefront/storefront/component/product/card/box-standard.html.twig' %}
```

## Prefer single quotes for twig strings

```diff
- {% sw_include "@Storefront/test.html.twig" %}
+ {% sw_include '@Storefront/test.html.twig' %}
```

## Prefer double quotes for HTML attributes

```diff
- <div title='Lorem ipsum'></div>
+ <div title="Lorem ipsum"></div>
```

## Prefer no quotes when not needed

```diff
{% sw_include '@Storefront/test.html.twig' with {
-    'address': context.customer.defaultShippingAddress
+    address: context.customer.defaultShippingAddress
} %}
```

## Prefer camelCase for variables

```diff
- {% set default_shipping_address_id = context.customer.defaultShippingAddressId %}
+ {% set defaultShippingAddressId = context.customer.defaultShippingAddressId %}
```

## Prefer snake_case for block names

```diff
- {% block componentProductBoxContent %}
+ {% block component_product_box_content %}
```

## Prefer indentation inside enclosing code blocks

```diff
{% block component_product_box_content %}
-<div class="box"></div>
+    <div class="box"></div>
{% endblock %}

{% if someCondition %}
-<div class="box-actions"></div>
+    <div class="box-actions"></div>
{% endblock %}
```

## Prefer self-closing HTML tags

```diff
- <br>
+ <br />
```

## Prefer shorthand boolean attributes

```diff
- <input id="search" disabled="true" />
+ <input id="search" disabled />
```

## Prefer twig comment syntax

```diff
- <!-- This should not be touched -->
+ {# This should not be touched #}
```
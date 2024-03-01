use ludtwig_parser::syntax::typed::{AstNode, TwigBlock};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode, TextRange, TextSize};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigBlockLineBreaks;

impl Rule for RuleTwigBlockLineBreaks {
    fn name(&self) -> &'static str {
        "twig-block-line-breaks"
    }

    #[allow(clippy::too_many_lines)]
    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        if ctx.traversal_ctx().inside_trivia_sensitive_node {
            return None; // no trivia modification allowed here
        }

        let block = TwigBlock::cast(node)?;

        // early return if parent is the root
        if block.syntax().parent().map_or(true, |may_be_body| {
            matches!(may_be_body.kind(), SyntaxKind::ROOT)
        }) {
            return None;
        }
        let next_sibling = block.syntax().next_sibling();

        // find first token of twig block (ideally a line break)
        let starting_block = block.starting_block()?;
        let prev_sibling = block.syntax().prev_sibling();
        let starting_syntax = match prev_sibling {
            Some(ref n)
                if matches!(
                    n.kind(),
                    SyntaxKind::TWIG_COMMENT
                        | SyntaxKind::HTML_COMMENT
                        | SyntaxKind::LUDTWIG_DIRECTIVE_FILE_IGNORE
                        | SyntaxKind::LUDTWIG_DIRECTIVE_IGNORE
                ) =>
            {
                // use comment before the twig block as starting point if it exists
                n
            }
            _ => starting_block.syntax(),
        };
        let first_child_token = starting_syntax.first_token();

        // don't consider comments as previous siblings
        let prev_sibling = prev_sibling.and_then(|n| {
            if matches!(
                n.kind(),
                SyntaxKind::TWIG_COMMENT
                    | SyntaxKind::HTML_COMMENT
                    | SyntaxKind::LUDTWIG_DIRECTIVE_FILE_IGNORE
                    | SyntaxKind::LUDTWIG_DIRECTIVE_IGNORE
            ) {
                n.prev_sibling()
            } else {
                Some(n)
            }
        });

        // find first token after the twig block (ideally a line break)
        // set to None if next sibling is also a block (which also places linebreaks before)
        let ending_block = block.ending_block()?;
        let after_block_token = ending_block
            .syntax()
            .last_token()
            .and_then(|t| t.next_token())
            .filter(|t| match t.parent() {
                // return no token if the parent is also another twig block
                Some(p) if p.kind() == SyntaxKind::TWIG_STARTING_BLOCK => false,
                _ => true,
            });

        let expected_line_break = ctx.config().format.line_ending.corresponding_string();
        let config_line_break_amount = if ctx.config().format.linebreaks_around_blocks {
            2
        } else {
            1
        };
        let before_line_break_amount = match prev_sibling {
            Some(_) => config_line_break_amount,
            None => {
                // check if there is at least one next sibling
                if next_sibling.is_some() {
                    config_line_break_amount
                } else {
                    1 // nested block inside of block, no line break needed
                }
            }
        };
        let after_line_break_amount = match next_sibling {
            Some(_) => config_line_break_amount,
            None => {
                // check if there is at least one prev sibling
                if prev_sibling.is_some() {
                    config_line_break_amount
                } else {
                    1 // nested block inside of block, no line break needed
                }
            }
        };
        let before_expected_str = expected_line_break.repeat(before_line_break_amount);
        let after_expected_str = expected_line_break.repeat(after_line_break_amount);

        let validate_iter = [
            (
                first_child_token,
                before_expected_str,
                before_line_break_amount,
            ),
            (
                after_block_token,
                after_expected_str,
                after_line_break_amount,
            ),
        ]
        .into_iter()
        .filter_map(|(may_be_token, expected_str, line_break_amount)| {
            may_be_token.map(|token| (token, expected_str, line_break_amount))
        });

        let mut results = vec![];
        for (token, expected_str, line_break_amount) in validate_iter {
            if token.kind() == SyntaxKind::TK_LINE_BREAK {
                // validate existing line break
                if token.text() != expected_str {
                    let result = self
                        .create_result(Severity::Help, "Wrong line break around block")
                        .primary_note(
                            token.text_range(),
                            format!("Expected {line_break_amount} line breaks here"),
                        )
                        .suggestion(
                            token.text_range(),
                            expected_str.clone(),
                            format!("Change to {line_break_amount} line breaks"),
                        );

                    results.push(result);
                }
            } else {
                let range = TextRange::at(token.text_range().start(), TextSize::from(0));

                // missing line break
                let result = self
                    .create_result(Severity::Help, "Missing line break around block")
                    .primary_note(
                        range,
                        format!("Expected {line_break_amount} line breaks before this"),
                    )
                    .suggestion(
                        range,
                        expected_str.clone(),
                        format!("Add {line_break_amount} line breaks before this"),
                    );

                results.push(result);
            }
        }

        if results.is_empty() {
            None
        } else {
            Some(results)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-block-line-breaks",
            "{% block my_block %}
    <div cla-ss=\"my-div\">
        {% block inner_a %}
            hello
        {% endblock %}
        {% block inner_b %}
            world
        {% endblock %}
        <hr/>
        {% block inner_c %}
            {% block inner_c_inner %}
                abc
            {% endblock %}
        {% endblock %}
    </div>
{% endblock %}",
            expect![[r#"
                help[twig-block-line-breaks]: Wrong line break around block
                  ┌─ ./debug-rule.html.twig:2:26
                  │    
                2 │         <div cla-ss="my-div">
                  │ ╭───────────────────────────^
                  │ │ ╭─────────────────────────'
                3 │ │ │         {% block inner_a %}
                  │ ╰─│^ Expected 2 line breaks here
                  │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                  ┌─ ./debug-rule.html.twig:5:23
                  │    
                5 │             {% endblock %}
                  │ ╭────────────────────────^
                  │ │ ╭──────────────────────'
                6 │ │ │         {% block inner_b %}
                  │ ╰─│^ Expected 2 line breaks here
                  │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                  ┌─ ./debug-rule.html.twig:8:23
                  │    
                8 │             {% endblock %}
                  │ ╭────────────────────────^
                  │ │ ╭──────────────────────'
                9 │ │ │         <hr/>
                  │ ╰─│^ Expected 2 line breaks here
                  │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:9:14
                   │    
                 9 │             <hr/>
                   │ ╭───────────────^
                   │ │ ╭─────────────'
                10 │ │ │         {% block inner_c %}
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:14:23
                   │    
                14 │             {% endblock %}
                   │ ╭────────────────────────^
                   │ │ ╭──────────────────────'
                15 │ │ │     </div>
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-block-line-breaks",
            "{% block my_block %}
    <div cla-ss=\"my-div\">
        {% block inner_a %}
            hello
        {% endblock %}
        {% block inner_b %}
            world
        {% endblock %}
        <hr/>
        {% block inner_c %}
            {% block inner_c_inner %}
                abc
            {% endblock %}
        {% endblock %}
    </div>
{% endblock %}",
            expect![[r#"
                {% block my_block %}
                    <div cla-ss="my-div">

                        {% block inner_a %}
                            hello
                        {% endblock %}

                        {% block inner_b %}
                            world
                        {% endblock %}

                        <hr/>

                        {% block inner_c %}
                            {% block inner_c_inner %}
                                abc
                            {% endblock %}
                        {% endblock %}

                    </div>
                {% endblock %}"#]],
        );
    }

    #[test]
    fn rule_should_report_empty_line_in_nesting_with_siblings() {
        test_rule(
            "twig-block-line-breaks",
            r#"{% block swag_customized_products_description %}
                    {% block swag_customized_products_description_image %}
                        {% block swag_customized_products_description_image_img %}
                            <img class="swag-customized-products__image img-fluid"
                                 src="{{ customizedProductsTemplate.media|sw_encode_media_url }}"
                                 alt="{{ customizedProductsTemplate.translated.displayName }}"/>
                        {% endblock %}
                        {% block swag_customized_products_description_image_sibling %}
                            <div>
                                some other sibling
                            </div>
                        {% endblock %}
                    {% endblock %}
                {% endblock %}"#,
            expect![[r#"
                help[twig-block-line-breaks]: Wrong line break around block
                  ┌─ ./debug-rule.html.twig:2:75
                  │    
                2 │                         {% block swag_customized_products_description_image %}
                  │ ╭────────────────────────────────────────────────────────────────────────────^
                  │ │ ╭──────────────────────────────────────────────────────────────────────────'
                3 │ │ │                         {% block swag_customized_products_description_image_img %}
                  │ ╰─│^ Expected 2 line breaks here
                  │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                  ┌─ ./debug-rule.html.twig:7:39
                  │    
                7 │                             {% endblock %}
                  │ ╭────────────────────────────────────────^
                  │ │ ╭──────────────────────────────────────'
                8 │ │ │                         {% block swag_customized_products_description_image_sibling %}
                  │ ╰─│^ Expected 2 line breaks here
                  │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:12:39
                   │    
                12 │                             {% endblock %}
                   │ ╭────────────────────────────────────────^
                   │ │ ╭──────────────────────────────────────'
                13 │ │ │                     {% endblock %}
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



            "#]],
        );
    }

    #[test]
    fn rule_should_report_empty_line_in_nesting_with_siblings_fixes() {
        test_rule_fix(
            "twig-block-line-breaks",
            r#"{% block swag_customized_products_description %}
                    {% block swag_customized_products_description_image %}
                        {% block swag_customized_products_description_image_img %}
                            <img class="swag-customized-products__image img-fluid"
                                 src="{{ customizedProductsTemplate.media|sw_encode_media_url }}"
                                 alt="{{ customizedProductsTemplate.translated.displayName }}"/>
                        {% endblock %}
                        {% block swag_customized_products_description_image_sibling %}
                            <div>
                                some other sibling
                            </div>
                        {% endblock %}
                    {% endblock %}
                {% endblock %}"#,
            expect![[r#"
                {% block swag_customized_products_description %}
                                    {% block swag_customized_products_description_image %}

                                        {% block swag_customized_products_description_image_img %}
                                            <img class="swag-customized-products__image img-fluid"
                                                 src="{{ customizedProductsTemplate.media|sw_encode_media_url }}"
                                                 alt="{{ customizedProductsTemplate.translated.displayName }}"/>
                                        {% endblock %}

                                        {% block swag_customized_products_description_image_sibling %}
                                            <div>
                                                some other sibling
                                            </div>
                                        {% endblock %}

                                    {% endblock %}
                                {% endblock %}"#]],
        );
    }

    #[test]
    fn rule_does_not_report_trivia_sensitive() {
        test_rule(
            "twig-block-line-breaks",
            r#"<pre>
    {% block inner_a %}
        hello
    {% endblock %}
    {% block inner_b %}
        world
        
    {% endblock %}
            </pre>
            <pre>
                <code>
    {% block inner_a %}
        hello
    {% endblock %}
    {% block inner_b %}
        world
        
    {% endblock %}
                </code>
            </pre>
            <textarea>
    {% block inner_a %}
        hello
    {% endblock %}
    {% block inner_b %}
        world
        
    {% endblock %}
            </textarea>
            <textarea>
                <p>
    {% block inner_a %}
        hello
    {% endblock %}
    {% block inner_b %}
        world
        
    {% endblock %}
                </p>
            </textarea>
<div>
    {% block inner_a %}
        hello
    {% endblock %}
    {% block inner_b %}
        world
    {% endblock %}
</div>"#,
            expect![[r#"
                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:41:6
                   │    
                41 │     <div>
                   │ ╭───────^
                   │ │ ╭─────'
                42 │ │ │     {% block inner_a %}
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:44:19
                   │    
                44 │         {% endblock %}
                   │ ╭────────────────────^
                   │ │ ╭──────────────────'
                45 │ │ │     {% block inner_b %}
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



                help[twig-block-line-breaks]: Wrong line break around block
                   ┌─ ./debug-rule.html.twig:47:19
                   │    
                47 │         {% endblock %}
                   │ ╭────────────────────^
                   │ │ ╭──────────────────'
                48 │ │ │ </div>
                   │ ╰─│^ Expected 2 line breaks here
                   │   ╰' Change to 2 line breaks: 



            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_on_nested_blocks() {
        test_rule(
            "twig-block-line-breaks",
            r#"
                {% block swag_customized_products_description %}
                    {% block swag_customized_products_description_image %}
                        {% if customizedProductsTemplate.media %}
                            <div class="swag-customized-products__image">
                                <img class="swag-customized-products__image img-fluid"
                                     src="{{ customizedProductsTemplate.media|sw_encode_media_url }}"
                                     alt="{{ customizedProductsTemplate.translated.displayName }}"/>
                            </div>
                        {% endif %}
                    {% endblock %}
                {% endblock %}
        "#,
            expect![r#""#],
        );
    }

    #[test]
    fn rule_does_not_report_on_nested_blocks_with_comments() {
        test_rule(
            "twig-block-line-breaks",
            r#"
                {% block swag_customized_products_description %}
                    {# @deprecated tag:v6.7.0 - Block will be removed. #}
                    {% block swag_customized_products_description_image %}
                        {% if customizedProductsTemplate.media %}
                            <div class="swag-customized-products__image">
                                <img class="swag-customized-products__image img-fluid"
                                     src="{{ customizedProductsTemplate.media|sw_encode_media_url }}"
                                     alt="{{ customizedProductsTemplate.translated.displayName }}"/>
                            </div>
                        {% endif %}
                    {% endblock %}
                {% endblock %}
        "#,
            expect![r#""#],
        );
    }

    #[test]
    fn rule_does_not_report_inside_attribute_string() {
        test_rule(
            "twig-block-line-breaks",
            r#"
                <div class="{% block my_string_block %}hello{% endblock %}">
                </div>
        "#,
            expect![r#""#],
        );
    }
}

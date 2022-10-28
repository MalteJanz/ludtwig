use ludtwig_parser::syntax::typed::{AstNode, TwigBlock};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode, TextRange, TextSize};

use crate::check::rule::{Rule, RuleContext, Severity};

pub struct RuleTwigBlockLineBreaks;

impl Rule for RuleTwigBlockLineBreaks {
    fn name(&self) -> &'static str {
        "twig-block-line-breaks"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let block = TwigBlock::cast(node)?;

        // early return if parent is the root
        if block.syntax().parent().map_or(true, |may_be_body| {
            matches!(may_be_body.kind(), SyntaxKind::ROOT)
        }) {
            return None;
        }

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

        // find first token after the twig block (ideally a line break)
        // set to None if next sibling is also a block (which also places linebreaks before)
        let ending_block = block.ending_block()?;
        let after_block_token = ending_block
            .syntax()
            .last_token()
            .and_then(|t| t.next_token())
            .filter(|t| match t.parent() {
                None => true,
                // return no token if the parent is also another twig block
                Some(p) if p.kind() == SyntaxKind::TWIG_STARTING_BLOCK => false,
                _ => true,
            });

        let expected_line_break = ctx.config().format.line_ending.corresponding_string();
        let config_line_break_amount = match ctx.config().format.linebreaks_around_blocks {
            true => 2,
            false => 1,
        };
        let before_line_break_amount = match prev_sibling {
            Some(_) => config_line_break_amount,
            None => 1,
        };
        let after_line_break_amount = match block.syntax().next_sibling() {
            Some(_) => config_line_break_amount,
            None => 1,
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

        for (token, expected_str, line_break_amount) in validate_iter {
            match token.kind() {
                SyntaxKind::TK_LINE_BREAK => {
                    // validate existing line break
                    if token.text() != expected_str {
                        let result = ctx
                            .create_result(
                                self.name(),
                                Severity::Help,
                                "Wrong line break around block",
                            )
                            .primary_note(
                                token.text_range(),
                                format!("Expected {} line breaks here", line_break_amount),
                            )
                            .suggestion(
                                token.text_range(),
                                expected_str.clone(),
                                format!("Change to {} line breaks", line_break_amount),
                            );
                        ctx.add_result(result);
                    }
                }
                _ => {
                    let range = TextRange::at(token.text_range().start(), TextSize::from(0));

                    // missing line break
                    let result = ctx
                        .create_result(
                            self.name(),
                            Severity::Help,
                            "Missing line break around block",
                        )
                        .primary_note(
                            range,
                            format!("Expected {} line breaks before this", line_break_amount),
                        )
                        .suggestion(
                            range,
                            expected_str.clone(),
                            format!("Add {} line breaks before this", line_break_amount),
                        );
                    ctx.add_result(result);
                }
            }
        }

        None
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
}

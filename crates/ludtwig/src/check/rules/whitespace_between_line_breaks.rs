use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken};

use crate::check::rule::{Rule, RuleContext, Severity};

#[derive(Clone)]
pub struct RuleWhitespaceBetweenLineBreaks;

impl Rule for RuleWhitespaceBetweenLineBreaks {
    fn name(&self) -> &'static str {
        "whitespace-between-line-breaks"
    }

    fn check_token(&self, token: SyntaxToken, ctx: &mut RuleContext) -> Option<()> {
        // rule only inspects line breaks
        if token.kind() != SyntaxKind::TK_LINE_BREAK {
            return None;
        }

        let may_be_ws = token.next_token()?;
        if may_be_ws.kind() != SyntaxKind::TK_WHITESPACE {
            return None;
        }

        let may_be_another_lb = may_be_ws.next_token()?;
        if may_be_another_lb.kind() != SyntaxKind::TK_LINE_BREAK {
            return None;
        }

        let result = ctx
            .create_result(
                self.name(),
                Severity::Warning,
                "Whitespace between line breaks",
            )
            .primary_note(
                may_be_ws.text_range(),
                "Unexpected whitespace on empty line",
            )
            .suggestion(may_be_ws.text_range(), "", "Remove whitespace");
        ctx.add_result(result);

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
            "whitespace-between-line-breaks",
            "{% block my_block %}
\t
            <hr/>
{% endblock %}",
            expect![[r#"
                warning[whitespace-between-line-breaks]: Whitespace between line breaks
                  ┌─ ./debug-rule.html.twig:2:1
                  │
                2 │     
                  │ ^^^^
                  │ │   
                  │ Unexpected whitespace on empty line
                  │ Remove whitespace: 

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "whitespace-between-line-breaks",
            "{% block my_block %}
\t
            <hr/>
{% endblock %}",
            expect![[r#"
                {% block my_block %}

                            <hr/>
                {% endblock %}"#]],
        );
    }
}

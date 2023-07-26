use ludtwig_parser::syntax::typed::{AstNode, TwigBinaryExpression};
use ludtwig_parser::syntax::untyped::SyntaxNode;
use ludtwig_parser::T;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigUseIsSameAs;

impl Rule for RuleTwigUseIsSameAs {
    fn name(&self) -> &'static str {
        "twig-use-is-same-as"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let binary = TwigBinaryExpression::cast(node)?;
        let op = binary.operator()?;

        if op.kind() != T!["==="] {
            return None;
        }

        let mut result = self
            .create_result(Severity::Error, "=== is not a valid twig operator")
            .primary_note(
                op.text_range(),
                "This is not a valid Twig operator, try 'is same as(condition)' instead",
            );

        if let Some(rhs) = binary.rhs_expression() {
            result = result.suggestion(
                op.text_range().cover(rhs.syntax().text_range()),
                format!("is same as({})", rhs.syntax().text().to_string().trim()),
                "Try this instead",
            );
        }

        Some(vec![result])
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn rule_reports_strict_comparison() {
        test_rule(
            "twig-use-is-same-as",
            "{% if a === 5 %}hello{% endif %}",
            expect![[r#"
                error[twig-use-is-same-as]: === is not a valid twig operator
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ {% if a === 5 %}hello{% endif %}
                  │         ^^^--
                  │         │
                  │         Try this instead: is same as(5)
                  │         This is not a valid Twig operator, try 'is same as(condition)' instead

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-use-is-same-as",
            "{% if test === false %}{% endif %}",
            expect!["{% if test is same as(false) %}{% endif %}"],
        );
    }

    #[test]
    fn rule_fixes_complex() {
        test_rule_fix(
            "twig-use-is-same-as",
            "{% if test === 42 + 4 * 2 %}{% endif %}",
            expect!["{% if test is same as(42 + 4 * 2) %}{% endif %}"],
        );
    }
}

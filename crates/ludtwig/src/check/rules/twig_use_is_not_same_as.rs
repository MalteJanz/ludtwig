use crate::check::rule::{CheckResult, RuleExt, RuleRunContext};
use crate::{Rule, Severity};
use ludtwig_parser::syntax::typed::{AstNode, TwigBinaryExpression};
use ludtwig_parser::syntax::untyped::SyntaxNode;
use ludtwig_parser::T;

pub struct RuleTwigUseIsNotSameAs;

impl Rule for RuleTwigUseIsNotSameAs {
    fn name(&self) -> &'static str {
        "twig-use-is-not-same-as"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let expression = TwigBinaryExpression::cast(node)?;
        let op = expression.operator()?;

        if op.kind() != T!["!=="] {
            return None;
        }

        let mut result = self
            .create_result(Severity::Error, "!== is not a valid twig operator")
            .primary_note(
                op.text_range(),
                "This is not a valid Twig operator, try 'is not same as(condition)' instead",
            );

        if let Some(rhs) = expression.rhs_expression() {
            result = result.suggestion(
                op.text_range().cover(rhs.syntax().text_range()),
                format!("is not same as({})", rhs.syntax().text().to_string().trim()),
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
    fn rule_reports() {
        test_rule(
            "twig-use-is-not-same-as",
            "{% if test !== false %}{% endif %}",
            expect![[r#"
                error[twig-use-is-not-same-as]: !== is not a valid twig operator
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ {% if test !== false %}{% endif %}
                  │            ^^^------
                  │            │
                  │            Try this instead: is not same as(false)
                  │            This is not a valid Twig operator, try 'is not same as(condition)' instead

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-use-is-not-same-as",
            "{% if test !== false %}{% endif %}",
            expect!["{% if test is not same as(false) %}{% endif %}"],
        );
    }
}

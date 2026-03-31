use ludtwig_parser::T;
use ludtwig_parser::syntax::typed::{
    AstNode, TwigBinaryExpression, TwigExpression, TwigFunctionCall, TwigLiteralName, support,
};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigValidTest;

impl Rule for RuleTwigValidTest {
    fn name(&self) -> &'static str {
        "twig-valid-test"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let binary_expr = TwigBinaryExpression::cast(node)?;
        let operator = binary_expr.operator()?;

        // Only handle `is` and `is not` operators
        if operator.kind() != T!["is"] && operator.kind() != T!["is not"] {
            return None;
        }

        let rhs = binary_expr.rhs_expression()?;
        let name_token = extract_test_name(&rhs)?;
        let test_name = name_token.text();

        let valid_tests = &ctx.config().twig.valid_tests;

        if !valid_tests.iter().any(|t| t == test_name) {
            let result = self
                .create_result(
                    Severity::Warning,
                    format!("unknown twig test '{test_name}'"),
                )
                .primary_note(
                    name_token.text_range(),
                    "this test is not in the valid-tests config",
                );

            return Some(vec![result]);
        }

        None
    }
}

/// Extract the test name token from the RHS expression of an `is` / `is not` expression.
/// The RHS can be a plain `TwigLiteralName` or a `TwigFunctionCall` (for tests with args).
fn extract_test_name(rhs: &TwigExpression) -> Option<ludtwig_parser::syntax::untyped::SyntaxToken> {
    // Try plain name first (e.g. `is defined`, `is even`)
    if let Some(name_node) = support::child::<TwigLiteralName>(rhs.syntax()) {
        return name_node.get_name();
    }

    // Try function call form (e.g. `is same as(false)`, `is divisible by(3)`)
    if let Some(func_call) = support::child::<TwigFunctionCall>(rhs.syntax()) {
        let name_operand = func_call.name_operand()?;
        let name_node: TwigLiteralName = support::child(name_operand.syntax())?;
        return name_node.get_name();
    }

    None
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn rule_does_not_report_known_test() {
        test_rule(
            "twig-valid-test",
            "{% if foo is defined %}{% endif %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_known_test_is_not() {
        test_rule(
            "twig-valid-test",
            "{% if foo is not empty %}{% endif %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_same_as() {
        test_rule(
            "twig-valid-test",
            "{{ foo is same as(false) }}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_divisible_by() {
        test_rule(
            "twig-valid-test",
            "{{ foo is divisible by(3) }}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_unknown_test() {
        test_rule(
            "twig-valid-test",
            "{% if foo is unknown_test %}{% endif %}",
            expect![[r#"
                warning[twig-valid-test]: unknown twig test 'unknown_test'
                  ┌─ ./debug-rule.html.twig:1:14
                  │
                1 │ {% if foo is unknown_test %}{% endif %}
                  │              ^^^^^^^^^^^^ this test is not in the valid-tests config

            "#]],
        );
    }

    #[test]
    fn rule_reports_unknown_test_is_not() {
        test_rule(
            "twig-valid-test",
            "{% if foo is not unknown_test %}{% endif %}",
            expect![[r#"
                warning[twig-valid-test]: unknown twig test 'unknown_test'
                  ┌─ ./debug-rule.html.twig:1:18
                  │
                1 │ {% if foo is not unknown_test %}{% endif %}
                  │                  ^^^^^^^^^^^^ this test is not in the valid-tests config

            "#]],
        );
    }
}

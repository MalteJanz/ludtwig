use ludtwig_parser::syntax::typed::{AstNode, TwigBinaryExpression};
use ludtwig_parser::syntax::untyped::SyntaxNode;
use ludtwig_parser::T;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigLogicAnd;

impl Rule for RuleTwigLogicAnd {
    fn name(&self) -> &'static str {
        "twig-logic-and"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let binary_expr_op = TwigBinaryExpression::cast(node)?.operator()?;

        if binary_expr_op.kind() == T!["&&"] {
            // not a valid twig operator
            let result = self
                .create_result(Severity::Error, "'&&' is not a valid twig operator")
                .primary_note(binary_expr_op.text_range(), "help: change this operator")
                .suggestion(
                    binary_expr_op.text_range(),
                    "and",
                    "Try this operator instead",
                );

            return Some(vec![result]);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-logic-and",
            "{% if a == 5 && b %}hello{% endif %}",
            expect![[r#"
                error[twig-logic-and]: '&&' is not a valid twig operator
                  ┌─ ./debug-rule.html.twig:1:14
                  │
                1 │ {% if a == 5 && b %}hello{% endif %}
                  │              ^^
                  │              │
                  │              help: change this operator
                  │              Try this operator instead: and

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-logic-and",
            "{% if a == 5 && b %}hello{% endif %}",
            expect!["{% if a == 5 and b %}hello{% endif %}"],
        );
    }
}

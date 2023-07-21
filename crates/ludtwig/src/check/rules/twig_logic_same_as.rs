use ludtwig_parser::syntax::typed::{AstNode, TwigBinaryExpression};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigLogicSameAs;

impl Rule for RuleTwigLogicSameAs {
    fn name(&self) -> &'static str {
        "twig-logic-same-as"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let binary = TwigBinaryExpression::cast(node.clone())?;
        let binary_expr_op = binary.operator()?;

        if binary_expr_op.kind() == SyntaxKind::TK_TRIPLE_EQUAL
            || binary_expr_op.kind() == SyntaxKind::TK_DOUBLE_EQUAL
        {
            let result = self
                .create_result(Severity::Help, "Don't use '===' or '=='")
                .primary_note(
                    binary_expr_op.text_range(),
                    "help: change this operator; try to use 'same as(...)' instead",
                );
            // TODO: add suggestion to change to 'same as(...)'

            return Some(vec![result]);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::test_rule;
    use expect_test::expect;

    #[test]
    fn rule_reports_simple_comparison() {
        test_rule(
            "twig-logic-same-as",
            "{% if a == 5 %}hello{% endif %}",
            expect![[r#"
                help[twig-logic-same-as]: Don't use '===' or '=='
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ {% if a == 5 %}hello{% endif %}
                  │         ^^ help: change this operator; try to use 'same as(...)' instead

            "#]],
        );
    }

    #[test]
    fn rule_reports_strict_comparison() {
        test_rule(
            "twig-logic-same-as",
            "{% if a === 5 %}hello{% endif %}",
            expect![[r#"
                help[twig-logic-same-as]: Don't use '===' or '=='
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ {% if a === 5 %}hello{% endif %}
                  │         ^^^ help: change this operator; try to use 'same as(...)' instead

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        // TODO: add test for fix
    }
}

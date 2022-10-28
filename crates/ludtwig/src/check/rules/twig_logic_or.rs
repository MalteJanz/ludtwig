use ludtwig_parser::syntax::typed::{AstNode, TwigBinaryExpression};
use ludtwig_parser::syntax::untyped::SyntaxNode;
use ludtwig_parser::T;

use crate::check::rule::{Rule, RuleContext, Severity};

pub struct RuleTwigLogicOr;

impl Rule for RuleTwigLogicOr {
    fn name(&self) -> &'static str {
        "twig-logic-or"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let binary = TwigBinaryExpression::cast(node)?;
        let binary_expr_op = binary.operator()?;

        if binary_expr_op.kind() == T!["||"] {
            // not a valid twig operator
            let result = ctx
                .create_result(
                    self.name(),
                    Severity::Error,
                    "'||' is not a valid twig operator",
                )
                .primary_note(binary_expr_op.text_range(), "help: change this operator")
                .suggestion(
                    binary_expr_op.text_range(),
                    "or",
                    "Try this operator instead",
                );

            ctx.add_result(result);
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
            "twig-logic-or",
            "{% if a == 5 || b %}hello{% endif %}",
            expect![[r#"
                error[twig-logic-or]: '||' is not a valid twig operator
                  ┌─ ./debug-rule.html.twig:1:14
                  │
                1 │ {% if a == 5 || b %}hello{% endif %}
                  │              ^^
                  │              │
                  │              help: change this operator
                  │              Try this operator instead: or

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-logic-or",
            "{% if a == 5 || b %}hello{% endif %}",
            expect!["{% if a == 5 or b %}hello{% endif %}"],
        );
    }
}

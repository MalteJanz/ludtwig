use ludtwig_parser::syntax::typed::{AstNode, TwigExtends};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigShopwareExtends;

impl Rule for RuleTwigShopwareExtends {
    fn name(&self) -> &'static str {
        "twig-prefer-sw-extends-over-extends"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let twig_extends = TwigExtends::cast(node)?;

        // not a valid twig operator
        let result = self
            .create_result(Severity::Error, "prefer 'sw_extends' over 'extends'")
            .primary_note(
                twig_extends.syntax().text_range(),
                "help: change 'extends' to 'sw_extends'",
            )
            .suggestion(
                twig_extends.syntax().text_range(),
                "sw_extends",
                "Try this keyword instead",
            );

        return Some(vec![result]);
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-prefer-sw-extends-over-extends",
            "{% extends foo %}",
            expect![[r#"
                error[twig-prefer-sw-extends-over-extends]: prefer 'sw_extends' over 'extends'
                  ┌─ ./debug-rule.html.twig:1:1
                  │
                1 │ {% extends foo %}
                  │ ^^^^^^^^^^^^^^^^^
                  │ │
                  │ help: change 'extends' to 'sw_extends'
                  │ Try this keyword instead: sw_extends

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-prefer-sw-extends-over-extends",
            "{% extends foo %}",
            expect!["sw_extends"],
        );
    }
}

use ludtwig_parser::syntax::typed::{AstNode, TwigExtends};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigPreferShopwareExtends;

impl Rule for RuleTwigPreferShopwareExtends {
    fn name(&self) -> &'static str {
        "twig-prefer-shopware-extends"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let twig_extends = TwigExtends::cast(node)?;
        let extends_keyword = twig_extends.get_extends_keyword()?;

        // not a valid twig operator
        let result = self
            .create_result(
                Severity::Warning,
                "prefer 'sw_extends' over 'extends' in shopware",
            )
            .primary_note(
                twig_extends.syntax().text_range(),
                "change this 'extends' to 'sw_extends'",
            )
            .suggestion(
                extends_keyword.text_range(),
                "sw_extends",
                "Try this keyword instead",
            );

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
            "twig-prefer-shopware-extends",
            "{% extends foo %}",
            expect![[r"
                warning[twig-prefer-shopware-extends]: prefer 'sw_extends' over 'extends' in shopware
                  ┌─ ./debug-rule.html.twig:1:1
                  │
                1 │ {% extends foo %}
                  │ ^^^^^^^^^^^^^^^^^
                  │ │  │
                  │ │  Try this keyword instead: sw_extends
                  │ change this 'extends' to 'sw_extends'

            "]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-prefer-shopware-extends",
            "{% extends foo %}",
            expect!["{% sw_extends foo %}"],
        );
    }
}

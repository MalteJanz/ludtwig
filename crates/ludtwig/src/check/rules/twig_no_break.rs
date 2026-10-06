use ludtwig_parser::syntax::typed::{AstNode, TwigBreak};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigNoBreak;

impl Rule for RuleTwigNoBreak {
    fn name(&self) -> &'static str {
        "twig-no-break"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let twig_break = TwigBreak::cast(node)?;

        Some(vec![
            self.create_result(Severity::Error, "'{% break %}' requires a Twig extension")
                .primary_note(
                    twig_break.syntax().text_range(),
                    "Prefer native Twig; use 'find' to select the first matching item",
                ),
        ])
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::test_rule;
    use expect_test::expect;

    #[test]
    fn reports_break() {
        test_rule(
            "twig-no-break",
            "{% for item in items %}{% break %}{% endfor %}",
            expect![[r"
                error[twig-no-break]: '{% break %}' requires a Twig extension
                  ┌─ ./debug-rule.html.twig:1:24
                  │
                1 │ {% for item in items %}{% break %}{% endfor %}
                  │                        ^^^^^^^^^^^ Prefer native Twig; use 'find' to select the first matching item

            "]],
        );
    }

    #[test]
    fn accepts_loop_without_break() {
        test_rule(
            "twig-no-break",
            "{% for item in items %}{{ item }}{% endfor %}",
            expect![""],
        );
    }
}

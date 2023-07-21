use crate::check::rule::{CheckResult, RuleExt, RuleRunContext};
use crate::{Rule, Severity};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken};

pub struct RuleUseNotSameAs;

impl Rule for RuleUseNotSameAs {
    fn name(&self) -> &'static str {
        "use-not-same-as"
    }

    fn check_token(&self, token: SyntaxToken, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        if token.kind() != SyntaxKind::TK_EXCLAMATION_MARK_DOUBLE_EQUALS {
            return None;
        }
        

        let result: CheckResult = self
        .create_result(Severity::Info, "Don't use !== ")
        .primary_note(token.text_range(), "Don't use '!==', try: not same as(condition)");

        Some(vec![result])

    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::test_rule;
    use expect_test::expect;

    #[test]
    fn rule_reports() {
        test_rule(
            "use-not-same-as",
            "{% if test !== false %}{% endif %}",
            expect![[r#"
                note[use-not-same-as]: Don't use !== 
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ {% if test !== false %}{% endif %}
                  │            ^^^ Don't use '!==', try: not same as(condition)

            "#]],
        );
    }
}
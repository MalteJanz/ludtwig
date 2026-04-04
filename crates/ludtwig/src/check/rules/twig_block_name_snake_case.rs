use crate::check::naming_convention::{is_valid_ascii_alpha_snake_case, try_make_snake_case};
use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};
use ludtwig_parser::syntax::typed::{AstNode, TwigStartingBlock};
use ludtwig_parser::syntax::untyped::SyntaxNode;

pub struct RuleTwigBlockNameSnakeCase;

impl Rule for RuleTwigBlockNameSnakeCase {
    fn name(&self) -> &'static str {
        "twig-block-name-snake-case"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let block_name = TwigStartingBlock::cast(node)?.name()?;
        if !is_valid_ascii_alpha_snake_case(block_name.text(), false) {
            // name is not valid ascii snake case
            let mut result = self
                .create_result(Severity::Help, "Block name is not written in snake_case")
                .primary_note(
                    block_name.text_range(),
                    "help: rename this block in snake_case",
                );

            // try make a suggestion
            if let Some(suggested_name) = try_make_snake_case(block_name.text(), false) {
                result = result.suggestion(
                    block_name.text_range(),
                    suggested_name,
                    "Try this name instead",
                );
            }

            return Some(vec![result]);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-block-name-snake-case",
            "{% block a-b %}hello{% endblock %}",
            expect![[r"
                help[twig-block-name-snake-case]: Block name is not written in snake_case
                  ┌─ ./debug-rule.html.twig:1:10
                  │
                1 │ {% block a-b %}hello{% endblock %}
                  │          ^^^
                  │          │
                  │          help: rename this block in snake_case
                  │          Try this name instead: a_b

            "]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-block-name-snake-case",
            "{% block a-b %}hello{% endblock %}",
            expect!["{% block a_b %}hello{% endblock %}"],
        );
    }
}

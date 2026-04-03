use ludtwig_parser::syntax::typed::{
    AstNode, TwigAssignment, TwigForBlock, TwigLiteralName, support,
};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigVariableNameCamelCase;

impl Rule for RuleTwigVariableNameCamelCase {
    fn name(&self) -> &'static str {
        "twig-variable-name-camel-case"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        match node.kind() {
            SyntaxKind::TWIG_ASSIGNMENT => {
                let assignment = TwigAssignment::cast(node)?;
                check_literal_names(self, assignment.syntax())
            }
            SyntaxKind::TWIG_FOR_BLOCK => {
                let for_block = TwigForBlock::cast(node)?;
                check_literal_names(self, for_block.syntax())
            }
            _ => None,
        }
    }
}

fn check_literal_names(
    rule: &RuleTwigVariableNameCamelCase,
    syntax: &SyntaxNode,
) -> Option<Vec<CheckResult>> {
    let mut results = vec![];

    for name_node in support::children::<TwigLiteralName>(syntax) {
        let Some(name_token) = name_node.get_name() else {
            continue;
        };
        let name = name_token.text();

        if !is_camel_case(name) {
            results.push(
                rule.create_result(Severity::Help, "twig variable name is not in camelCase")
                    .primary_note(
                        name_token.text_range(),
                        format!("rename '{name}' to camelCase"),
                    ),
            );
        }
    }

    if results.is_empty() {
        None
    } else {
        Some(results)
    }
}

/// Check if a name is valid camelCase.
/// Allows leading underscores (e.g. `_private`).
/// First non-underscore character must be lowercase.
/// No underscores or hyphens after the initial prefix.
fn is_camel_case(name: &str) -> bool {
    let stripped = name.trim_start_matches('_');
    if stripped.is_empty() {
        return true; // just underscores, allow it
    }

    // First character must be lowercase
    let first = stripped.chars().next().unwrap();
    if !first.is_ascii_lowercase() {
        return false;
    }

    // No underscores or hyphens in the rest
    !stripped.contains('_') && !stripped.contains('-')
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn rule_reports_snake_case_set() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% set my_variable = 'hello' %}",
            expect![[r#"
                help[twig-variable-name-camel-case]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% set my_variable = 'hello' %}
                  │        ^^^^^^^^^^^ rename 'my_variable' to camelCase

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_camel_case_set() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% set myVariable = 'hello' %}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_snake_case_for_loop() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% for my_item in items %}{{ my_item }}{% endfor %}",
            expect![[r#"
                help[twig-variable-name-camel-case]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% for my_item in items %}{{ my_item }}{% endfor %}
                  │        ^^^^^^^ rename 'my_item' to camelCase

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_camel_case_for_loop() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% for myItem in items %}{{ myItem }}{% endfor %}",
            expect![""],
        );
    }

    #[test]
    fn rule_allows_leading_underscore() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% set _privateVar = 'hello' %}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_for_key_value() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% for my_key, my_value in items %}{% endfor %}",
            expect![[r#"
                help[twig-variable-name-camel-case]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% for my_key, my_value in items %}{% endfor %}
                  │        ^^^^^^ rename 'my_key' to camelCase

                help[twig-variable-name-camel-case]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:16
                  │
                1 │ {% for my_key, my_value in items %}{% endfor %}
                  │                ^^^^^^^^ rename 'my_value' to camelCase

            "#]],
        );
    }

    #[test]
    fn rule_allows_single_word() {
        test_rule(
            "twig-variable-name-camel-case",
            r"{% set name = 'hello' %}",
            expect![""],
        );
    }
}

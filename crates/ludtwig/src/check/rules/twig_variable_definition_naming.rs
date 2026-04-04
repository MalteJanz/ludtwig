use crate::check::naming_convention::{
    is_camel_case, is_valid_ascii_alpha_snake_case, try_make_snake_case,
};
use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};
use ludtwig_parser::syntax::typed::{
    AstNode, TwigAssignment, TwigExpression, TwigForBlock, TwigFunctionCall, TwigLiteralName,
    support,
};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

pub struct RuleTwigVariableDefinitionNaming;

impl Rule for RuleTwigVariableDefinitionNaming {
    fn name(&self) -> &'static str {
        "twig-variable-definition-naming"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        match node.kind() {
            SyntaxKind::TWIG_ASSIGNMENT => {
                let assignment = TwigAssignment::cast(node)?;

                let is_constant = is_constant_assignment(assignment.syntax());
                check_literal_names(self, assignment.syntax(), is_constant)
            }
            SyntaxKind::TWIG_FOR_BLOCK => {
                let for_block = TwigForBlock::cast(node)?;
                check_literal_names(self, for_block.syntax(), false)
            }
            _ => None,
        }
    }
}

fn is_constant_assignment(syntax: &SyntaxNode) -> bool {
    let Some(expr) = support::child::<TwigExpression>(syntax) else {
        return false;
    };

    let Some(twig_fn) = support::child::<TwigFunctionCall>(expr.syntax()) else {
        return false;
    };

    let Some(op) = twig_fn.name_operand() else {
        return false;
    };

    let Some(op_name) = support::child::<TwigLiteralName>(op.syntax()) else {
        return false;
    };

    let Some(name_lit) = op_name.get_name() else {
        return false;
    };

    if name_lit.text() != "constant" {
        return false;
    }

    true
}

fn check_literal_names(
    rule: &RuleTwigVariableDefinitionNaming,
    syntax: &SyntaxNode,
    is_constant: bool,
) -> Option<Vec<CheckResult>> {
    let mut results = vec![];

    for name_node in support::children::<TwigLiteralName>(syntax) {
        let Some(name_token) = name_node.get_name() else {
            continue;
        };
        let name = name_token.text();

        if is_constant && !is_valid_ascii_alpha_snake_case(name, true) {
            let maybe_suggestion = try_make_snake_case(name, true);
            let suggestion = maybe_suggestion
                .as_deref()
                .unwrap_or("SCREAMING_SNAKE_CASE");

            results.push(
                rule.create_result(
                    Severity::Help,
                    "twig constant name is not in SCREAMING_SNAKE_CASE",
                )
                .primary_note(
                    name_token.text_range(),
                    format!("rename '{name}' to {suggestion}"),
                ),
            );
        }

        if !is_constant && !is_camel_case(name) {
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_does_not_fix};

    #[test]
    fn rule_reports_snake_case_set() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set my_variable = 'hello' %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% set my_variable = 'hello' %}
                  │        ^^^^^^^^^^^ rename 'my_variable' to camelCase

            "#]],
        );
    }

    #[test]
    fn rule_reports_camel_case_constant_set() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set myVariable2 = constant('hello') %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig constant name is not in SCREAMING_SNAKE_CASE
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% set myVariable2 = constant('hello') %}
                  │        ^^^^^^^^^^^ rename 'myVariable2' to MY_VARIABLE2

            "#]],
        );
    }

    #[test]
    fn rule_reports_lower_snake_case_constant_set() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set my_variable = constant('hello') %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig constant name is not in SCREAMING_SNAKE_CASE
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% set my_variable = constant('hello') %}
                  │        ^^^^^^^^^^^ rename 'my_variable' to MY_VARIABLE

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_camel_case_set() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set myVariable = 'hello' %}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_snake_case_for_loop() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% for my_item in items %}{{ my_item }}{% endfor %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig variable name is not in camelCase
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
            "twig-variable-definition-naming",
            r"{% for myItem in items %}{{ myItem }}{% endfor %}",
            expect![""],
        );
    }

    #[test]
    fn rule_allows_leading_underscore() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set _privateVar = 'hello' %}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_for_key_value() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% for my_key, my_value in items %}{% endfor %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% for my_key, my_value in items %}{% endfor %}
                  │        ^^^^^^ rename 'my_key' to camelCase

                help[twig-variable-definition-naming]: twig variable name is not in camelCase
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
            "twig-variable-definition-naming",
            r"{% set name = 'hello' %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_rhs_variable_in_set() {
        // The RHS of a `set` assignment is not a variable definition, so it must not be flagged
        test_rule(
            "twig-variable-definition-naming",
            r"{% set myVar = snake_case_rhs %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_collection_in_for_loop() {
        // The iterable in a `for` loop is not a variable definition, so it must not be flagged
        test_rule(
            "twig-variable-definition-naming",
            r"{% for item in snake_items %}{% endfor %}",
            expect![""],
        );
    }

    #[test]
    fn rule_reports_upper_camel_case_set() {
        test_rule(
            "twig-variable-definition-naming",
            r"{% set MyVar = 'hello' %}",
            expect![[r#"
                help[twig-variable-definition-naming]: twig variable name is not in camelCase
                  ┌─ ./debug-rule.html.twig:1:8
                  │
                1 │ {% set MyVar = 'hello' %}
                  │        ^^^^^ rename 'MyVar' to camelCase

            "#]],
        );
    }

    #[test]
    fn rule_does_not_fix_variable_set() {
        test_rule_does_not_fix(
            "twig-variable-definition-naming",
            r"{% set my_variable = constant('hello') %}",
            expect![[r#"{% set my_variable = constant('hello') %}"#]],
        );

        test_rule_does_not_fix(
            "twig-variable-definition-naming",
            r"{% set my_variable = 'hello' %}",
            expect![[r#"{% set my_variable = 'hello' %}"#]],
        );

        test_rule_does_not_fix(
            "twig-variable-definition-naming",
            r"{% for my_key, my_value in items %}{% endfor %}",
            expect![[r#"{% for my_key, my_value in items %}{% endfor %}"#]],
        );
    }
}

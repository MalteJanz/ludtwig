use ludtwig_parser::T;
use ludtwig_parser::syntax::typed::{
    AstNode, TwigBinaryExpression, TwigFilter, TwigFunctionCall, TwigLiteralName, support,
};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigValidFunction;

impl Rule for RuleTwigValidFunction {
    fn name(&self) -> &'static str {
        "twig-valid-function"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let func_call = TwigFunctionCall::cast(node)?;

        // Skip function calls that are inside a TwigFilter's right operand (those are filter calls, not standalone functions)
        if let Some(parent) = func_call.syntax().parent() {
            // parent is TwigOperand, grandparent could be TwigFilter
            if let Some(grandparent) = parent.parent() {
                if let Some(filter) = TwigFilter::cast(grandparent) {
                    if filter.filter().is_some_and(|f| *f.syntax() == parent) {
                        return None;
                    }
                }
            }
        }

        // Skip function calls that are the RHS of `is` / `is not` expressions (those are twig tests, not functions)
        // AST: TwigFunctionCall -> TwigExpression -> TwigBinaryExpression
        if let Some(parent) = func_call.syntax().parent() {
            if let Some(grandparent) = parent.parent() {
                if let Some(binary_expr) = TwigBinaryExpression::cast(grandparent) {
                    if let Some(op) = binary_expr.operator() {
                        if op.kind() == T!["is"] || op.kind() == T!["is not"] {
                            return None;
                        }
                    }
                }
            }
        }

        let name_operand = func_call.name_operand()?;
        let name_node: TwigLiteralName = support::child(name_operand.syntax())?;
        let name_token = name_node.get_name()?;
        let func_name = name_token.text();

        let valid_functions = &ctx.config().twig.valid_functions;

        if !valid_functions.iter().any(|f| f == func_name) {
            let result = self
                .create_result(
                    Severity::Warning,
                    format!("unknown twig function '{func_name}'"),
                )
                .primary_note(
                    name_token.text_range(),
                    "this function is not in the valid-functions config",
                );

            return Some(vec![result]);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn rule_does_not_report_known_function() {
        test_rule("twig-valid-function", "{{ dump(var) }}", expect![""]);
    }

    #[test]
    fn rule_reports_unknown_function() {
        test_rule(
            "twig-valid-function",
            "{{ unknown_func(var) }}",
            expect![[r#"
                warning[twig-valid-function]: unknown twig function 'unknown_func'
                  ┌─ ./debug-rule.html.twig:1:4
                  │
                1 │ {{ unknown_func(var) }}
                  │    ^^^^^^^^^^^^ this function is not in the valid-functions config

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_filter_function_call() {
        test_rule(
            "twig-valid-function",
            "{{ name|date('Y-m-d') }}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_is_test_with_args() {
        test_rule(
            "twig-valid-function",
            "{% if foo is same as(bar) %}{% endif %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_is_not_test_with_args() {
        test_rule(
            "twig-valid-function",
            "{% if foo is not divisible by(3) %}{% endif %}",
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_known_range() {
        test_rule(
            "twig-valid-function",
            "{% for i in range(0, 10) %}{{ i }}{% endfor %}",
            expect![""],
        );
    }
}

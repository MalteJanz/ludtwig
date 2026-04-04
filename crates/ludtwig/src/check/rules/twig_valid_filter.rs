use ludtwig_parser::syntax::typed::{
    AstNode, TwigFilter, TwigFunctionCall, TwigLiteralName, support,
};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigValidFilter;

impl Rule for RuleTwigValidFilter {
    fn name(&self) -> &'static str {
        "twig-valid-filter"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let filter = TwigFilter::cast(node)?;
        let right_operand = filter.filter()?;

        // Filter name can be a plain TwigLiteralName or inside a TwigFunctionCall (when filter has args)
        let name_token = if let Some(name_node) =
            support::child::<TwigLiteralName>(right_operand.syntax())
        {
            name_node.get_name()?
        } else if let Some(func_call) = support::child::<TwigFunctionCall>(right_operand.syntax()) {
            let name_operand = func_call.name_operand()?;
            let name_node: TwigLiteralName = support::child(name_operand.syntax())?;
            name_node.get_name()?
        } else {
            return None;
        };

        let filter_name = name_token.text();
        let valid_filters = &ctx.config().twig.valid_filters;

        if !valid_filters.iter().any(|f| f == filter_name) {
            let result = self
                .create_result(
                    Severity::Warning,
                    format!("unknown twig filter '{filter_name}'"),
                )
                .primary_note(
                    name_token.text_range(),
                    "this filter is not in the valid-filters config",
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
    fn rule_does_not_report_known_filter() {
        test_rule("twig-valid-filter", "{{ name|lower }}", expect![""]);
    }

    #[test]
    fn rule_does_not_report_known_filter_with_args() {
        test_rule("twig-valid-filter", "{{ date|date('Y-m-d') }}", expect![""]);
    }

    #[test]
    fn rule_reports_unknown_filter() {
        test_rule(
            "twig-valid-filter",
            "{{ name|unknown_filter }}",
            expect![[r#"
                warning[twig-valid-filter]: unknown twig filter 'unknown_filter'
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ {{ name|unknown_filter }}
                  │         ^^^^^^^^^^^^^^ this filter is not in the valid-filters config

            "#]],
        );
    }

    #[test]
    fn rule_reports_unknown_filter_with_args() {
        test_rule(
            "twig-valid-filter",
            "{{ name|unknown_filter('arg') }}",
            expect![[r#"
                warning[twig-valid-filter]: unknown twig filter 'unknown_filter'
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ {{ name|unknown_filter('arg') }}
                  │         ^^^^^^^^^^^^^^ this filter is not in the valid-filters config

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_chained_known_filters() {
        test_rule("twig-valid-filter", "{{ name|lower|upper }}", expect![""]);
    }

    #[test]
    fn rule_reports_unknown_filter_in_chain_but_not_known_ones() {
        test_rule(
            "twig-valid-filter",
            "{{ name|lower|unknown_custom }}",
            expect![[r#"
                warning[twig-valid-filter]: unknown twig filter 'unknown_custom'
                  ┌─ ./debug-rule.html.twig:1:15
                  │
                1 │ {{ name|lower|unknown_custom }}
                  │               ^^^^^^^^^^^^^^ this filter is not in the valid-filters config

            "#]],
        );
    }
}

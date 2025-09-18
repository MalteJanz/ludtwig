use ludtwig_parser::syntax::typed::{support, AstNode, TwigFilter, TwigLiteralName};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigJsonEncodeEscapeJs;

impl Rule for RuleTwigJsonEncodeEscapeJs {
    fn name(&self) -> &'static str {
        "twig-json-encode-escape-js"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let filter = TwigFilter::cast(node)?;

        let right_operand = filter.filter()?;
        let right_name_node: TwigLiteralName = support::child(right_operand.syntax())?;
        let right_name_token = right_name_node.get_name()?;
        let right_name = right_name_token.text();

        if right_name != "raw" {
            return None;
        }

        if !chain_contains_json_encode(&filter) {
            return None;
        }

        let result = self
            .create_result(
                Severity::Warning,
                "avoid raw after json_encode; use escape('js')",
            )
            .primary_note(
                right_name_token.text_range(),
                "help: replace 'raw' with escape('js')",
            )
            .suggestion(
                right_name_token.text_range(),
                "escape('js')",
                "Try this filter instead",
            );

        Some(vec![result])
    }
}

/// Walk nested filter chain leftwards and check if any filter is `json_encode`.
fn chain_contains_json_encode(filter: &TwigFilter) -> bool {
    let mut current = filter.clone();
    loop {
        if let Some(right_operand) = current.filter() {
            if let Some(name_node) = support::child::<TwigLiteralName>(right_operand.syntax()) {
                if name_node
                    .get_name()
                    .is_some_and(|t| t.text() == "json_encode")
                {
                    return true;
                }
            }
        }

        if let Some(left_operand) = current.operand() {
            if let Some(inner) = support::child::<TwigFilter>(left_operand.syntax()) {
                current = inner;
                continue;
            }
        }

        break;
    }

    false
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_does_not_fix, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn test_rule_report() {
        test_rule(
            "twig-json-encode-escape-js",
            "{{ a|json_encode|raw }}",
            expect![[r#"
                warning[twig-json-encode-escape-js]: avoid raw after json_encode; use escape('js')
                  ┌─ ./debug-rule.html.twig:1:18
                  │
                1 │ {{ a|json_encode|raw }}
                  │                  ^^^
                  │                  │
                  │                  help: replace 'raw' with escape('js')
                  │                  Try this filter instead: escape('js')

            "#]],
        );
    }

    #[test]
    fn fixes_simple_chain() {
        test_rule_fix(
            "twig-json-encode-escape-js",
            "{{ a|json_encode|raw }}",
            expect!["{{ a|json_encode|escape('js') }}"],
        );
    }

    #[test]
    fn fixes_with_json_encode_args() {
        test_rule_fix(
            "twig-json-encode-escape-js",
            "{{ breakpoint|json_encode()|raw }}",
            expect!["{{ breakpoint|json_encode()|escape('js') }}"],
        );
    }

    #[test]
    fn fixes_inside_html_attribute() {
        test_rule_fix(
            "twig-json-encode-escape-js",
            "data-magnifier-options='{{ magnifierOptions|json_encode|raw }}'",
            expect!["data-magnifier-options='{{ magnifierOptions|json_encode|escape('js') }}'"],
        );
    }

    #[test]
    fn fixes_with_function_call() {
        test_rule_fix(
            "twig-json-encode-escape-js",
            "{{ getAllFeatures()|json_encode|raw }}",
            expect!["{{ getAllFeatures()|json_encode|escape('js') }}"],
        );
    }

    #[test]
    fn does_not_change_raw_without_json_encode() {
        test_rule_does_not_fix(
            "twig-json-encode-escape-js",
            "{{ a|raw }}",
            expect!["{{ a|raw }}"],
        );
    }

    #[test]
    fn does_not_change_when_already_escaped_js() {
        test_rule_does_not_fix(
            "twig-json-encode-escape-js",
            "{{ a|json_encode|escape('js') }}",
            expect!["{{ a|json_encode|escape('js') }}"],
        );
    }
}

use ludtwig_parser::syntax::typed::{support, AstNode, TwigLiteralHashKey, TwigLiteralString};
use ludtwig_parser::syntax::untyped::{SyntaxNode, SyntaxNodeExt, TextRange, TextSize};
use ludtwig_parser::TWIG_NAME_REGEX;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigHashKeyNoQuotes;

impl Rule for RuleTwigHashKeyNoQuotes {
    fn name(&self) -> &'static str {
        "twig-hash-key-no-quotes"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let hash_key = TwigLiteralHashKey::cast(node)?;
        let key_string_literal: TwigLiteralString = support::child(hash_key.syntax())?;
        let key_string_inner = key_string_literal.get_inner()?;

        // validate that inner string is a single valid twig word
        let key_only_contains_one_element =
            key_string_inner.syntax().children_with_tokens().count() == 1;
        let key_string_matches_twig_word_regex =
            TWIG_NAME_REGEX.is_match(&key_string_inner.syntax().text().to_string());

        if key_only_contains_one_element && key_string_matches_twig_word_regex {
            let result = self
                .create_result(Severity::Help, "unnecessary quotation")
                .primary_note(
                    key_string_literal.syntax().text_range_trimmed_trivia(),
                    "help: remove quotation",
                )
                .suggestion(
                    key_string_literal.get_opening_quote().map_or_else(
                        || {
                            TextRange::at(
                                key_string_inner.syntax().text_range().start(),
                                TextSize::from(0),
                            )
                        },
                        |q| q.text_range(),
                    ),
                    "",
                    "remove this quote",
                )
                .suggestion(
                    key_string_literal.get_closing_quote().map_or_else(
                        || {
                            TextRange::at(
                                key_string_inner.syntax().text_range().end(),
                                TextSize::from(0),
                            )
                        },
                        |q| q.text_range(),
                    ),
                    "",
                    "remove this quote",
                );

            return Some(vec![result]);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_does_not_fix, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-hash-key-no-quotes",
            r"{% set v = { 'myKey': 42 } %}",
            expect![[r"
                help[twig-hash-key-no-quotes]: unnecessary quotation
                  ┌─ ./debug-rule.html.twig:1:14
                  │
                1 │ {% set v = { 'myKey': 42 } %}
                  │              ^^^^^^^
                  │              │     │
                  │              │     remove this quote: 
                  │              help: remove quotation
                  │              remove this quote: 

            "]],
        );
    }

    #[test]
    fn rule_not_reports() {
        test_rule(
            "twig-hash-key-no-quotes",
            r"{% set v = { 'my-key': 42 } %}",
            expect![[r""]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-hash-key-no-quotes",
            r"{% set v = { 'myKey': 42 } %}",
            expect![r"{% set v = { myKey: 42 } %}"],
        );
    }

    #[test]
    fn rule_does_not_fix_strings_containing_quotes() {
        test_rule_does_not_fix(
            "twig-hash-key-no-quotes",
            r#"{{ 'replace " this quote'|replace({'"': 'another quote with'}) }}"#,
            expect![r#"{{ 'replace " this quote'|replace({'"': 'another quote with'}) }}"#],
        );
    }
}

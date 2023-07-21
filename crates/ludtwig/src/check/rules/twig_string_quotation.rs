use ludtwig_parser::syntax::typed::{AstNode, TwigLiteralString};
use ludtwig_parser::syntax::untyped::{SyntaxNode, SyntaxNodeExt, TextRange, TextSize};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};
use crate::config::Quotation;

pub struct RuleTwigStringQuotation;

impl Rule for RuleTwigStringQuotation {
    fn name(&self) -> &'static str {
        "twig-string-quotation"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let twig_string = TwigLiteralString::cast(node)?;

        // check for interpolated string where single quotes would be suggested
        if ctx.config().format.twig_quotation == Quotation::Single
            && twig_string
                .get_inner()
                .map_or(0, |inner| inner.get_interpolations().count())
                > 0
        {
            return None; // interpolated strings can't change quotes
        }

        let correct_quote = ctx.config().format.twig_quotation.corresponding_char();
        let opening_is_fine = twig_string
            .get_opening_quote()
            .map_or(false, |t| t.text().starts_with(correct_quote));
        let closing_is_fine = twig_string
            .get_closing_quote()
            .map_or(false, |t| t.text().starts_with(correct_quote));

        if !opening_is_fine || !closing_is_fine {
            // invalid quotation
            let mut result = self
                .create_result(Severity::Help, "wrong quotation")
                .primary_note(
                    twig_string.syntax().text_range_trimmed_trivia(),
                    format!(
                        "help: change the quotation to {}",
                        ctx.config().format.twig_quotation
                    ),
                );

            result =
                make_changed_quotes_suggestion_if_possible(&twig_string, correct_quote, result);

            return Some(vec![result]);
        }

        None
    }
}

pub fn make_changed_quotes_suggestion_if_possible(
    twig_string: &TwigLiteralString,
    correct_quote: char,
    mut result: CheckResult,
) -> CheckResult {
    let inner = match twig_string.get_inner() {
        Some(inner) => inner,
        None => return result,
    };

    let inner_text = inner.syntax().text();
    if inner_text.contains_char(correct_quote) {
        return result; // TODO: could still try to transform the string with more effort...
    }

    // opening quote
    if let Some(quote) = twig_string.get_opening_quote() {
        result = result.suggestion(quote.text_range(), correct_quote, "Try this quote instead");
    } else {
        result = result.suggestion(
            TextRange::at(inner.syntax().text_range().start(), TextSize::from(0)),
            correct_quote,
            "Try this quote instead",
        );
    }

    // closing quote
    if let Some(quote) = twig_string.get_closing_quote() {
        result = result.suggestion(quote.text_range(), correct_quote, "Try this quote instead");
    } else {
        result = result.suggestion(
            TextRange::at(inner.syntax().text_range().end(), TextSize::from(0)),
            correct_quote,
            "Try this quote instead",
        );
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::check::rules::test::{test_rule, test_rule_does_not_fix, test_rule_fix};
    use expect_test::expect;

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-string-quotation",
            r#"{{ "double-quoted" }}"#,
            expect![[r#"
                help[twig-string-quotation]: wrong quotation
                  ┌─ ./debug-rule.html.twig:1:4
                  │
                1 │ {{ "double-quoted" }}
                  │    ^^^^^^^^^^^^^^^
                  │    │             │
                  │    │             Try this quote instead: '
                  │    help: change the quotation to single quotes (')
                  │    Try this quote instead: '

            "#]],
        );
    }

    #[test]
    fn rule_does_report_strings_containing_same_quotation() {
        test_rule(
            "twig-string-quotation",
            r#"{{ "doesn't" }}"#,
            expect![[r#"
                help[twig-string-quotation]: wrong quotation
                  ┌─ ./debug-rule.html.twig:1:4
                  │
                1 │ {{ "doesn't" }}
                  │    ^^^^^^^^^ help: change the quotation to single quotes (')

            "#]],
        );
    }

    #[test]
    fn rule_doesnt_report_double_quoted_strings_containing_interpolation() {
        test_rule(
            "twig-string-quotation",
            r#"{{ "/#{prefix}/phoneNumber" }}"#,
            expect![r#""#],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-string-quotation",
            r#"{{ "double-quoted" }}"#,
            expect![r#"{{ 'double-quoted' }}"#],
        );
    }

    #[test]
    fn rule_doesnt_fix_strings_containing_same_quotation() {
        test_rule_does_not_fix(
            "twig-string-quotation",
            r#"{{ "doesn't" }}"#,
            expect![r#"{{ "doesn't" }}"#],
        );
    }
}

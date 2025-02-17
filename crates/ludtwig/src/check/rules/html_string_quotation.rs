use ludtwig_parser::syntax::typed::{AstNode, HtmlString};
use ludtwig_parser::syntax::untyped::{SyntaxNode, SyntaxNodeExt, TextRange, TextSize};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleHtmlStringQuotation;

impl Rule for RuleHtmlStringQuotation {
    fn name(&self) -> &'static str {
        "html-string-quotation"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let html_string = HtmlString::cast(node)?;

        let correct_quote = ctx.config().format.html_quotation.corresponding_char();
        let opening_is_fine = html_string
            .get_opening_quote()
            .is_some_and(|t| t.text().starts_with(correct_quote));
        let closing_is_fine = html_string
            .get_closing_quote()
            .is_some_and(|t| t.text().starts_with(correct_quote));

        if !opening_is_fine || !closing_is_fine {
            // invalid quotation
            let mut result = self
                .create_result(Severity::Help, "wrong quotation")
                .primary_note(
                    html_string.syntax().text_range_trimmed_trivia(),
                    format!(
                        "help: change the quotation to {}",
                        ctx.config().format.html_quotation
                    ),
                );

            result =
                make_changed_quotes_suggestion_if_possible(&html_string, correct_quote, result);
            return Some(vec![result]);
        }

        None
    }
}

pub fn make_changed_quotes_suggestion_if_possible(
    twig_string: &HtmlString,
    correct_quote: char,
    mut result: CheckResult,
) -> CheckResult {
    let Some(inner) = twig_string.get_inner() else {
        return result;
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
            "html-string-quotation",
            r"<div class='a'></div>",
            expect![[r#"
                help[html-string-quotation]: wrong quotation
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ <div class='a'></div>
                  │            ^^^
                  │            │ │
                  │            │ Try this quote instead: "
                  │            help: change the quotation to double quotes (")
                  │            Try this quote instead: "

            "#]],
        );
    }

    #[test]
    fn rule_does_report_strings_containing_same_quotation() {
        test_rule(
            "html-string-quotation",
            r#"<div style='value: "a"'></div>"#,
            expect![[r#"
                help[html-string-quotation]: wrong quotation
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ <div style='value: "a"'></div>
                  │            ^^^^^^^^^^^^ help: change the quotation to double quotes (")

            "#]],
        );
    }

    #[test]
    fn rule_does_report_strings_without_quotes() {
        test_rule(
            "html-string-quotation",
            r"<div class=a></div>",
            expect![[r#"
                help[html-string-quotation]: wrong quotation
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ <div class=a></div>
                  │            ^- Try this quote instead: "
                  │            │ 
                  │            help: change the quotation to double quotes (")
                  │            Try this quote instead: "

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "html-string-quotation",
            r"<div class='a'></div>",
            expect![r#"<div class="a"></div>"#],
        );
    }

    #[test]
    fn rule_fixes_no_quotation() {
        test_rule_fix(
            "html-string-quotation",
            r"<div class=a required></div>",
            expect![r#"<div class="a" required></div>"#],
        );
    }

    #[test]
    fn rule_doesnt_fix_strings_containing_same_quotation() {
        test_rule_does_not_fix(
            "html-string-quotation",
            r#"<div style='value: "a"'></div>"#,
            expect![r#"<div style='value: "a"'></div>"#],
        );
    }
}

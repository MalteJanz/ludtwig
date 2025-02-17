use regex::Regex;
use std::sync::OnceLock;

use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken, TextRange, TextSize};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};
use crate::config::LineEnding;

pub struct RuleLineEnding;

impl Rule for RuleLineEnding {
    fn name(&self) -> &'static str {
        "line-ending"
    }

    fn check_token(&self, token: SyntaxToken, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        static INVALID_REGEX: OnceLock<Regex> = OnceLock::new();

        if token.kind() != SyntaxKind::TK_LINE_BREAK {
            return None;
        }

        let correct_line_ending = ctx.config().format.line_ending.corresponding_string();
        let message = format!("use {} instead", ctx.config().format.line_ending);

        // compile regex only once and store it in a static
        // because this function is called in a hot loop this does improve it's performance significantly
        let invalid_regex = INVALID_REGEX.get_or_init(|| {
            Regex::new(&format!(
                r"({})",
                match ctx.config().format.line_ending {
                    LineEnding::UnixLF => "\r\n", // inverse: look for windows line endings
                    LineEnding::WindowsCRLF => "[^\r]?\n", // inverse: look for unix line endings
                }
            ))
            .unwrap()
        });

        let mut results = vec![];
        for invalid in invalid_regex.find_iter(token.text()) {
            #[allow(clippy::cast_possible_truncation)]
            let range = TextRange::new(
                token.text_range().start() + TextSize::from(invalid.start() as u32),
                token.text_range().start() + TextSize::from(invalid.end() as u32),
            );
            let result = self
                .create_result(Severity::Warning, "invalid line ending")
                .primary_note(
                    range,
                    "this line ending does not conform to the configured style",
                )
                .suggestion(range, correct_line_ending, message.clone());

            results.push(result);
        }

        if results.is_empty() {
            None
        } else {
            Some(results)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    #[test]
    fn rule_line_ending_trivial() {
        test_rule(
            "line-ending",
            "\r\n",
            expect![[r"
                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:1:1
                  │    
                1 │ ╭ ╭ 
                2 │ │ │ 
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


            "]],
        );
        test_rule_fix(
            "line-ending",
            "\r\nA",
            expect![[r"

                A"]],
        );
    }

    #[test]
    fn rule_line_ending_simple() {
        test_rule(
            "line-ending",
            "hello\r\nworld\r\n",
            expect![[r"
                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:1:6
                  │    
                1 │     hello
                  │ ╭───────^
                  │ │ ╭─────'
                2 │ │ │ world
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:2:6
                  │    
                2 │     world
                  │ ╭───────^
                  │ │ ╭─────'
                3 │ │ │ 
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


            "]],
        );
        test_rule_fix(
            "line-ending",
            "hello\r\nworld\r\n",
            expect![[r"
                hello
                world
            "]],
        );
    }

    #[test]
    fn rule_line_ending_chained() {
        test_rule(
            "line-ending",
            "hello\r\n\r\n\r\nworld",
            expect![[r"
                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:1:6
                  │    
                1 │     hello
                  │ ╭───────^
                  │ │ ╭─────'
                2 │ │ │ 
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:2:1
                  │    
                2 │ ╭ ╭ 
                3 │ │ │ 
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:3:1
                  │    
                3 │ ╭ ╭ 
                4 │ │ │ world
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


            "]],
        );
        test_rule_fix(
            "line-ending",
            "hello\r\n\r\n\r\nworld",
            expect![[r"
                hello


                world"]],
        );
    }

    #[test]
    fn rule_line_ending_mixed() {
        test_rule(
            "line-ending",
            "hello\n\r\nworld",
            expect![[r"
                warning[line-ending]: invalid line ending
                  ┌─ ./debug-rule.html.twig:2:1
                  │    
                2 │ ╭ ╭ 
                3 │ │ │ world
                  │ ╰─│^ this line ending does not conform to the configured style
                  │   ╰' use UnixLF (\n) instead: 


            "]],
        );
        test_rule_fix(
            "line-ending",
            "hello\n\r\nworld",
            expect![[r"
                hello

                world"]],
        );
    }
}

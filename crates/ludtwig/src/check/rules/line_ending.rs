use crate::check::rule::{Rule, RuleContext, Severity};
use crate::config::LineEnding;
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken};
use regex::Regex;

pub struct RuleLineEnding;

impl Rule for RuleLineEnding {
    fn name(&self) -> &'static str {
        "line-ending"
    }

    fn check_token(&self, token: SyntaxToken, ctx: &mut RuleContext) -> Option<()> {
        if token.kind() != SyntaxKind::TK_LINE_BREAK {
            return None;
        }

        let correct_line_ending = match ctx.config().format.line_ending {
            LineEnding::UnixLF => "\n",
            LineEnding::WindowsCRLF => "\r\n",
        };
        let check_regex = Regex::new(&format!(r#"^({})+$"#, correct_line_ending)).unwrap();

        // TODO: fix this for multiple line ending in single token

        if !check_regex.is_match(token.text()) {
            let message = match ctx.config().format.line_ending {
                LineEnding::UnixLF => "use UnixLF (\\n) instead",
                LineEnding::WindowsCRLF => "use WindowsCRLF (\\r\\n) instead",
            };

            let result = ctx
                .create_result(self.name(), Severity::Warning, "invalid line ending")
                .primary_note(
                    token.text_range(),
                    "this line ending does not conform to the configured style",
                )
                .suggestion(token.text_range(), correct_line_ending, message);

            ctx.add_result(result);
        }

        None
    }
}

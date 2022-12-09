use crate::check::rule::{CheckResult, RuleExt, RuleRunContext};
use crate::{Rule, Severity};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken};

pub struct RuleUnknownToken;

impl Rule for RuleUnknownToken {
    fn name(&self) -> &'static str {
        "unknown-token"
    }

    fn check_token(&self, token: SyntaxToken, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        if token.kind() != SyntaxKind::TK_UNKNOWN {
            return None;
        }

        let result = self
            .create_result(Severity::Info, "Unknown syntax token found")
            .primary_note(token.text_range(), "Unknown syntax token encountered here");

        Some(vec![result])
    }
}

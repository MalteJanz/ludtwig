use crate::check::rule::RuleContext;
use crate::{Rule, Severity};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxToken};

pub struct RuleUnknownToken;

impl Rule for RuleUnknownToken {
    fn name(&self) -> &'static str {
        "unknown-token"
    }

    fn check_token(&self, token: SyntaxToken, ctx: &mut RuleContext) -> Option<()> {
        if token.kind() != SyntaxKind::TK_UNKNOWN {
            return None;
        }

        let result = ctx
            .create_result(self.name(), Severity::Info, "Unknown syntax token found")
            .primary_note(token.text_range(), "Unknown syntax token encountered here");
        ctx.add_result(result);

        None
    }
}

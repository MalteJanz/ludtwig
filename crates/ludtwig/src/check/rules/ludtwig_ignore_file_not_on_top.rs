use crate::Severity;
use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext};
use ludtwig_parser::syntax::typed::{AstNode, LudtwigDirectiveFileIgnore};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

pub struct RuleLudtwigIgnoreFileNotOnTop;

impl Rule for RuleLudtwigIgnoreFileNotOnTop {
    fn name(&self) -> &'static str {
        "ludtwig-ignore-file-not-on-top"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let directive = LudtwigDirectiveFileIgnore::cast(node)?;
        let parent = directive.syntax().parent()?;

        if parent.kind() != SyntaxKind::ROOT {
            let result = self.create_result( Severity::Error, "ludtwig-ignore-file directive must be on the top level in a file otherwise it is discarded!")
                .primary_note(directive.syntax().text_range(), "move this to the top level of the file (ideally the first line)");

            return Some(vec![result]);
        }

        None
    }
}

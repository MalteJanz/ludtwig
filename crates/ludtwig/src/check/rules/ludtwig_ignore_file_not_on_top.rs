use crate::check::rule::{Rule, RuleContext};
use crate::Severity;
use ludtwig_parser::syntax::typed::{AstNode, LudtwigDirectiveFileIgnore};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

#[derive(Clone)]
pub struct RuleLudtwigIgnoreFileNotOnTop;

impl Rule for RuleLudtwigIgnoreFileNotOnTop {
    fn name(&self) -> &'static str {
        "ludtwig-ignore-file-not-on-top"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let directive = LudtwigDirectiveFileIgnore::cast(node)?;
        let parent = directive.syntax().parent()?;

        if parent.kind() != SyntaxKind::ROOT {
            let result = ctx.create_result(self.name(), Severity::Error, "ludtwig-ignore-file directive must be on the top level in a file otherwise it is discarded!")
                .primary_note(directive.syntax().text_range(), "move this to the top level of the file (ideally the first line)");

            ctx.add_result(result);
        }

        None
    }
}

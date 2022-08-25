use crate::check::rule::{Rule, RuleContext, Severity};
use ludtwig_parser::syntax::typed::{AstNode, TwigStartingBlock};
use ludtwig_parser::syntax::untyped::SyntaxNode;

pub struct RuleTwigBlockNameSnakeCase;

impl Rule for RuleTwigBlockNameSnakeCase {
    fn name(&self) -> &'static str {
        "twig-block-name-snake-case"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let block_name = TwigStartingBlock::cast(node)?.name()?;
        if !block_name
            .text()
            .chars()
            .all(|c| (c.is_ascii_alphanumeric() && c.is_ascii_lowercase()) || c == '_')
        {
            // name contains invalid characters
            let result = ctx
                .create_result(
                    self.name(),
                    Severity::Warning,
                    "Block name is not written in snake_case",
                )
                .primary_note(
                    block_name.text_range(),
                    "help: rename this block in snake_case",
                );

            ctx.add_result(result);
        }
        None
    }
}

// TODO: write a test (after parsing / generating syntax trees is possible)

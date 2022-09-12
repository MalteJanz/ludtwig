use crate::check::rule::{Rule, RuleContext, Severity};
use crate::config::LineEnding;
use ludtwig_parser::syntax::typed::{AstNode, TwigBlock};
use ludtwig_parser::syntax::untyped::{
    debug_tree, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TemplateLanguage, TextRange,
    TextSize,
};

pub struct RuleTwigBlockLineBreaks;

impl Rule for RuleTwigBlockLineBreaks {
    fn name(&self) -> &'static str {
        "twig-block-line-breaks"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let block = TwigBlock::cast(node)?;

        // early return if no parent exists or the parent is also a block
        if block.syntax().parent().map_or(true, |p| {
            matches!(p.kind(), SyntaxKind::TWIG_BLOCK | SyntaxKind::ROOT)
        }) {
            // TODO: what about first child of block?
            return None;
        }

        println!("found {:?}", block);

        let starting_block = block.starting_block()?;
        let first_child_token = starting_block
            .syntax()
            .children_with_tokens()
            .filter_map(|element| match element {
                SyntaxElement::Node(_) => None,
                SyntaxElement::Token(t) => Some(t),
            })
            .next()?;

        let expected_line_break = match ctx.config().format.line_ending {
            LineEnding::UnixLF => "\n",
            LineEnding::WindowsCRLF => "\r\n",
        };
        let line_break_amount = if ctx.config().format.linebreaks_around_blocks {
            2
        } else {
            1
        };
        let expected_str = expected_line_break.repeat(line_break_amount);

        match first_child_token.kind() {
            SyntaxKind::TK_LINE_BREAK => {
                // validate existing line break
                if first_child_token.text() != expected_str {
                    let result = ctx
                        .create_result(
                            self.name(),
                            Severity::Warning,
                            "Wrong line break around block",
                        )
                        .primary_note(
                            first_child_token.text_range(),
                            format!("Expected {} line breaks here", line_break_amount),
                        )
                        .suggestion(
                            first_child_token.text_range(),
                            expected_str,
                            format!("Change to {} line breaks", line_break_amount),
                        );
                    ctx.add_result(result);
                }
            }
            _ => {
                let range =
                    TextRange::at(first_child_token.text_range().start(), TextSize::from(0));

                // missing line break
                let result = ctx
                    .create_result(
                        self.name(),
                        Severity::Warning,
                        "Missing line break around block",
                    )
                    .primary_note(
                        range,
                        format!("Expected {} line breaks before this", line_break_amount),
                    )
                    .suggestion(
                        range,
                        expected_str,
                        format!("Add {} line breaks before this", line_break_amount),
                    );
                ctx.add_result(result);
            }
        }

        None
    }

    fn check_root(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        // TODO: remove me
        println!("{}", debug_tree(&node));
        None
    }
}

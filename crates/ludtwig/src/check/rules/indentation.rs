use crate::check::rule::{Rule, RuleContext, Severity};
use crate::config::IndentationMode;
use ludtwig_parser::syntax::untyped::{
    debug_tree, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize, WalkEvent,
};

pub struct RuleIndentation;

impl Rule for RuleIndentation {
    fn name(&self) -> &'static str {
        "indentation"
    }

    fn check_root(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let mut line_break_encountered = true;
        let mut indentation = 0;
        let indent_block_children = ctx.config().format.indent_children_of_blocks;

        println!("tree: {}", debug_tree(&node));

        for walk in node.preorder_with_tokens() {
            match walk {
                WalkEvent::Enter(element) => {
                    match element {
                        SyntaxElement::Token(t) if t.kind() == SyntaxKind::TK_LINE_BREAK => {
                            line_break_encountered = true;
                        }
                        SyntaxElement::Token(t)
                            if t.kind() == SyntaxKind::TK_WHITESPACE && line_break_encountered =>
                        {
                            handle_first_whitespace_in_line(t, indentation, ctx);
                            line_break_encountered = false;
                        }
                        SyntaxElement::Token(_) => {
                            // any other token encountered
                            line_break_encountered = false;
                        }
                        SyntaxElement::Node(n)
                            if indent_block_children && n.kind() == SyntaxKind::TWIG_BLOCK =>
                        {
                            indentation += 1;
                        }
                        SyntaxElement::Node(n) if n.kind() == SyntaxKind::HTML_TAG => {
                            indentation += 1;
                        }
                        _ => {}
                    }
                }
                WalkEvent::Leave(element) => match element {
                    SyntaxElement::Node(n)
                        if indent_block_children && n.kind() == SyntaxKind::TWIG_BLOCK =>
                    {
                        indentation -= 1;
                    }
                    SyntaxElement::Node(n) if n.kind() == SyntaxKind::HTML_TAG => {
                        indentation -= 1;
                    }
                    _ => {}
                },
            }
        }

        None
    }
}

fn handle_first_whitespace_in_line(token: SyntaxToken, indentation: usize, ctx: &mut RuleContext) {
    // TODO: this gets the wrong indentation!
    // inspect the syntax tree carefully again and find a solution for this
    println!("{:?} should indent {}", token, indentation);
}

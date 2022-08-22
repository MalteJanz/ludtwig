use super::untyped::SyntaxElement;
use super::untyped::SyntaxKind;
use super::untyped::SyntaxNode;
use super::untyped::SyntaxToken;

/// So far, we've been working with a homogeneous untyped tree.
/// It's nice to provide generic tree operations, like traversals,
/// but it's a bad fit for semantic analysis.
/// The rowan crate itself does not provide AST facilities directly,
/// but it is possible to layer AST on top of `SyntaxNode` API.
///
/// Let's define AST nodes.
/// It'll be quite a bunch of repetitive code, so we'll use a macro.
///
/// For a real language, you'd want to generate an AST. I find a
/// combination of `serde`, `ron` and `tera` crates invaluable for that!
macro_rules! ast_node {
    ($ast:ident, $kind:path) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        struct $ast(SyntaxNode);
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
                }
            }
            #[allow(unused)]
            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

ast_node!(TwigBlock, SyntaxKind::TWIG_BLOCK);
impl TwigBlock {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|el| match el {
                SyntaxElement::Node(_) => None,
                SyntaxElement::Token(t) => Some(t),
            })
            .skip_while(|n| n.kind() != SyntaxKind::TWIG_KEYWORD_BLOCK)
            .skip(1)
            .skip_while(|n| {
                n.kind() == SyntaxKind::WHITESPACE || n.kind() == SyntaxKind::LINE_BREAK
            })
            .next()
            .filter(|n| n.kind() == SyntaxKind::WORD)
    }
}

ast_node!(TwigVar, SyntaxKind::TWIG_VAR);
ast_node!(HtmlAttribute, SyntaxKind::HTML_ATTRIBUTE);
ast_node!(HtmlTag, SyntaxKind::HTML_TAG);
ast_node!(Root, SyntaxKind::ROOT);

#[cfg(test)]
mod tests {
    use super::super::untyped::{build_example_tree, print_syntax_tree, print_syntax_tree_text};
    use super::*;

    #[test]
    fn it_works() {
        let tree = build_example_tree();

        println!("syntax tree:");
        print_syntax_tree(0, tree.clone().into());

        let twig_block = tree.children().find_map(TwigBlock::cast).unwrap();

        for child in twig_block.syntax().children_with_tokens() {
            println!("{:?}", child);
        }

        println!();

        println!("block name {:?}", twig_block.name());
    }
}

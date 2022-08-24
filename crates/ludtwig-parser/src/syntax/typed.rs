use super::untyped::{SyntaxKind, SyntaxNode, SyntaxToken, TwigHtmlLanguage};

pub use rowan::ast::support;
pub use rowan::ast::AstChildren;
pub use rowan::ast::AstNode;

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
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $ast {
            pub(crate) syntax: SyntaxNode,
        }

        impl AstNode for $ast {
            type Language = TwigHtmlLanguage;

            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
            where
                Self: Sized,
            {
                kind == $kind
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
            where
                Self: Sized,
            {
                if Self::can_cast(node.kind()) {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                &self.syntax
            }
        }
    };
}

ast_node!(TwigBlock, SyntaxKind::TWIG_BLOCK);
ast_node!(TwigStartingBlock, SyntaxKind::TWIG_STARTING_BLOCK);
ast_node!(Body, SyntaxKind::BODY);
ast_node!(TwigEndingBlock, SyntaxKind::TWIG_ENDING_BLOCK);
ast_node!(TwigVar, SyntaxKind::TWIG_VAR);
ast_node!(HtmlAttribute, SyntaxKind::HTML_ATTRIBUTE);
ast_node!(HtmlString, SyntaxKind::HTML_STRING);
ast_node!(HtmlTag, SyntaxKind::HTML_TAG);
ast_node!(HtmlStartingTag, SyntaxKind::HTML_STARTING_TAG);
ast_node!(HtmlEndingTag, SyntaxKind::HTML_ENDING_TAG);
ast_node!(Root, SyntaxKind::ROOT);

impl TwigStartingBlock {
    /// Name of the twig block
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::WORD)
    }
}

impl TwigBlock {
    /// Name of the twig block
    pub fn name(&self) -> Option<SyntaxToken> {
        match self.starting_block() {
            None => None,
            Some(n) => n.name(),
        }
    }

    pub fn starting_block(&self) -> Option<TwigStartingBlock> {
        support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Body> {
        support::child(&self.syntax)
    }

    pub fn ending_block(&self) -> Option<TwigEndingBlock> {
        support::child(&self.syntax)
    }
}

impl HtmlStartingTag {
    /// Name of the tag
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::WORD)
    }

    /// Attributes of the tag
    pub fn attributes(&self) -> AstChildren<HtmlAttribute> {
        support::children(&self.syntax)
    }
}

impl HtmlAttribute {
    /// Name of the attribute (left side of the equal sign)
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::WORD)
    }

    /// Value of the attribute
    pub fn value(&self) -> Option<HtmlString> {
        support::child(&self.syntax)
    }
}

impl HtmlTag {
    /// Name of the tag
    pub fn name(&self) -> Option<SyntaxToken> {
        match self.starting_tag() {
            None => None,
            Some(n) => n.name(),
        }
    }

    /// Attributes of the tag
    pub fn attributes(&self) -> AstChildren<HtmlAttribute> {
        match self.starting_tag() {
            Some(n) => n.attributes(),
            // create an iterator for HtmlAttribute over the tag itself, which should yield no results
            None => support::children(&self.syntax),
        }
    }

    pub fn starting_tag(&self) -> Option<HtmlStartingTag> {
        support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<Body> {
        support::child(&self.syntax)
    }

    pub fn ending_tag(&self) -> Option<HtmlEndingTag> {
        support::child(&self.syntax)
    }
}

#[cfg(test)]
mod tests {
    use super::super::untyped::build_example_tree;
    use super::*;

    #[test]
    fn test_twig_block_getters() {
        let tree = build_example_tree();

        let typed: TwigBlock = support::child(&tree).unwrap();
        assert!(typed.name().is_some());
        assert_eq!(typed.name().unwrap().text(), "my_block");
    }

    #[test]
    fn test_html_tag_getters() {
        let tree = build_example_tree();

        let typed = tree.descendants().find_map(HtmlTag::cast).unwrap();
        assert!(typed.name().is_some());
        assert_eq!(typed.name().unwrap().text(), "div");

        assert!(typed.starting_tag().is_some());
        let first_attribute = typed.attributes().next().unwrap();
        assert_eq!(first_attribute.name().unwrap().text(), "class");
        assert_eq!(first_attribute.value().unwrap().syntax().text(), "my-div");
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    /*
    Tokens
     */
    WHITESPACE = 0,
    LINE_BREAK,
    WORD, // a single word containing only characters or symbols
    // Twig specific
    TWIG_BLOCK_START,
    TWIG_BLOCK_END,
    TWIG_VAR_START,
    TWIG_VAR_END,
    TWIG_KEYWORD_BLOCK,
    TWIG_KEYWORD_ENDBLOCK,
    // Html specific
    HTML_OPENING_ANGLE_BRACKET,
    HTML_CLOSING_ANGLE_BRACKET,
    HTML_FORWARD_SLASH,
    HTML_EQUAL_SIGN,
    HTML_DOUBLE_QUOTE,
    /*
    Composite nodes (which have children and ast / typed counterparts)
    */
    BODY,
    TWIG_BLOCK,
    TWIG_STARTING_BLOCK,
    TWIG_ENDING_BLOCK,

    TWIG_VAR,
    HTML_ATTRIBUTE,
    HTML_STRING, // used as attribute values
    HTML_TAG,
    HTML_STARTING_TAG,
    HTML_ENDING_TAG,
    ERROR, // contains invalid syntax (used for error recovery)
    ROOT, // top-level node: list of elements inside the template (must be last item of enum for safety check!)
}

/// Some boilerplate is needed, as rowan settled on using its own
/// `struct SyntaxKind(u16)` internally, instead of accepting the
/// user's `enum SyntaxKind` as a type parameter.
///
/// First, to easily pass the enum variants into rowan via `.into()`:
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TwigHtmlLanguage {}
impl rowan::Language for TwigHtmlLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
pub use rowan::GreenNode;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
pub use rowan::GreenNodeBuilder;

/// To work with the parse results we need a view into the
/// green tree - the Syntax tree.
/// It is also immutable, like a GreenNode,
/// but it contains parent pointers, offsets, and
/// has identity semantics.

pub type SyntaxNode = rowan::SyntaxNode<TwigHtmlLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<TwigHtmlLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<TwigHtmlLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<TwigHtmlLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<TwigHtmlLanguage>;

pub use rowan::TextRange;
pub use rowan::WalkEvent;

pub fn print_syntax_tree(indent: usize, element: SyntaxElement) {
    print!("{:indent$}", "", indent = indent);
    match element {
        rowan::NodeOrToken::Node(node) => {
            println!("- {:?}", node);
            for child in node.children_with_tokens() {
                print_syntax_tree(indent + 2, child);
            }
        }

        rowan::NodeOrToken::Token(token) => println!("- {:?}", token),
    }
}

// TODO: remove me when parser is implemented
pub fn build_example_tree() -> SyntaxNode {
    let mut builder = GreenNodeBuilder::new();
    // Make sure that the root node covers all source
    builder.start_node(SyntaxKind::ROOT.into());

    // Outer twig block
    builder.start_node(SyntaxKind::TWIG_BLOCK.into());
    builder.start_node(SyntaxKind::TWIG_STARTING_BLOCK.into());
    builder.token(SyntaxKind::TWIG_BLOCK_START.into(), "{%");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::TWIG_KEYWORD_BLOCK.into(), "block");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::WORD.into(), "my-block"); // temporary issue for rule test
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::TWIG_BLOCK_END.into(), "%}");
    builder.finish_node(); // close TWIG_STARTING_BLOCK
    builder.start_node(SyntaxKind::BODY.into());
    builder.token(SyntaxKind::LINE_BREAK.into(), "\n");
    builder.token(SyntaxKind::WHITESPACE.into(), "    ");

    // Inner div
    builder.start_node(SyntaxKind::HTML_TAG.into());
    builder.start_node(SyntaxKind::HTML_STARTING_TAG.into());
    builder.token(SyntaxKind::HTML_OPENING_ANGLE_BRACKET.into(), "<");
    builder.token(SyntaxKind::WORD.into(), "div");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");

    // Inner div attribute
    builder.start_node(SyntaxKind::HTML_ATTRIBUTE.into());
    builder.token(SyntaxKind::WORD.into(), "class");
    builder.token(SyntaxKind::HTML_EQUAL_SIGN.into(), "=");
    builder.token(SyntaxKind::HTML_DOUBLE_QUOTE.into(), "\"");
    builder.start_node(SyntaxKind::HTML_STRING.into());
    builder.token(SyntaxKind::WORD.into(), "my-div");
    builder.finish_node(); // Close HTML_STRING
    builder.token(SyntaxKind::HTML_DOUBLE_QUOTE.into(), "\"");

    // Close inner div attribute
    builder.finish_node();

    builder.token(SyntaxKind::HTML_CLOSING_ANGLE_BRACKET.into(), ">");
    builder.finish_node(); // close HTML_STARTING_TAG
    builder.start_node(SyntaxKind::BODY.into());
    builder.token(SyntaxKind::LINE_BREAK.into(), "\n");
    builder.token(SyntaxKind::WHITESPACE.into(), "        ");
    builder.token(SyntaxKind::WORD.into(), "world");
    builder.token(SyntaxKind::LINE_BREAK.into(), "\n");
    builder.token(SyntaxKind::WHITESPACE.into(), "    ");
    builder.finish_node(); // close BODY
    builder.start_node(SyntaxKind::HTML_ENDING_TAG.into());
    builder.token(SyntaxKind::HTML_OPENING_ANGLE_BRACKET.into(), "<");
    builder.token(SyntaxKind::HTML_FORWARD_SLASH.into(), "/");
    builder.token(SyntaxKind::WORD.into(), "div");
    builder.token(SyntaxKind::HTML_CLOSING_ANGLE_BRACKET.into(), ">");
    builder.finish_node(); // close HTML_ENDING_TAG
    builder.token(SyntaxKind::LINE_BREAK.into(), "\n");

    // Close inner div
    builder.finish_node();

    builder.finish_node(); // close BODY
    builder.start_node(SyntaxKind::TWIG_ENDING_BLOCK.into());
    builder.token(SyntaxKind::TWIG_BLOCK_START.into(), "{%");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::TWIG_KEYWORD_ENDBLOCK.into(), "endblock");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.start_node(SyntaxKind::ERROR.into());
    builder.token(SyntaxKind::WORD.into(), "SomeInvalidSyntax");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.finish_node(); // close ERROR
    builder.token(SyntaxKind::TWIG_BLOCK_END.into(), "%}");
    builder.finish_node(); // close TWIG_ENDING_BLOCK
    builder.token(SyntaxKind::LINE_BREAK.into(), "\n");

    // Close outer twig block
    builder.finish_node();

    // Close the root node.
    builder.finish_node();
    SyntaxNode::new_root(builder.finish())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let tree = build_example_tree();
        println!("syntax tree underlying text:");
        println!("{}", tree.text());

        println!("syntax tree:");
        print_syntax_tree(0, tree.into());
    }

    #[test]
    fn it_should_print_the_original_text() {
        let tree = build_example_tree();
        assert_eq!(
            tree.text(),
            "{% block my-block %}\n    <div class=\"my-div\">\n        world\n    </div>\n{% endblock SomeInvalidSyntax %}\n"
        );
    }
}

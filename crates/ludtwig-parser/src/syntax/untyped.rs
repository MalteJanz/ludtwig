use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    /*
    Tokens without any meaning / semantic attached to them.
    These are produced by the lexer and provide a small abstraction over plain text
     */
    #[regex(r"[ \t]+")]
    TK_WHITESPACE = 0,
    #[regex(r"[\n\r\f]+")]
    TK_LINE_BREAK,
    /// a single word containing only characters, numbers or symbols
    #[regex(r"[a-zA-Z0-9_.,@:#!$&-]+")]
    TK_WORD,
    #[token("<")]
    TK_LESS_THAN,
    #[token("</")]
    TK_LESS_THAN_SLASH,
    #[token(">")]
    TK_GREATER_THAN,
    #[token("/>")]
    TK_SLASH_GREATER_THAN,
    #[token("=")]
    TK_EQUAL,
    #[token("\"")]
    TK_DOUBLE_QUOTES,
    #[token("{%")]
    TK_CURLY_PERCENT,
    #[token("%}")]
    TK_PERCENT_CURLY,
    #[token("{{")]
    TK_OPEN_CURLY_CURLY,
    #[token("}}")]
    TK_CLOSE_CURLY_CURLY,
    #[token("block")]
    TK_BLOCK,
    #[token("endblock")]
    TK_ENDBLOCK,

    /*
    Composite nodes (which can have children and ast / typed counterparts)
    These do have a meaning and are constructed by the parser
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
    /*
    Special Nodes
     */
    #[error]
    ERROR, // contains invalid syntax (used for error recovery). Can be a node or token
    /// SAFETY: this must be the last enum element for u16 conversion!
    ROOT, // top-level node: list of elements inside the template (must be last item of enum for safety check!)
}

#[macro_export]
macro_rules! T {
    [ws] => { $crate::syntax::untyped::SyntaxKind::TK_WHITESPACE };
    [lb] => { $crate::syntax::untyped::SyntaxKind::TK_LINE_BREAK };
    [word] => { $crate::syntax::untyped::SyntaxKind::TK_WORD };
    ["<"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN };
    ["</"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_SLASH };
    [">"] => { $crate::syntax::untyped::SyntaxKind::TK_GREATER_THAN };
    ["/>"] => { $crate::syntax::untyped::SyntaxKind::TK_SLASH_GREATER_THAN };
    ["="] => { $crate::syntax::untyped::SyntaxKind::TK_EQUAL };
    ["\""] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_QUOTES };
    ["{%"] => { $crate::syntax::untyped::SyntaxKind::TK_CURLY_PERCENT };
    ["%}"] => { $crate::syntax::untyped::SyntaxKind::TK_PERCENT_CURLY };
    ["{{"] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_CURLY_CURLY };
    ["}}"] => { $crate::syntax::untyped::SyntaxKind::TK_CLOSE_CURLY_CURLY };
    ["block"] => { $crate::syntax::untyped::SyntaxKind::TK_BLOCK };
    ["endblock"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDBLOCK };
}

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        // Add comments and other non interesting things for the parser here in the future
        matches!(self, T![ws] | T![lb])
    }
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

pub use rowan::Language;

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TemplateLanguage {}
impl Language for TemplateLanguage {
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

pub type SyntaxNode = rowan::SyntaxNode<TemplateLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<TemplateLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<TemplateLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<TemplateLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<TemplateLanguage>;

pub use rowan::TextRange;
pub use rowan::TextSize;
pub use rowan::WalkEvent;

pub fn debug_tree(syntax_node: SyntaxNode) -> String {
    let formatted = format!("{:#?}", syntax_node);
    // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
    formatted[0..formatted.len() - 1].to_string()
}

// TODO: remove me when parser is implemented
pub fn build_example_tree() -> SyntaxNode {
    let mut builder = GreenNodeBuilder::new();
    // Make sure that the root node covers all source
    builder.start_node(SyntaxKind::ROOT.into());

    // Outer twig block
    builder.start_node(SyntaxKind::TWIG_BLOCK.into());
    builder.start_node(SyntaxKind::TWIG_STARTING_BLOCK.into());
    builder.token(T!["{%"].into(), "{%");
    builder.token(T![ws].into(), " ");
    builder.token(T!["block"].into(), "block");
    builder.token(T![ws].into(), " ");
    builder.token(T![word].into(), "my-block"); // temporary issue for rule test
    builder.token(T![ws].into(), " ");
    builder.token(T!["%}"].into(), "%}");
    builder.finish_node(); // close TWIG_STARTING_BLOCK
    builder.start_node(SyntaxKind::BODY.into());
    builder.token(T![lb].into(), "\n");
    builder.token(T![ws].into(), "    ");

    // Inner div
    builder.start_node(SyntaxKind::HTML_TAG.into());
    builder.start_node(SyntaxKind::HTML_STARTING_TAG.into());
    builder.token(T!["<"].into(), "<");
    builder.token(T![word].into(), "div");
    builder.token(T![ws].into(), " ");

    // Inner div attribute
    builder.start_node(SyntaxKind::HTML_ATTRIBUTE.into());
    builder.token(T![word].into(), "claSs");
    builder.token(T!["="].into(), "=");
    builder.token(T!["\""].into(), "\"");
    builder.start_node(SyntaxKind::HTML_STRING.into());
    builder.token(T![word].into(), "my-div");
    builder.finish_node(); // Close HTML_STRING
    builder.token(T!["\""].into(), "\"");

    // Close inner div attribute
    builder.finish_node();

    builder.token(T![">"].into(), ">");
    builder.finish_node(); // close HTML_STARTING_TAG
    builder.start_node(SyntaxKind::BODY.into());
    builder.token(T![lb].into(), "\n");
    builder.token(T![ws].into(), "        ");
    builder.token(T![word].into(), "world");
    builder.token(T![lb].into(), "\n");
    builder.token(T![ws].into(), "    ");
    builder.finish_node(); // close BODY
    builder.start_node(SyntaxKind::HTML_ENDING_TAG.into());
    builder.token(T!["</"].into(), "</");
    builder.token(T![word].into(), "div");
    builder.token(T![">"].into(), ">");
    builder.finish_node(); // close HTML_ENDING_TAG
    builder.token(T![lb].into(), "\n");

    // Close inner div
    builder.finish_node();

    builder.finish_node(); // close BODY
    builder.start_node(SyntaxKind::TWIG_ENDING_BLOCK.into());
    builder.token(T!["{%"].into(), "{%");
    builder.token(T![ws].into(), " ");
    builder.token(T!["endblock"].into(), "endblock");
    builder.token(T![ws].into(), " ");
    builder.start_node(SyntaxKind::ERROR.into());
    builder.token(T![word].into(), "SomeInvalidSyntax");
    builder.token(T![ws].into(), " ");
    builder.finish_node(); // close ERROR
    builder.token(T!["%}"].into(), "%}");
    builder.finish_node(); // close TWIG_ENDING_BLOCK
    builder.token(T![lb].into(), "\n");

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

        println!("syntax tree:\n{}", debug_tree(tree));
    }

    #[test]
    fn it_should_print_the_original_text() {
        let tree = build_example_tree();
        assert_eq!(
            tree.text(),
            "{% block my-block %}\n    <div claSs=\"my-div\">\n        world\n    </div>\n{% endblock SomeInvalidSyntax %}\n"
        );
    }
}

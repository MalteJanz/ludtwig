//! The AST (abstract syntax tree) represents the template syntax in a structured format.
//! It mainly consists of an enum [SyntaxNode] which has different variants for each syntax.

/// The base enum for each syntax element in a document.
/// Each variant represents some sort of structured representation of the document syntax.
/// This is the foundation for the AST (abstract syntax tree) that is produced by the parser.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SyntaxNode {
    /// The root of the AST which contains other [SyntaxNode] values.
    Root(Vec<SyntaxNode>),

    /// Plain old Html tag that can have any value as a name (even vue components for example).
    /// It may also have attributes.
    /// It can also have children which are only a list of [SyntaxNode] instances.
    /// For example `<h2 class="bold">...</h2>`
    Tag(Tag),

    /// Basically only plain text but does only represent text without line break characters or indentation.
    Plain(Plain),

    /// Some sort of whitespace (can be anything from spaces to tabs to line breaks).
    /// Multiple sequential forms of whitespace will always result in only one instance of this.
    Whitespace,

    /// Comments in Html that look like `<!-- some comment -->`
    HtmlComment(HtmlComment),

    /// Some expression to output something like  
    /// twig: `{{ my_counter }}` (can be php)  
    /// vue: `{{ myCounter }}` (can be javascript)
    OutputExpression(OutputExpression),

    /// twig block structure which has a name and contains other [SyntaxNode] values as children.
    /// For example `{% block my_block_name %}...{% endblock %}`
    TwigBlock(TwigBlock),

    /// Comment in twig syntax: `{# some comment #}`
    TwigComment(TwigComment),

    /// twig parent call: `{% parent %}`.
    /// This variant will eventually be merged with a more general execute statement variant.
    TwigParentCall,
    // Some execute statement that has no children.
    // twig: `{% set foo = 'foo' %}`
    //Statement(),
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlAttribute {
    pub name: String,
    pub value: Option<String>,
}

impl HtmlAttribute {
    pub fn new(name: String, value: Option<String>) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Tag {
    pub name: String,
    pub self_closed: bool,
    pub attributes: Vec<HtmlAttribute>,
    pub children: Vec<SyntaxNode>,
}

impl Tag {
    pub fn new(
        name: String,
        self_closed: bool,
        attributes: Vec<HtmlAttribute>,
        children: Vec<SyntaxNode>,
    ) -> Self {
        Self {
            name,
            self_closed,
            attributes,
            children,
        }
    }
}

/// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Plain {
    pub plain: String,
}

impl Plain {
    pub fn new(plain: String) -> Self {
        Self { plain }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlComment {
    pub content: String,
}

impl HtmlComment {
    pub fn new(content: String) -> Self {
        Self { content }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct OutputExpression {
    pub content: String,
}

impl OutputExpression {
    pub fn new(content: String) -> Self {
        Self { content }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigBlock {
    pub name: String,
    pub children: Vec<SyntaxNode>,
}

impl TwigBlock {
    pub fn new(name: String, children: Vec<SyntaxNode>) -> Self {
        Self { name, children }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigComment {
    pub content: String,
}

impl TwigComment {
    pub fn new(content: String) -> Self {
        Self { content }
    }
}

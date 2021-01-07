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
    /// twig: `{{ my_counter|e }}` (can be php)  
    /// vue: `{{ myCounter.count }}` (can be javascript)
    OutputExpression(OutputExpression),

    /// Comment in twig syntax: `{# some comment #}`
    TwigComment(TwigComment),

    /// Some execute statement that has no children / has no closing syntax.
    ///
    /// # Examples
    /// `{% set foo = 'foo' %}`
    ///
    /// or `{% parent %}`
    ///
    /// or ...
    TwigStatement(TwigStatement),

    /// Some hierarchical twig syntax.
    ///
    /// # Examples
    /// `{% block my_block_name %}...{% endblock %}`
    ///
    /// # Notes
    /// This is preferred over [TwigStatement] by the parser if it sees special keywords like `block` right after the `{% `.
    ///
    TwigStructure(TwigStructure),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TwigStatement {
    /// For a first implementation there is no difference between all the possibilities of twig execute statements.
    /// This may change in the future with a more advanced parser.
    ///
    /// # Examples
    /// `{% set foo = 'foo' %}` -> `Raw("set foo = 'foo'")`
    ///
    /// `{% parent %}` -> `Raw("parent")`
    ///
    /// `{% include 'header.html' %}` -> `Raw("include 'header.html'")`
    ///
    /// ...
    Raw(String),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TwigStructure {
    /// Twig block structure which has a name and contains other [SyntaxNode] values as children.
    /// For example `{% block my_block_name %}...{% endblock %}`
    TwigBlock(TwigBlock),

    /// Twig for block.
    ///
    /// # Example
    /// ```twig
    /// {% for user in users %}
    ///      <li>{{ user.username|e }}</li>
    /// {% endfor %}
    /// ```
    TwigFor(TwigFor),

    /// Twig if block.
    ///
    /// # Example
    /// ```twig
    /// {% if product.stock > 10 %}
    ///    Available
    /// {% elseif product.stock > 0 %}
    ///    Only {{ product.stock }} left!
    /// {% else %}
    ///    Sold-out!
    /// {% endif %}
    /// ```
    TwigIf(TwigIf),

    /// Twig apply block.
    ///
    /// # Example
    /// ```twig
    /// {% apply upper %}
    ///     This text becomes uppercase
    /// {% endapply %}
    /// ```
    TwigApply(TwigApply),

    /// Twig set block with no '='. captures the children.
    ///
    /// # Example
    /// ```twig
    /// {% set foo %}
    ///     <div>
    ///         hello world
    ///     </div>
    /// {% endset %}
    /// ```
    TwigSetCapture(TwigSetCapture),
}
/*
impl HasChildren for TwigStructure {
    fn get_children(&self) -> &[SyntaxNode] {
        match self {
            TwigStructure::TwigBlock(p) => p.get_children(),
            TwigStructure::TwigFor(p) => p.get_children(),
            TwigStructure::TwigIf(p) => p.get_children(), // how to handle this? is it really needed?
            TwigStructure::TwigApply(p) => p.get_children(),
            TwigStructure::TwigSetCapture(p) => p.get_children(),
        }
    }
}
*/
/// Every AST data structure that implements this trait has a list of children (of type [SyntaxNode]).
pub trait HasChildren {
    /// Get the children of this AST node.
    fn get_children(&self) -> &[SyntaxNode];
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

impl HasChildren for Tag {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
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
pub struct TwigComment {
    pub content: String,
}

impl TwigComment {
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

impl HasChildren for TwigBlock {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigFor {
    pub expression: String,
    pub children: Vec<SyntaxNode>,
}

impl TwigFor {
    pub fn new(expression: String, children: Vec<SyntaxNode>) -> Self {
        Self {
            expression,
            children,
        }
    }
}

impl HasChildren for TwigFor {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
    }
}

/// Represents a full set of if / elseif / else expressions.
///
/// # Example
/// ```twig
/// {% if product.stock > 10 %}
///    Available
/// {% elseif product.stock > 0 %}
///    Only {{ product.stock }} left!
/// {% else %}
///    Sold-out!
/// {% endif %}
/// ```
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigIf {
    pub if_arms: Vec<TwigIfArm>,
}

/// Represents one Arm of a possible multi arm if
///
/// # Example
/// ```twig
/// {% if product.stock > 10 %}
///    Available
/// ...
/// ```
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigIfArm {
    /// 'if' and 'elseif' arms have an expression,
    /// otherwise it is an 'else' arm.
    pub expression: Option<String>,
    pub children: Vec<SyntaxNode>,
}

impl HasChildren for TwigIfArm {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigApply {
    pub expression: String,
    pub children: Vec<SyntaxNode>,
}

impl TwigApply {
    pub fn new(expression: String, children: Vec<SyntaxNode>) -> Self {
        Self {
            expression,
            children,
        }
    }
}

impl HasChildren for TwigApply {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigSetCapture {
    pub name: String,
    pub children: Vec<SyntaxNode>,
}

impl TwigSetCapture {
    pub fn new(name: String, children: Vec<SyntaxNode>) -> Self {
        Self { name, children }
    }
}

impl HasChildren for TwigSetCapture {
    fn get_children(&self) -> &[SyntaxNode] {
        self.children.as_ref()
    }
}

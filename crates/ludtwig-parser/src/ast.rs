//! The AST (abstract syntax tree) represents the template syntax in a structured format.
//! It mainly consists of an enum [SyntaxNode] which has different variants for each syntax.

use std::fmt::{Display, Formatter};

// Re-export types for iteration over the AST from the private modules.
pub use crate::ast_context_iter::AstContextIterator;
pub use crate::ast_context_iter::IteratorContext;
pub use crate::ast_iter::AstIterator;

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
    TwigStructure(TwigStructure<SyntaxNode>),
}

impl SyntaxNode {
    pub fn is_whitespace(&self) -> bool {
        if let SyntaxNode::Whitespace = self {
            return true;
        }

        false
    }
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
pub enum TwigStructure<C> {
    /// Twig block structure which has a name and contains other [SyntaxNode] values as children.
    /// For example `{% block my_block_name %}...{% endblock %}`
    TwigBlock(TwigBlock<C>),

    /// Twig for block.
    ///
    /// # Example
    /// ```twig
    /// {% for user in users %}
    ///      <li>{{ user.username|e }}</li>
    /// {% endfor %}
    /// ```
    TwigFor(TwigFor<C>),

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
    TwigIf(TwigIf<C>),

    /// Twig apply block.
    ///
    /// # Example
    /// ```twig
    /// {% apply upper %}
    ///     This text becomes uppercase
    /// {% endapply %}
    /// ```
    TwigApply(TwigApply<C>),

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
    TwigSetCapture(TwigSetCapture<C>),
}

/// implement the display trait for TwigStructure<TagAttribute> to easily display the user
/// tag attributes as context, so they can find the tag that is causing parsing issues.
impl Display for TwigStructure<TagAttribute> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TwigStructure::TwigBlock(t) => {
                write!(f, "{{% block {} %}}", t.name)?;

                for child in &t.children {
                    write!(f, " {}", child)?;
                }

                write!(f, "{{% endblock %}}")
            }
            TwigStructure::TwigFor(t) => {
                write!(f, "{{% for {} %}}", t.expression)?;

                for child in &t.children {
                    write!(f, " {}", child)?;
                }

                write!(f, "{{% endfor %}}")
            }
            TwigStructure::TwigIf(t) => {
                for (i, arm) in t.if_arms.iter().enumerate() {
                    match (i, &arm.expression) {
                        (0, Some(expr)) => write!(f, "{{% if {} %}}", expr),
                        (_, Some(expr)) => write!(f, "{{% elseif {} %}}", expr),
                        (_, None) => write!(f, "{{% else %}}"),
                    }?;

                    for child in &arm.children {
                        write!(f, " {}", child)?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "{{% endif %}}")
            }
            TwigStructure::TwigApply(t) => {
                write!(f, "{{% apply {} %}}", t.expression)?;

                for child in &t.children {
                    write!(f, " {}", child)?;
                }

                write!(f, "{{% endapply %}}")
            }
            TwigStructure::TwigSetCapture(t) => {
                write!(f, "{{% set {} %}}", t.name)?;

                for child in &t.children {
                    write!(f, " {}", child)?;
                }

                write!(f, "{{% endset %}}")
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TagAttribute {
    HtmlAttribute(HtmlAttribute),
    TwigComment(TwigComment),
    TwigStructure(TwigStructure<TagAttribute>),
}

impl Display for TagAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TagAttribute::HtmlAttribute(a) => {
                write!(f, "{}", a)
            }
            TagAttribute::TwigComment(c) => {
                write!(f, "{}", c)
            }
            TagAttribute::TwigStructure(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

/// Represents any html tag attribute like `class="hello"`.
///
/// ## It could also contain output expressions like
/// ...="{{ ... }}"
/// {{ ... }}="..."  
/// {{ ... }}="{{ ... }}"
/// {{ ... }}
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

impl Display for HtmlAttribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.value {
            write!(f, r#"{}="{}""#, self.name, v)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Tag {
    pub name: String,
    pub self_closed: bool,
    pub attributes: Vec<TagAttribute>,
    pub children: Vec<SyntaxNode>,
}

impl Tag {
    pub fn new(
        name: String,
        self_closed: bool,
        attributes: Vec<TagAttribute>,
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
pub struct TwigComment {
    pub content: String,
}

impl TwigComment {
    pub fn new(content: String) -> Self {
        Self { content }
    }
}

impl Display for TwigComment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{# {} #}}", self.content)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigBlock<C> {
    pub name: String,
    pub children: Vec<C>,
}

impl<C> Default for TwigBlock<C> {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            children: vec![],
        }
    }
}

impl<C> TwigBlock<C> {
    pub fn new(name: String, children: Vec<C>) -> Self {
        Self { name, children }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigFor<C> {
    pub expression: String,
    pub children: Vec<C>,
}

impl<C> Default for TwigFor<C> {
    fn default() -> Self {
        Self {
            expression: "".to_string(),
            children: vec![],
        }
    }
}

impl<C> TwigFor<C> {
    pub fn new(expression: String, children: Vec<C>) -> Self {
        Self {
            expression,
            children,
        }
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigIf<C> {
    pub if_arms: Vec<TwigIfArm<C>>,
}

/// Represents one Arm of a possible multi arm if
///
/// # Example
/// ```twig
/// {% if product.stock > 10 %}
///    Available
/// ...
/// ```
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigIfArm<C> {
    /// 'if' and 'elseif' arms have an expression,
    /// otherwise it is an 'else' arm.
    pub expression: Option<String>,
    pub children: Vec<C>,
}

impl<C> Default for TwigIfArm<C> {
    fn default() -> Self {
        Self {
            expression: None,
            children: vec![],
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigApply<C> {
    pub expression: String,
    pub children: Vec<C>,
}

impl<C> Default for TwigApply<C> {
    fn default() -> Self {
        Self {
            expression: "".to_string(),
            children: vec![],
        }
    }
}

impl<C> TwigApply<C> {
    pub fn new(expression: String, children: Vec<C>) -> Self {
        Self {
            expression,
            children,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TwigSetCapture<C> {
    pub name: String,
    pub children: Vec<C>,
}

impl<C> Default for TwigSetCapture<C> {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            children: vec![],
        }
    }
}

impl<C> TwigSetCapture<C> {
    pub fn new(name: String, children: Vec<C>) -> Self {
        Self { name, children }
    }
}

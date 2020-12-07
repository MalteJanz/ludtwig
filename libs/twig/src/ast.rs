#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlAttribute {
    pub name: String,
    pub value: Option<String>,
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlTag {
    pub name: String,
    pub self_closed: bool,
    pub attributes: Vec<HtmlAttribute>,
    pub children: Vec<HtmlNode>,
}

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlPlain {
    pub plain: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct HtmlComment {
    pub content: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct VueBlock {
    pub content: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigBlock {
    pub name: String,
    pub children: Vec<HtmlNode>,
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct TwigComment {
    pub content: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum HtmlNode {
    Root(Vec<HtmlNode>),
    Tag(HtmlTag),
    Plain(HtmlPlain),
    Comment(HtmlComment),
    VueBlock(VueBlock),
    TwigBlock(TwigBlock),
    TwigParentCall,
    TwigComment(TwigComment),
    Whitespace,
}

use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq)]
pub struct HtmlTag<'a> {
    pub name: &'a str,
    pub self_closed: bool,
    pub arguments: HashMap<&'a str, &'a str>,
    pub children: Vec<HtmlNode<'a>>,
}

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug, Eq, PartialEq)]
pub struct HtmlPlain<'a> {
    pub plain: &'a str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct VueBlock<'a> {
    pub content: &'a str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TwigBlock<'a> {
    pub name: &'a str,
    pub children: Vec<HtmlNode<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum HtmlNode<'a> {
    Tag(HtmlTag<'a>),
    Plain(HtmlPlain<'a>),
    VueBlock(VueBlock<'a>),
    TwigBlock(TwigBlock<'a>),
}

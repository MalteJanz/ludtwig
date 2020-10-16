use nom::lib::std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq)]
pub struct HtmlTag<'a> {
    pub name: &'a str,
    pub self_closed: bool,
    pub attributes: BTreeMap<&'a str, &'a str>,
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
    TwigParentCall,
}

impl<'a> Default for HtmlTag<'a> {
    fn default() -> Self {
        HtmlTag {
            name: "",
            self_closed: false,
            attributes: BTreeMap::new(),
            children: Vec::new(),
        }
    }
}

impl<'a> Default for HtmlPlain<'a> {
    fn default() -> Self {
        HtmlPlain { plain: "" }
    }
}

impl<'a> Default for VueBlock<'a> {
    fn default() -> Self {
        VueBlock { content: "" }
    }
}

impl<'a> Default for TwigBlock<'a> {
    fn default() -> Self {
        TwigBlock {
            name: "",
            children: Vec::new(),
        }
    }
}

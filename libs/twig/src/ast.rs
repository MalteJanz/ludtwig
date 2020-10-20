use nom::lib::std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq)]
pub struct HtmlTag {
    pub name: String,
    pub self_closed: bool,
    pub attributes: BTreeMap<String, String>,
    pub children: Vec<HtmlNode>,
}

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug, Eq, PartialEq)]
pub struct HtmlPlain {
    pub plain: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct VueBlock {
    pub content: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TwigBlock {
    pub name: String,
    pub children: Vec<HtmlNode>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum HtmlNode {
    Root(Vec<HtmlNode>),
    Tag(HtmlTag),
    Plain(HtmlPlain),
    VueBlock(VueBlock),
    TwigBlock(TwigBlock),
    TwigParentCall,
    Whitespace,
}

impl Default for HtmlTag {
    fn default() -> Self {
        HtmlTag {
            name: "".to_string(),
            self_closed: false,
            attributes: BTreeMap::new(),
            children: Vec::new(),
        }
    }
}

impl Default for HtmlPlain {
    fn default() -> Self {
        HtmlPlain {
            plain: "".to_string(),
        }
    }
}

impl Default for VueBlock {
    fn default() -> Self {
        VueBlock {
            content: "".to_string(),
        }
    }
}

impl Default for TwigBlock {
    fn default() -> Self {
        TwigBlock {
            name: "".to_string(),
            children: Vec::new(),
        }
    }
}

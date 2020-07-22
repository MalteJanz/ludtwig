extern crate nom;
use nom::bytes::complete::{tag, take_till, take_till1, take_until};
use nom::character::is_space;
use nom::{
    branch::*, character::complete::*, combinator::*, multi::*, sequence::*, whitespace, IResult,
};
use std::fs;

#[derive(Debug)]
struct HtmlTag<'a> {
    name: &'a str,
    children: Vec<HtmlNode<'a>>,
}
/*
impl<'a> HtmlTag<'a> {
    fn new(tag: &'a str) -> Self {
        HtmlTag {
            name: tag,
            children: Vec::new(),
        }
    }

    fn add_child(&mut self, html_tag: HtmlTag<'a>) {
        self.children.push(html_tag);
    }
}
*/

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug)]
struct HtmlPlain<'a> {
    plain: &'a str,
}

#[derive(Debug)]
enum HtmlNode<'a> {
    Tag(HtmlTag<'a>),
    Plain(HtmlPlain<'a>),
}

fn html_open_tag(input: &str) -> IResult<&str, &str> {
    // TODO: implement self closing tags.
    // TODO: implement tags that are forbidden to be closed like img, input, br, hr, meta, etc.
    delimited(
        multispace0,
        delimited(
            tag("<"),
            terminated(take_till1(|c| c == ' ' || c == '>'), many0(none_of(">"))),
            tag(">"),
        ),
        multispace0,
    )(input)
}

fn html_close_tag<'a>(open_tag: &'a str) -> impl Fn(&'a str) -> IResult<&str, &str> {
    delimited(
        multispace0,
        delimited(
            tag("</"),
            terminated(tag(open_tag), many0(none_of(">"))),
            tag(">"),
        ),
        multispace0,
    )
}

fn html_plain_text(input: &str) -> IResult<&str, HtmlNode> {
    let (remaining, plain) = delimited(
        multispace0,
        take_till1(|c| c == '<' || c == '\r' || c == '\n'),
        multispace0,
    )(input)?;

    Ok((remaining, HtmlNode::Plain(HtmlPlain { plain })))
}

fn html_tag_content(input: &str) -> IResult<&str, HtmlNode> {
    alt((html_plain_text, html_complete_tag))(input)
}

fn html_complete_tag(input: &str) -> IResult<&str, HtmlNode> {
    let (remaining, open) = html_open_tag(input)?;
    let (remaining, children) = many0(html_tag_content)(remaining)?;
    let (remaining, _close) = preceded(take_till(|c| c == '<'), html_close_tag(open))(remaining)?;

    let tag = HtmlTag {
        name: open,
        children,
    };

    Ok((remaining, HtmlNode::Tag(tag)))
}

fn main() {
    let file_content = fs::read_to_string("simple.html").expect("Can't read file 'example.html'");
    let (_remainder, result) = html_complete_tag(&file_content).unwrap();

    println!("{:#?}", result);
}

extern crate nom;
use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*,
    whitespace, IResult,
};

#[derive(Debug)]
pub struct HtmlTag<'a> {
    name: &'a str,
    children: Vec<HtmlNode<'a>>,
}

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug)]
pub struct HtmlPlain<'a> {
    plain: &'a str,
}

#[derive(Debug)]
pub enum HtmlNode<'a> {
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

pub fn parse(input: &str) -> Result<HtmlNode, nom::Err<(&str, nom::error::ErrorKind)>> {
    // TODO: implement better error handling - current errors don't give much information.
    let (_remainder, result) = html_complete_tag(&input)?;
    Ok(result)
}

extern crate nom;
use self::nom::error::{context, ParseError, VerboseError};
use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*,
    whitespace,
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

type IResult<'a, O> = nom::IResult<&'a str, O, VerboseError<&'a str>>;

fn html_open_tag(input: &str) -> IResult<&str> {
    // TODO: implement self closing tags.
    // TODO: implement tags that are forbidden to be closed like img, input, br, hr, meta, etc.

    let (input, _) = multispace0(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, open) = take_till1(|c| c == ' ' || c == '>' || c == '/' || c == '<')(input)?;
    let (input, _args) = many0(none_of("></"))(input)?;
    let (input, _) = tag(">")(input)?;

    Ok((input, open))
    /*
    delimited(
        multispace0,
        delimited(
            tag("<"),
            terminated(take_till1(|c| c == ' ' || c == '>'), many0(none_of("></"))),
            tag(">"),
        ),
        multispace0,
    )(input)
     */
}

fn html_close_tag<'a>(open_tag: &'a str) -> impl Fn(&'a str) -> IResult<&'a str> {
    context(
        "Unexpected closing tag that does not match opening tag!",
        delimited(
            multispace0,
            delimited(
                tag("</"),
                terminated(cut(tag(open_tag)), many0(none_of(">"))),
                tag(">"),
            ),
            multispace0,
        ),
    )
}

fn html_plain_text(input: &str) -> IResult<HtmlNode> {
    let (remaining, plain) = delimited(
        multispace0,
        take_till1(|c| c == '<' || c == '\r' || c == '\n'),
        multispace0,
    )(input)?;

    Ok((remaining, HtmlNode::Plain(HtmlPlain { plain })))
}

fn html_tag_content(input: &str) -> IResult<HtmlNode> {
    alt((html_plain_text, html_complete_tag))(input)
}

fn html_complete_tag(input: &str) -> IResult<HtmlNode> {
    let (remaining, open) = html_open_tag(input)?;
    let (remaining, children) = many0(html_tag_content)(remaining)?;
    let (remaining, _close) = preceded(take_till(|c| c == '<'), html_close_tag(open))(remaining)?;

    let tag = HtmlTag {
        name: open,
        children,
    };

    Ok((remaining, HtmlNode::Tag(tag)))
}

pub fn parse(input: &str) -> IResult<HtmlNode> {
    // TODO: implement better error handling - current errors don't give much information.
    html_complete_tag(&input)
    //let (_remainder, result) = html_complete_tag::<VerboseError<&str>>(&input)?;
    //Ok((_remainder, result))
}

#[cfg(test)]
mod tests {
    use super::nom::error::{ParseError, VerboseError};
    use super::nom::Err::Error;
    use crate::parser::html_open_tag;

    #[test]
    fn test_open_tag_postive() {
        assert_eq!(html_open_tag("<a href=\"#\">"), Ok(("", "a")));
        assert_eq!(html_open_tag("<p>"), Ok(("", "p")));
        assert_eq!(html_open_tag("<h1>"), Ok(("", "h1")));
        assert_eq!(html_open_tag("<h1>"), Ok(("", "h1")));
        assert_eq!(html_open_tag("<!DOCTYPE html>"), Ok(("", "!DOCTYPE")));
    }

    #[test]
    fn test_open_tag_negative() {
        assert_eq!(
            html_open_tag("<a href=\"#\" <p></p>"),
            Err(Error(VerboseError::from_error_kind(
                "<p></p>",
                nom::error::ErrorKind::Tag
            )))
        );
        assert_eq!(
            html_open_tag("</p>"),
            Err(Error(VerboseError::from_error_kind(
                "/p>",
                nom::error::ErrorKind::TakeTill1
            )))
        );
    }
}

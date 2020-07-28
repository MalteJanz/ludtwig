extern crate nom;
use self::nom::error::{context, ParseError, VerboseError};
use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*,
    whitespace,
};
use std::collections::HashMap;

static NON_CLOSING_TAGS: [&str; 6] = ["!DOCTYPE", "meta", "input", "img", "br", "hr"];

#[derive(Debug, Eq, PartialEq)]
pub struct HtmlTag<'a> {
    name: &'a str,
    self_closed: bool,
    arguments: HashMap<&'a str, &'a str>,
    children: Vec<HtmlNode<'a>>,
}

// Represents one line of plain text in the html document without line break characters or indentation.
#[derive(Debug, Eq, PartialEq)]
pub struct HtmlPlain<'a> {
    plain: &'a str,
}

#[derive(Debug, Eq, PartialEq)]
pub enum HtmlNode<'a> {
    Tag(HtmlTag<'a>),
    Plain(HtmlPlain<'a>),
}

type IResult<'a, O> = nom::IResult<&'a str, O, VerboseError<&'a str>>;

fn html_tag_argument<'a>(input: &'a str) -> IResult<(&'a str, &'a str)> {
    let (input, _) = multispace0(input)?;
    let (input, key) = alphanumeric1(input)?;
    let (input, equal) = opt(tag("=\""))(input)?;

    if equal == None {
        return Ok((input, (key, "")));
    }

    let (input, value) = take_till1(|c| c == '"')(input)?;
    let (input, _) = tag("\"")(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, (key, value)))
}

fn html_tag_argument_map<'a>(input: &'a str) -> IResult<HashMap<&'a str, &'a str>> {
    let (input, list) = many0(html_tag_argument)(input)?;
    let map = list.into_iter().collect::<HashMap<&str, &str>>();
    Ok((input, map))
}

// returns (tag, self_closed)
fn html_open_tag(input: &str) -> IResult<(&str, bool, HashMap<&str, &str>)> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, open) = take_till1(|c| c == ' ' || c == '>' || c == '/' || c == '<')(input)?;
    //let (input, _args) = many0(none_of("></"))(input)?;
    let (input, args) = html_tag_argument_map(input)?;

    let (input, mut closed) = alt((value(false, tag(">")), value(true, tag("/>"))))(input)?;

    if NON_CLOSING_TAGS.contains(&open) {
        closed = true;
    }

    Ok((input, (open, closed, args)))
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
    // TODO: also parse whitespace because it matters in rendering!: https://prettier.io/blog/2018/11/07/1.15.0.html
    let (mut remaining, (open, self_closed, args)) = html_open_tag(input)?;
    let mut children = vec![];

    if !self_closed {
        let (remaining_new, children_new) = many0(html_tag_content)(remaining)?;
        let (remaining_new, _close) =
            preceded(take_till(|c| c == '<'), html_close_tag(open))(remaining_new)?;
        remaining = remaining_new;
        children = children_new;
    }

    let tag = HtmlTag {
        name: open,
        self_closed,
        arguments: args,
        children,
    };

    Ok((remaining, HtmlNode::Tag(tag)))
}

pub fn parse(input: &str) -> IResult<HtmlNode> {
    let (remaining, children) = many1(html_complete_tag)(&input)?;

    Ok((
        remaining,
        HtmlNode::Tag(HtmlTag {
            name: "ROOT",
            self_closed: false,
            arguments: HashMap::new(),
            children,
        }),
    ))
}

#[cfg(test)]
mod tests {
    use super::nom::error::{ParseError, VerboseError};
    use super::nom::lib::std::collections::HashMap;
    use super::nom::Err::Error;
    use crate::parser::{
        html_complete_tag, html_open_tag, html_tag_argument, html_tag_argument_map, HtmlNode,
        HtmlTag,
    };

    #[test]
    fn test_open_tag_postive() {
        assert_eq!(
            html_open_tag("<a href=\"#\">"),
            Ok(("", ("a", false, vec![("href", "#")].into_iter().collect())))
        );
        assert_eq!(html_open_tag("<p>"), Ok(("", ("p", false, HashMap::new()))));
        assert_eq!(
            html_open_tag("<h1>"),
            Ok(("", ("h1", false, HashMap::new())))
        );
        assert_eq!(
            html_open_tag("<h1>"),
            Ok(("", ("h1", false, HashMap::new())))
        );
        assert_eq!(
            html_open_tag("<!DOCTYPE html>"),
            Ok((
                "",
                ("!DOCTYPE", true, vec![("html", "")].into_iter().collect())
            ))
        );
    }

    #[test]
    fn test_open_tag_negative() {
        assert_eq!(
            html_open_tag("<a href=\"#\" <p></p>"),
            Err(Error(VerboseError::append(
                "<p></p>",
                nom::error::ErrorKind::Alt,
                VerboseError::from_error_kind("<p></p>", nom::error::ErrorKind::Tag)
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

    #[test]
    fn test_open_self_closing_tag() {
        assert_eq!(
            html_open_tag("<br/>"),
            Ok(("", ("br", true, HashMap::new())))
        );
        assert_eq!(
            html_open_tag("<a href=\"#\"/>"),
            Ok(("", ("a", true, vec![("href", "#")].into_iter().collect())))
        )
    }

    #[test]
    fn test_open_non_closing_tag() {
        assert_eq!(
            html_open_tag("<meta charset=\"UTF-8\"><title>SomeTitle</title>"),
            Ok((
                "<title>SomeTitle</title>",
                (
                    "meta",
                    true,
                    vec![("charset", "UTF-8")].into_iter().collect()
                )
            ))
        );
    }

    #[test]
    fn test_complete_tag() {
        assert_eq!(
            html_complete_tag("<meta charset=\"UTF-8\"><title>SomeTitle</title>"),
            Ok((
                "<title>SomeTitle</title>",
                HtmlNode::Tag(HtmlTag {
                    name: "meta",
                    self_closed: true,
                    arguments: vec![("charset", "UTF-8")].into_iter().collect(),
                    children: vec![]
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<div><meta charset=\"UTF-8\"><title></title></div>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "div",
                    self_closed: false,
                    arguments: HashMap::new(),
                    children: vec![
                        HtmlNode::Tag(HtmlTag {
                            name: "meta",
                            self_closed: true,
                            arguments: vec![("charset", "UTF-8")].into_iter().collect(),
                            children: vec![]
                        }),
                        HtmlNode::Tag(HtmlTag {
                            name: "title",
                            self_closed: false,
                            arguments: HashMap::new(),
                            children: vec![]
                        })
                    ]
                })
            ))
        );
    }

    #[test]
    fn test_tag_argument() {
        assert_eq!(html_tag_argument("href=\"#\""), Ok(("", ("href", "#"))));
        assert_eq!(
            html_tag_argument("onClick=\"alert('Hello world');\" "),
            Ok(("", ("onClick", "alert('Hello world');")))
        );
        assert_eq!(html_tag_argument("disabled"), Ok(("", ("disabled", ""))));
    }

    #[test]
    fn test_tag_argument_map() {
        let mut map = HashMap::new();
        map.insert("href", "#");
        map.insert("target", "_blank");

        assert_eq!(
            html_tag_argument_map("href=\"#\" \n\t         target=\"_blank\"   "),
            Ok(("", map))
        );
    }
}

use super::IResult;
use crate::ast::*;
use crate::parser::general::{document_node, dynamic_context};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::{char, multispace0, none_of};
use nom::combinator::{cut, not, opt, peek, value};
use nom::error::context;
use nom::lib::std::collections::BTreeMap;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated};

static NON_CLOSING_TAGS: [&str; 6] = ["!DOCTYPE", "meta", "input", "img", "br", "hr"];

pub(crate) fn html_tag_argument<'a>(input: &'a str) -> IResult<(&'a str, &'a str)> {
    let (input, _) = multispace0(input)?;
    let (input, key) = take_till1(|c| {
        c == '='
            || c == ' '
            || c == '>'
            || c == '/'
            || c == '<'
            || c == '"'
            || c == '\n'
            || c == '\r'
            || c == '\t'
    })(input)?; //alphanumeric1(input)?;
    let (input, equal) = opt(tag("="))(input)?;

    if equal == None {
        let (input, _) = context("invalid attribute name", cut(peek(not(char('"')))))(input)?;

        return Ok((input, (key, "")));
    }

    let (input, _) = context("missing '\"' quote", cut(tag("\"")))(input)?;
    let (input, value) = take_till1(|c| c == '"')(input)?;
    let (input, _) = context("missing '\"' quote", cut(tag("\"")))(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, (key, value)))
}

pub(crate) fn html_tag_argument_map<'a>(input: &'a str) -> IResult<BTreeMap<&'a str, &'a str>> {
    let (input, list) = many0(html_tag_argument)(input)?;
    let map = list.into_iter().collect::<BTreeMap<&str, &str>>();
    Ok((input, map))
}

// returns (tag, self_closed)
pub(crate) fn html_open_tag(input: &str) -> IResult<(&str, bool, BTreeMap<&str, &str>)> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, open) = take_till1(|c| {
        c == ' ' || c == '>' || c == '/' || c == '<' || c == '\n' || c == '\r' || c == '\t'
    })(input)?;
    //let (input, _args) = many0(none_of("></"))(input)?;
    let (input, args) = html_tag_argument_map(input)?;

    let (input, mut closed) = alt((value(false, tag(">")), value(true, tag("/>"))))(input)?;

    if NON_CLOSING_TAGS.contains(&open) {
        closed = true;
    }

    Ok((input, (open, closed, args)))
}

pub(crate) fn html_close_tag<'a>(open_tag: &'a str) -> impl Fn(&'a str) -> IResult<&'a str> {
    delimited(
        multispace0,
        delimited(
            tag("</"),
            terminated(cut(tag(open_tag)), many0(none_of(">"))),
            tag(">"),
        ),
        multispace0,
    )
}

pub(crate) fn html_plain_text(input: &str) -> IResult<HtmlNode> {
    let (remaining, plain) = delimited(
        multispace0,
        take_till1(|c| c == '<' || c == '{' || c == '\t' || c == '\r' || c == '\n'),
        multispace0,
    )(input)?;

    Ok((remaining, HtmlNode::Plain(HtmlPlain { plain })))
}

pub(crate) fn html_complete_tag(input: &str) -> IResult<HtmlNode> {
    // TODO: also parser whitespace because it matters in rendering!: https://prettier.io/blog/2018/11/07/1.15.0.html
    let (mut remaining, (open, self_closed, args)) = html_open_tag(input)?;
    let mut children = vec![];

    if !self_closed {
        let (remaining_new, children_new) = many0(document_node)(remaining)?;
        let (remaining_new, _close) = preceded(
            multispace0, /*take_till(|c| c == '<')*/
            dynamic_context(
                format!(
                    "Missing closing tag for opening tag '{}' with arguments {:?}",
                    open, args
                ),
                cut(html_close_tag(open)),
            ),
        )(remaining_new)?;
        remaining = remaining_new;
        children = children_new;
    }

    let tag = HtmlTag {
        name: open,
        self_closed,
        attributes: args,
        children,
    };

    Ok((remaining, HtmlNode::Tag(tag)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::TwigParsingErrorInformation;
    use nom::error::ErrorKind;

    #[test]
    fn test_open_tag_positive() {
        assert_eq!(
            html_open_tag("<a href=\"#\">"),
            Ok(("", ("a", false, vec![("href", "#")].into_iter().collect())))
        );
        assert_eq!(
            html_open_tag("<p>"),
            Ok(("", ("p", false, BTreeMap::new())))
        );
        assert_eq!(
            html_open_tag("<h1>"),
            Ok(("", ("h1", false, BTreeMap::new())))
        );
        assert_eq!(
            html_open_tag("<h1>"),
            Ok(("", ("h1", false, BTreeMap::new())))
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
            Err(nom::Err::Error(TwigParsingErrorInformation {
                leftover: "<p></p>",
                context: None,
                kind: ErrorKind::Tag
            }))
        );

        assert_eq!(
            html_open_tag("</p>"),
            Err(nom::Err::Error(TwigParsingErrorInformation {
                leftover: "/p>",
                context: None,
                kind: ErrorKind::TakeTill1
            }))
        );
    }

    #[test]
    fn test_open_self_closing_tag() {
        assert_eq!(
            html_open_tag("<br/>"),
            Ok(("", ("br", true, BTreeMap::new())))
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
                    attributes: vec![("charset", "UTF-8")].into_iter().collect(),
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<div><meta charset=\"UTF-8\"><title></title></div>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "div",
                    children: vec![
                        HtmlNode::Tag(HtmlTag {
                            name: "meta",
                            self_closed: true,
                            attributes: vec![("charset", "UTF-8")].into_iter().collect(),
                            ..Default::default()
                        }),
                        HtmlNode::Tag(HtmlTag {
                            name: "title",
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
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
    fn test_tag_argument_with_missing_quote() {
        assert_eq!(
            html_tag_argument("href=#\""),
            Err(nom::Err::Failure(TwigParsingErrorInformation {
                leftover: "#\"",
                context: Some("missing '\"' quote".into()),
                kind: ErrorKind::Tag
            }))
        );
    }

    #[test]
    fn test_tag_argument_map_with_missing_quote() {
        assert_eq!(
            html_tag_argument_map("size=\"small @click=\"onCloseModal\""),
            Err(nom::Err::Failure(TwigParsingErrorInformation {
                leftover: "\"",
                context: Some("invalid attribute name".into()),
                kind: ErrorKind::Not
            }))
        );
    }

    #[test]
    fn test_tag_argument_map() {
        let mut map = BTreeMap::new();
        map.insert("href", "#");
        map.insert("target", "_blank");

        assert_eq!(
            html_tag_argument_map("href=\"#\" \n\t         target=\"_blank\"   "),
            Ok(("", map))
        );
    }
}

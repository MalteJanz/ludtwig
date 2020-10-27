use super::IResult;
use crate::ast::*;
use crate::parser::general::{document_node, dynamic_context};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_till1};
use nom::character::complete::{anychar, char, multispace0, none_of};
use nom::combinator::{cut, map, not, opt, peek, recognize, value};
use nom::error::context;
use nom::lib::std::collections::BTreeMap;
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, preceded, terminated};

static NON_CLOSING_TAGS: [&str; 7] = ["!DOCTYPE", "meta", "input", "img", "br", "hr", "source"];

pub(crate) fn html_tag_attribute<'a>(input: &'a str) -> IResult<(String, String)> {
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
    })(input)?;
    let (input, equal) = opt(tag("="))(input)?;

    if equal == None {
        let (input, _) = context("invalid attribute name", cut(peek(not(char('"')))))(input)?;

        return Ok((input, (key.to_owned(), "".to_string())));
    }

    let (input, _) = context("missing '\"' quote", cut(tag("\"")))(input)?;
    let (input, value) = take_till(|c| c == '"')(input)?;
    let (input, _) = context("missing '\"' quote", cut(tag("\"")))(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, (key.to_owned(), value.to_owned())))
}

pub(crate) fn html_tag_attribute_map(input: &str) -> IResult<BTreeMap<String, String>> {
    let (input, list) = many0(html_tag_attribute)(input)?;
    let map = list.into_iter().collect::<BTreeMap<String, String>>();
    Ok((input, map))
}

// returns (tag, self_closed, attributes)
pub(crate) fn html_open_tag(input: &str) -> IResult<(&str, bool, BTreeMap<String, String>)> {
    let (input, _) = tag("<")(input)?;
    let (input, open) = take_till1(|c| {
        c == ' ' || c == '>' || c == '/' || c == '<' || c == '\n' || c == '\r' || c == '\t'
    })(input)?;
    let (input, args) = html_tag_attribute_map(input)?;

    // get rid of whitespace between tag name and closing tag for tags without attributes.
    let (input, _) = multispace0(input)?;

    let (input, mut closed) = alt((value(false, tag(">")), value(true, tag("/>"))))(input)?;

    if NON_CLOSING_TAGS.contains(&open) {
        closed = true;
    }

    Ok((input, (open, closed, args)))
}

pub(crate) fn html_close_tag<'a>(open_tag: &'a str) -> impl Fn(&'a str) -> IResult<&'a str> {
    delimited(
        tag("</"),
        terminated(cut(tag(open_tag)), many0(none_of(">"))),
        tag(">"),
    )
}

pub(crate) fn html_complete_tag(input: &str) -> IResult<HtmlNode> {
    let (mut remaining, (open, self_closed, args)) = html_open_tag(input)?;
    let mut children = vec![];

    if !self_closed {
        let (remaining_new, children_new) = many0(document_node)(remaining)?;
        let (remaining_new, _close) = dynamic_context(
            format!(
                "Missing closing tag for opening tag '{}' with arguments {:?}",
                open, args
            ),
            cut(html_close_tag(open)),
        )(remaining_new)?;
        remaining = remaining_new;
        children = children_new;
    }

    let tag = HtmlTag {
        name: open.to_owned(),
        self_closed,
        attributes: args,
        children,
    };

    Ok((remaining, HtmlNode::Tag(tag)))
}

pub(crate) fn html_plain_text(input: &str) -> IResult<HtmlNode> {
    let (remaining, plain) = recognize(many1(preceded(
        opt(char(' ')),
        take_till1(|c| c == '<' || c == '{' || c == '\t' || c == '\r' || c == '\n' || c == ' '),
    )))(input)?;

    Ok((
        remaining,
        HtmlNode::Plain(HtmlPlain {
            plain: plain.to_owned(),
        }),
    ))
}

pub(crate) fn html_comment(input: &str) -> IResult<HtmlNode> {
    preceded(
        terminated(tag("<!--"), multispace0),
        map(
            many_till(anychar, preceded(multispace0, tag("-->"))),
            |(v, _)| {
                HtmlNode::Comment(HtmlComment {
                    content: v.into_iter().collect(),
                })
            },
        ),
    )(input)
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
            Ok((
                "",
                (
                    "a",
                    false,
                    vec![("href".to_string(), "#".to_string())]
                        .into_iter()
                        .collect()
                )
            ))
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
                (
                    "!DOCTYPE",
                    true,
                    vec![("html".to_string(), "".to_string())]
                        .into_iter()
                        .collect()
                )
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
            Ok((
                "",
                (
                    "a",
                    true,
                    vec![("href".to_string(), "#".to_string())]
                        .into_iter()
                        .collect()
                )
            ))
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
                    vec![("charset".to_string(), "UTF-8".to_string())]
                        .into_iter()
                        .collect()
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
                    name: "meta".to_string(),
                    self_closed: true,
                    attributes: vec![("charset".to_string(), "UTF-8".to_string())]
                        .into_iter()
                        .collect(),
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<div><meta charset=\"UTF-8\"><title></title></div>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "div".to_string(),
                    children: vec![
                        HtmlNode::Tag(HtmlTag {
                            name: "meta".to_string(),
                            self_closed: true,
                            attributes: vec![("charset".to_string(), "UTF-8".to_string())]
                                .into_iter()
                                .collect(),
                            ..Default::default()
                        }),
                        HtmlNode::Tag(HtmlTag {
                            name: "title".to_string(),
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
                })
            ))
        );
    }

    #[test]
    fn test_special_complete_tag() {
        assert_eq!(
            html_complete_tag("<br/>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "br".to_string(),
                    self_closed: true,
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<br />"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "br".to_string(),
                    self_closed: true,
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<br>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "br".to_string(),
                    self_closed: true,
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<br >"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "br".to_string(),
                    self_closed: true,
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            html_complete_tag("<source>"),
            Ok((
                "",
                HtmlNode::Tag(HtmlTag {
                    name: "source".to_string(),
                    self_closed: true,
                    ..Default::default()
                })
            ))
        );
    }

    #[test]
    fn test_tag_attribute() {
        assert_eq!(
            html_tag_attribute("href=\"#\""),
            Ok(("", ("href".to_string(), "#".to_string())))
        );
        assert_eq!(
            html_tag_attribute("onClick=\"alert('Hello world');\" "),
            Ok((
                "",
                ("onClick".to_string(), "alert('Hello world');".to_string())
            ))
        );
        assert_eq!(
            html_tag_attribute("disabled"),
            Ok(("", ("disabled".to_string(), "".to_string())))
        );
    }

    #[test]
    fn test_special_attributes() {
        assert_eq!(
            html_tag_attribute("title=\"\""),
            Ok(("", ("title".to_string(), "".to_string())))
        );
        assert_eq!(
            html_tag_attribute("#body"),
            Ok(("", ("#body".to_string(), "".to_string())))
        );
        assert_eq!(
            html_tag_attribute("@click=\"counter += 1;\""),
            Ok(("", ("@click".to_string(), "counter += 1;".to_string())))
        );
        assert_eq!(
            html_tag_attribute(":name=\"firstname\""),
            Ok(("", (":name".to_string(), "firstname".to_string())))
        );
        assert_eq!(
            html_tag_attribute("v-model=\"lastname\""),
            Ok(("", ("v-model".to_string(), "lastname".to_string())))
        );
        assert_eq!(
            html_tag_attribute("@change=\"if counter < 100 { counter += 1; }\""),
            Ok((
                "",
                (
                    "@change".to_string(),
                    "if counter < 100 { counter += 1; }".to_string()
                )
            ))
        );
    }

    #[test]
    fn test_tag_argument_with_missing_quote() {
        assert_eq!(
            html_tag_attribute("href=#\""),
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
            html_tag_attribute_map("size=\"small @click=\"onCloseModal\""),
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
        map.insert("href".to_string(), "#".to_string());
        map.insert("target".to_string(), "_blank".to_string());

        assert_eq!(
            html_tag_attribute_map("href=\"#\" \n\t         target=\"_blank\"   "),
            Ok(("", map))
        );
    }

    #[test]
    fn test_html_comment() {
        assert_eq!(
            html_comment("<!-- not full implemented yet -->"),
            Ok((
                "",
                HtmlNode::Comment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );

        assert_eq!(
            html_comment("<!--              not full implemented yet                         -->"),
            Ok((
                "",
                HtmlNode::Comment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );

        assert_eq!(
            html_comment("<!--not full implemented yet-->"),
            Ok((
                "",
                HtmlNode::Comment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );
    }
}

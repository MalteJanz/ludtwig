use super::IResult;
use crate::ast::*;
use crate::parser::general::{
    document_node, dynamic_context, DynamicChildParser, GenericChildParser, Input,
};
use crate::parser::twig::{twig_comment, twig_structure};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1, take_until};
use nom::character::complete::{anychar, char, multispace0, none_of};
use nom::combinator::{cut, map, map_opt, not, opt, peek, recognize, value};
use nom::error::context;
use nom::multi::{many0, many1, many_till};
use nom::sequence::{delimited, preceded, terminated, tuple};

static NON_CLOSING_TAGS: [&str; 7] = ["!DOCTYPE", "meta", "input", "img", "br", "hr", "source"];

pub(crate) fn html_tag_html_attribute(input: Input) -> IResult<TagAttribute> {
    let (input, key) = alt((
        take_till1(|c| {
            c == '='
                || c == ' '
                || c == '>'
                || c == '/'
                || c == '<'
                || c == '"'
                || c == '{' // skip attribute names that start with '{' because the syntax is reserved for twig.
                || c == '\n'
                || c == '\r'
                || c == '\t'
        }),
        // allow the key to be an expression block but match the consumed input with {{ ... }}
        // this is a simplified version of the [expression_block] parser to increase performance
        recognize(tuple((tag("{{"), take_until("}}"), tag("}}")))),
    ))(input)?;
    let (input, _) = multispace0(input)?; // allow whitespace between equals
    let (input, equal) = opt(char('='))(input)?;
    let (input, _) = multispace0(input)?; // allow whitespace between equals

    if equal == None {
        let (input, _) = context("invalid attribute name", cut(peek(not(char('"')))))(input)?;

        return Ok((
            input,
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: key.to_owned(),
                value: None,
            }),
        ));
    }

    let (input, _) = context("missing '\"' quote", cut(char('"')))(input)?;

    // a value can contain anything until it ends with a quote "
    // to allow nested quotes in twig expressions it tries to parse syntax blocks when it runs into a '{'
    // syntax blocks can contain anything in them (even quotes ")
    // if it is not a twig syntax block it can also be a { } block from javascript
    let (input, value) = recognize(many_till(
        alt((
            recognize(take_till1(|c| c == '"' || c == '{')),
            recognize(tuple((tag("{{"), take_until("}}"), tag("}}")))),
            recognize(tuple((tag("{%"), take_until("%}"), tag("%}")))),
            recognize(tuple((tag("{#"), take_until("#}"), tag("#}")))),
            recognize(tuple((char('{'), take_till1(|c| c == '}'), char('}')))), // allow any javascript block { syntax }
        )),
        peek(char('"')),
    ))(input)?;

    let (input, _) = context("missing '\"' quote", cut(char('"')))(input)?;

    Ok((
        input,
        TagAttribute::HtmlAttribute(HtmlAttribute {
            name: key.to_owned(),
            value: Some(value.to_owned()),
        }),
    ))
}

pub(crate) fn html_tag_attribute(input: Input) -> IResult<TagAttribute> {
    alt((
        map_opt(twig_comment, |node| {
            if let SyntaxNode::TwigComment(comment) = node {
                return Some(TagAttribute::TwigComment(comment));
            }

            None
        }),
        map_opt(
            twig_structure::<TagAttribute, DynamicChildParser>,
            |twig_structure| Some(TagAttribute::TwigStructure(twig_structure)),
        ),
        html_tag_html_attribute,
    ))(input)
}

pub(crate) fn html_tag_attribute_map(input: Input) -> IResult<Vec<TagAttribute>> {
    DynamicChildParser::generic_parse_children(input)
}

// returns (tag, self_closed, attributes)
pub(crate) fn html_open_tag(input: Input) -> IResult<(Input, bool, Vec<TagAttribute>)> {
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

pub(crate) fn html_close_tag<'a>(
    open_tag: &'a str,
) -> impl FnMut(Input<'a>) -> IResult<Input<'a>> + 'a {
    delimited(
        tag("</"),
        terminated(cut(tag(open_tag)), many0(none_of(">"))),
        tag(">"),
    )
}

pub(crate) fn html_complete_tag(input: Input) -> IResult<SyntaxNode> {
    let (mut remaining, (open, self_closed, attributes)) = html_open_tag(input)?;
    let mut children = vec![];

    if !self_closed {
        let (remaining_new, children_new) = many0(document_node)(remaining)?;
        let (remaining_new, _close) = dynamic_context(
            || {
                format!(
                    "Missing closing tag for opening tag '{}' with attributes [{} ]",
                    open,
                    attributes
                        .iter()
                        .fold(String::new(), |acc, att| format!("{} {}", acc, att))
                )
            },
            cut(html_close_tag(open)),
        )(remaining_new)?;
        remaining = remaining_new;
        children = children_new;
    }

    let tag = Tag {
        name: open.to_owned(),
        self_closed,
        attributes,
        children,
    };

    Ok((remaining, SyntaxNode::Tag(tag)))
}

pub(crate) fn html_plain_text(input: Input) -> IResult<SyntaxNode> {
    let (remaining, plain) = recognize(many1(preceded(
        opt(char(' ')),
        take_till1(|c| c == '<' || c == '{' || c == '\t' || c == '\r' || c == '\n' || c == ' '),
    )))(input)?;

    Ok((
        remaining,
        SyntaxNode::Plain(Plain {
            plain: plain.to_owned(),
        }),
    ))
}

pub(crate) fn html_comment(input: Input) -> IResult<SyntaxNode> {
    preceded(
        terminated(tag("<!--"), multispace0),
        map(
            many_till(anychar, preceded(multispace0, tag("-->"))),
            |(v, _)| {
                SyntaxNode::HtmlComment(HtmlComment {
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

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    #[test]
    fn test_open_tag_positive() {
        assert_eq!(
            html_open_tag("<a href=\"#\">"),
            Ok((
                "",
                (
                    "a",
                    false,
                    vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "href".to_string(),
                        value: Some("#".to_string())
                    })]
                )
            ))
        );
        assert_eq!(html_open_tag("<p>"), Ok(("", ("p", false, Vec::new()))));
        assert_eq!(html_open_tag("<h1>"), Ok(("", ("h1", false, Vec::new()))));
        assert_eq!(html_open_tag("<h1>"), Ok(("", ("h1", false, Vec::new()))));
        assert_eq!(
            html_open_tag("<!DOCTYPE html>"),
            Ok((
                "",
                (
                    "!DOCTYPE",
                    true,
                    vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "html".to_string(),
                        value: None
                    })]
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
        assert_eq!(html_open_tag("<br/>"), Ok(("", ("br", true, Vec::new()))));
        assert_eq!(
            html_open_tag("<a href=\"#\"/>"),
            Ok((
                "",
                (
                    "a",
                    true,
                    vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "href".to_string(),
                        value: Some("#".to_string())
                    })]
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
                    vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "charset".to_string(),
                        value: Some("UTF-8".to_string())
                    })]
                )
            ))
        );
    }

    #[test]
    fn test_complete_tag() {
        let meta_tag = SyntaxNode::Tag(Tag {
            name: "meta".to_string(),
            self_closed: true,
            attributes: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "charset".to_string(),
                value: Some("UTF-8".to_string()),
            })],
            ..Default::default()
        });

        assert_eq!(
            html_complete_tag("<meta charset=\"UTF-8\"><title>SomeTitle</title>"),
            Ok(("<title>SomeTitle</title>", meta_tag.clone()))
        );

        assert_eq!(
            html_complete_tag("<div><meta charset=\"UTF-8\"><title></title></div>"),
            Ok((
                "",
                SyntaxNode::Tag(Tag {
                    name: "div".to_string(),
                    children: vec![
                        meta_tag,
                        SyntaxNode::Tag(Tag {
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
                SyntaxNode::Tag(Tag {
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
                SyntaxNode::Tag(Tag {
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
                SyntaxNode::Tag(Tag {
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
                SyntaxNode::Tag(Tag {
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
                SyntaxNode::Tag(Tag {
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
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "href".to_string(),
                    value: Some("#".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("onClick=\"alert('Hello world');\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "onClick".to_string(),
                    value: Some("alert('Hello world');".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("disabled"),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "disabled".to_string(),
                    value: None
                })
            ))
        );
    }

    #[test]
    fn test_special_attributes() {
        assert_eq!(
            html_tag_attribute("title=\"\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "title".to_string(),
                    value: Some("".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("#body"),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "#body".to_string(),
                    value: None
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("@click=\"counter += 1;\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "@click".to_string(),
                    value: Some("counter += 1;".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute(":name=\"firstname\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: ":name".to_string(),
                    value: Some("firstname".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("v-model=\"lastname\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "v-model".to_string(),
                    value: Some("lastname".to_string())
                })
            ))
        );
        assert_eq!(
            html_tag_attribute("@change=\"if counter < 100 { counter += 1; }\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "@change".to_string(),
                    value: Some("if counter < 100 { counter += 1; }".to_string())
                })
            ))
        );
    }

    #[test]
    fn test_tag_argument_with_spaces_between_equals() {
        assert_eq!(
            html_tag_attribute("class = \"ok\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("ok".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_attribute("class= \"ok\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("ok".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_attribute("class =\"ok\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("ok".to_string())
                })
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
                kind: ErrorKind::Not
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
        let attributes = vec![
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "href".to_string(),
                value: Some("#".to_string()),
            }),
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "target".to_string(),
                value: Some("_blank".to_string()),
            }),
        ];

        assert_eq!(
            html_tag_attribute_map("href=\"#\" \n\t         target=\"_blank\"   "),
            Ok(("", attributes))
        );
    }

    #[test]
    fn test_html_comment() {
        assert_eq!(
            html_comment("<!-- not full implemented yet -->"),
            Ok((
                "",
                SyntaxNode::HtmlComment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );

        assert_eq!(
            html_comment("<!--              not full implemented yet                         -->"),
            Ok((
                "",
                SyntaxNode::HtmlComment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );

        assert_eq!(
            html_comment("<!--not full implemented yet-->"),
            Ok((
                "",
                SyntaxNode::HtmlComment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );
    }

    #[test]
    fn test_html_comment_against_tag() {
        assert_eq!(
            document_node("<!-- not full implemented yet -->"),
            Ok((
                "",
                SyntaxNode::HtmlComment(HtmlComment {
                    content: "not full implemented yet".to_string()
                })
            ))
        );

        assert_eq!(
            document_node("<!DOCTYPE html>"),
            Ok((
                "",
                SyntaxNode::Tag(Tag {
                    name: "!DOCTYPE".to_string(),
                    self_closed: true,
                    attributes: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "html".to_string(),
                        value: None
                    })],
                    ..Default::default()
                })
            ))
        );

        assert_eq!(
            document_node("<#special></#special>"),
            Ok((
                "",
                SyntaxNode::Tag(Tag {
                    name: "#special".to_string(),
                    ..Default::default()
                })
            ))
        );
    }

    #[test]
    fn test_html_attributes_with_twig_comment() {
        let attributes = vec![
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "href".to_string(),
                value: Some("#".to_string()),
            }),
            TagAttribute::TwigComment(TwigComment {
                content: "this is a twig comment".to_string(),
            }),
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "target".to_string(),
                value: Some("_blank".to_string()),
            }),
        ];

        assert_eq!(
            html_tag_attribute_map(
                "href=\"#\" \n\t    {# this is a twig comment #}     target=\"_blank\"   "
            ),
            Ok(("", attributes))
        );
    }

    #[test]
    fn test_valid_twig_syntax_for_html_attributes() {
        assert_eq!(
            twig_structure::<TagAttribute, DynamicChildParser>(
                "{% block hello %}{# this is a twig comment #}{% endblock %}",
            ),
            Ok((
                "",
                TwigStructure::TwigBlock(TwigBlock {
                    name: "hello".to_string(),
                    children: vec![TagAttribute::TwigComment(TwigComment {
                        content: "this is a twig comment".to_string()
                    })]
                })
            ))
        );

        assert_eq!(
            twig_structure::<TagAttribute, DynamicChildParser>(
                "{% block hello %}     href=\"#\" {% endblock %}",
            ),
            Ok((
                "",
                TwigStructure::TwigBlock(TwigBlock {
                    name: "hello".to_string(),
                    children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "href".to_string(),
                        value: Some("#".to_string())
                    })]
                })
            ))
        );
    }

    #[test]
    fn test_html_attributes_with_twig_if() {
        let attributes = vec![
            TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "href".to_string(),
                value: Some("#".to_string()),
            }),
            TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                if_arms: vec![TwigIfArm {
                    expression: Some("differentBrowserTab == true".to_string()),
                    children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "target".to_string(),
                        value: Some("_blank".to_string()),
                    })],
                }],
            })),
        ];

        assert_eq!(
            html_tag_attribute_map("href=\"#\" \n\t   {% if differentBrowserTab == true  %}  target=\"_blank\"  {% endif %}   "),
            Ok(("", attributes))
        );
    }

    #[test]
    fn test_html_attributes_with_nested_quotes() {
        assert_eq!(
            html_tag_html_attribute(
                "alt=\"{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}\""
            ),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some(
                        "{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}".to_string()
                    )
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute(
                "alt=\"{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}-asdf\""
            ),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some(
                        "{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}-asdf"
                            .to_string()
                    )
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute(
                "alt=\"asdf-{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}\""
            ),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some(
                        "asdf-{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}"
                            .to_string()
                    )
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute(
                "alt=\"hello world asdf-{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}-asdf\""
            ),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some(
                        "hello world asdf-{{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}-asdf".to_string()
                    )
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute("alt=\"hello {% if w == \"world\" %} world {% endif %}\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some("hello {% if w == \"world\" %} world {% endif %}".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute("alt=\"hello-{{ \"world\"|trans }} {{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "alt".to_string(),
                    value: Some("hello-{{ \"world\"|trans }} {{ \"wishlist.wishlistEmptyDescription\"|trans|striptags }}".to_string())
                })
            ))
        );
    }

    #[test]
    fn test_html_attributes_with_output_expressions() {
        assert_eq!(
            html_tag_html_attribute("class=\"{{ world }}\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("{{ world }}".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute("{{ custom }}=\"yes\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "{{ custom }}".to_string(),
                    value: Some("yes".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute("{{ custom }}=\"{{ value }}\""),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "{{ custom }}".to_string(),
                    value: Some("{{ value }}".to_string())
                })
            ))
        );

        assert_eq!(
            html_tag_html_attribute("{{ completelyCustom }}"),
            Ok((
                "",
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "{{ completelyCustom }}".to_string(),
                    value: None
                })
            ))
        );
    }
}

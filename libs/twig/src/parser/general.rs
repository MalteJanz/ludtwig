use crate::ast::*;
use crate::error::{DynamicParseError, TwigParsingErrorInformation};
use crate::parser::expression::expression_block;
use crate::parser::html::{html_comment, html_complete_tag, html_plain_text, html_tag_attribute};
use crate::parser::twig::{twig_comment, twig_syntax};
use nom::branch::alt;
use nom::character::complete::{multispace0, multispace1};
use nom::multi::{many0, many1};
use nom::sequence::delimited;
use nom::Parser;

pub(crate) type Input<'a> = &'a str;
pub(crate) type IResult<'a, O> = nom::IResult<Input<'a>, O, TwigParsingErrorInformation<Input<'a>>>;

/// This is used mainly as the [dynamic_context] combinator, to add user friendly information
/// to errors when backtracking through a parse tree.
/// It works by executing the callback function context_creator to get a dynamic String,
/// which is then stored in the error as context
/// (this execution only happens in the case of parsing errors).
pub(crate) fn dynamic_context<I: Clone, C, E: DynamicParseError<I>, F, O>(
    context_creator: C,
    mut f: F,
) -> impl FnMut(I) -> nom::IResult<I, O, E>
where
    C: Fn() -> String,
    F: Parser<I, O, E>,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(E::add_dynamic_context(
            i,
            context_creator(),
            e,
        ))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(E::add_dynamic_context(
            i,
            context_creator(),
            e,
        ))),
    }
}

/// Trait that allows to parse a generic list of children.
pub(crate) trait GenericChildParser<T> {
    fn generic_parse_children(input: Input) -> IResult<Vec<T>>;
}

/// Struct that implements the GenericChildParser Trait for different children types.
pub(crate) struct DynamicChildParser();

/// In case of [SyntaxNode] the [document_node] parser is used.
impl GenericChildParser<SyntaxNode> for DynamicChildParser {
    fn generic_parse_children(input: Input) -> IResult<Vec<SyntaxNode>> {
        many0(document_node)(input)
    }
}

/// In case of [TagAttribute] the [html_tag_attribute] parser is used.
impl GenericChildParser<TagAttribute> for DynamicChildParser {
    fn generic_parse_children(input: Input) -> IResult<Vec<TagAttribute>> {
        many0(delimited(multispace0, html_tag_attribute, multispace0))(input)
    }
}

/// whitespace because it matters in rendering!: https://prettier.io/blog/2018/11/07/1.15.0.html
pub(crate) fn some_whitespace(input: Input) -> IResult<SyntaxNode> {
    let (remainder, _) = multispace1(input)?;

    Ok((remainder, SyntaxNode::Whitespace))
}

pub(crate) fn document_node(input: Input) -> IResult<SyntaxNode> {
    alt((
        some_whitespace,
        html_comment, //html comment must match before html tag because both can start with <!...
        html_complete_tag,
        expression_block,
        twig_syntax,
        twig_comment,
        html_plain_text,
    ))(input)
}

pub(crate) fn document_node_all(input: Input) -> IResult<SyntaxNode> {
    let (remaining, children) = many1(document_node)(&input)?;

    Ok((remaining, SyntaxNode::Root(children)))
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    #[test]
    fn test_some_vue_template() {
        let res = document_node(
            "<sw-button-group
                v-if=\"startButtonVisible\"
                :splitButton=\"true\">

            <sw-button variant=\"primary\"
                       :disabled=\"startButtonDisabled\"
                       @click=\"onStartButtonClick\">
                {{ $tc('swag-migration.index.confirmAbortDialog.hint') }}
            </sw-button>

            <sw-context-button :disabled=\"isLoading\">
                <template slot=\"button\">

                    <sw-button square
                               variant=\"primary\"
                               :disabled=\"isLoading\">
                        <sw-icon name=\"small-arrow-medium-down\" size=\"16\"></sw-icon>
                    </sw-button>
                </template>

                <sw-context-menu-item @click=\"onSaveButtonClick\"
                                      :disabled=\"isLoading\">
                    {{ $tc('swag-migration.index.confirmAbortDialog.hint') }}
                </sw-context-menu-item>
            </sw-context-button>

        </sw-button-group>",
        );

        assert_eq!(res, Ok(
            (
                "",
                SyntaxNode::Tag(
                    Tag {
                        name: "sw-button-group".to_string(),
                        self_closed: false,
                        attributes: vec![
                            TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "v-if".to_string(),
                                value: Some(
                                    "startButtonVisible".to_string(),
                                ),
                            }),
                            TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: ":splitButton".to_string(),
                                value: Some(
                                    "true".to_string(),
                                ),
                            }),
                        ],
                        children: vec![
                            SyntaxNode::Whitespace,
                            SyntaxNode::Tag(
                                Tag {
                                    name: "sw-button".to_string(),
                                    self_closed: false,
                                    attributes: vec![
                                        TagAttribute::HtmlAttribute(HtmlAttribute {
                                            name: "variant".to_string(),
                                            value: Some(
                                                "primary".to_string(),
                                            ),
                                        }),
                                        TagAttribute::HtmlAttribute(HtmlAttribute {
                                            name: ":disabled".to_string(),
                                            value: Some(
                                                "startButtonDisabled".to_string(),
                                            ),
                                        }),
                                        TagAttribute::HtmlAttribute(HtmlAttribute {
                                            name: "@click".to_string(),
                                            value: Some(
                                                "onStartButtonClick".to_string(),
                                            ),
                                        }),
                                    ],
                                    children: vec![
                                        SyntaxNode::Whitespace,
                                        SyntaxNode::OutputExpression(
                                            OutputExpression {
                                                content: "$tc(\'swag-migration.index.confirmAbortDialog.hint\')".to_string(),
                                            },
                                        ),
                                        SyntaxNode::Whitespace,
                                    ],
                                },
                            ),
                            SyntaxNode::Whitespace,
                            SyntaxNode::Tag(
                                Tag {
                                    name: "sw-context-button".to_string(),
                                    self_closed: false,
                                    attributes: vec![
                                        TagAttribute::HtmlAttribute(HtmlAttribute {
                                            name: ":disabled".to_string(),
                                            value: Some(
                                                "isLoading".to_string(),
                                            ),
                                        }),
                                    ],
                                    children: vec![
                                        SyntaxNode::Whitespace,
                                        SyntaxNode::Tag(
                                            Tag {
                                                name: "template".to_string(),
                                                self_closed: false,
                                                attributes: vec![
                                                    TagAttribute::HtmlAttribute(HtmlAttribute {
                                                        name: "slot".to_string(),
                                                        value: Some(
                                                            "button".to_string(),
                                                        ),
                                                    }),
                                                ],
                                                children: vec![
                                                    SyntaxNode::Whitespace,
                                                    SyntaxNode::Tag(
                                                        Tag {
                                                            name: "sw-button".to_string(),
                                                            self_closed: false,
                                                            attributes: vec![
                                                                TagAttribute::HtmlAttribute(HtmlAttribute {
                                                                    name: "square".to_string(),
                                                                    value: None,
                                                                }),
                                                                TagAttribute::HtmlAttribute(HtmlAttribute {
                                                                    name: "variant".to_string(),
                                                                    value: Some(
                                                                        "primary".to_string(),
                                                                    ),
                                                                }),
                                                                TagAttribute::HtmlAttribute(HtmlAttribute {
                                                                    name: ":disabled".to_string(),
                                                                    value: Some(
                                                                        "isLoading".to_string(),
                                                                    ),
                                                                }),
                                                            ],
                                                            children: vec![
                                                                SyntaxNode::Whitespace,
                                                                SyntaxNode::Tag(
                                                                    Tag {
                                                                        name: "sw-icon".to_string(),
                                                                        self_closed: false,
                                                                        attributes: vec![
                                                                            TagAttribute::HtmlAttribute(HtmlAttribute {
                                                                                name: "name".to_string(),
                                                                                value: Some(
                                                                                    "small-arrow-medium-down".to_string(),
                                                                                ),
                                                                            }),
                                                                            TagAttribute::HtmlAttribute(HtmlAttribute {
                                                                                name: "size".to_string(),
                                                                                value: Some(
                                                                                    "16".to_string(),
                                                                                ),
                                                                            }),
                                                                        ],
                                                                        children: vec![],
                                                                    },
                                                                ),
                                                                SyntaxNode::Whitespace,
                                                            ],
                                                        },
                                                    ),
                                                    SyntaxNode::Whitespace,
                                                ],
                                            },
                                        ),
                                        SyntaxNode::Whitespace,
                                        SyntaxNode::Tag(
                                            Tag {
                                                name: "sw-context-menu-item".to_string(),
                                                self_closed: false,
                                                attributes: vec![
                                                    TagAttribute::HtmlAttribute(HtmlAttribute {
                                                        name: "@click".to_string(),
                                                        value: Some(
                                                            "onSaveButtonClick".to_string(),
                                                        ),
                                                    }),
                                                    TagAttribute::HtmlAttribute(HtmlAttribute {
                                                        name: ":disabled".to_string(),
                                                        value: Some(
                                                            "isLoading".to_string(),
                                                        ),
                                                    }),
                                                ],
                                                children: vec![
                                                    SyntaxNode::Whitespace,
                                                    SyntaxNode::OutputExpression(
                                                        OutputExpression {
                                                            content: "$tc(\'swag-migration.index.confirmAbortDialog.hint\')".to_string(),
                                                        },
                                                    ),
                                                    SyntaxNode::Whitespace,
                                                ],
                                            },
                                        ),
                                        SyntaxNode::Whitespace,
                                    ],
                                },
                            ),
                            SyntaxNode::Whitespace,
                        ],
                    },
                ),
            ),
        ));
    }

    #[test]
    fn test_whitespace_between_nodes() {
        let res = document_node_all(
            "<h2>
    HelloWorld<span>asdf</span>
</h2>",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::Root(vec![SyntaxNode::Tag(Tag {
                    name: "h2".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "HelloWorld".to_string()
                        }),
                        SyntaxNode::Tag(Tag {
                            name: "span".to_string(),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "asdf".to_string()
                            })],
                            ..Default::default()
                        }),
                        SyntaxNode::Whitespace
                    ],
                    ..Default::default()
                })])
            ))
        );
    }

    #[test]
    fn test_no_whitespace_between_nodes() {
        let res = document_node_all("<h2>HelloWorld<span>asdf</span></h2>");

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::Root(vec![SyntaxNode::Tag(Tag {
                    name: "h2".to_string(),
                    children: vec![
                        SyntaxNode::Plain(Plain {
                            plain: "HelloWorld".to_string()
                        }),
                        SyntaxNode::Tag(Tag {
                            name: "span".to_string(),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "asdf".to_string()
                            })],
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
                })])
            ))
        );
    }

    #[test]
    fn test_whitespace_with_plain_text() {
        let res = document_node_all(
            "<h2>
    Hello World
    this is <strong>some</strong> text
    <span>!!!</span>
</h2>",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::Root(vec![SyntaxNode::Tag(Tag {
                    name: "h2".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "Hello World".to_string()
                        }),
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "this is".to_string()
                        }),
                        SyntaxNode::Whitespace,
                        SyntaxNode::Tag(Tag {
                            name: "strong".to_string(),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "some".to_string()
                            })],
                            ..Default::default()
                        }),
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "text".to_string()
                        }),
                        SyntaxNode::Whitespace,
                        SyntaxNode::Tag(Tag {
                            name: "span".to_string(),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "!!!".to_string()
                            })],
                            ..Default::default()
                        }),
                        SyntaxNode::Whitespace
                    ],
                    ..Default::default()
                })])
            ))
        );
    }
}

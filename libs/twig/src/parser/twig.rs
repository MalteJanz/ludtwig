use super::IResult;
use crate::ast::*;
use crate::error::DynamicParseError;
use crate::error::TwigParsingErrorInformation;
use crate::parser::general::{dynamic_context, DynamicChildParser, GenericChildParser, Input};
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::{alpha1, anychar, multispace0};
use nom::combinator::{cut, map, verify};
use nom::error::ErrorKind;
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, terminated};

/// matches any twig comment {# ... #}
pub(crate) fn twig_comment(input: Input) -> IResult<SyntaxNode> {
    preceded(
        terminated(tag("{#"), multispace0),
        map(
            many_till(anychar, preceded(multispace0, tag("#}"))),
            |(v, _)| {
                SyntaxNode::TwigComment(TwigComment {
                    content: v.into_iter().collect(),
                })
            },
        ),
    )(input)
}

/// Matches any {% ... %} syntax and decides what AST structure to use.
pub(crate) fn twig_syntax(input: Input) -> IResult<SyntaxNode> {
    let (remaining, _) = terminated(tag("{%"), multispace0)(input)?;
    let (remaining, keyword) = alpha1(remaining)?;

    return match keyword {
        "block" => preceded(
            multispace0,
            map(twig_complete_block::<SyntaxNode, DynamicChildParser>, |i| {
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(i))
            }),
        )(remaining),
        // ignore these because they are indicators for hierarchical syntax
        "if" => preceded(
            multispace0,
            map(twig_if_block::<SyntaxNode, DynamicChildParser>, |i| {
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(i))
            }),
        )(remaining),
        "for" => preceded(
            multispace0,
            map(twig_for_block::<SyntaxNode, DynamicChildParser>, |i| {
                SyntaxNode::TwigStructure(TwigStructure::TwigFor(i))
            }),
        )(remaining),
        "apply" => preceded(
            multispace0,
            map(twig_apply_block::<SyntaxNode, DynamicChildParser>, |i| {
                SyntaxNode::TwigStructure(TwigStructure::TwigApply(i))
            }),
        )(remaining),
        // every hierarchical closing block should not be parsed here
        "endblock" | "elseif" | "else" | "endif" | "endfor" | "endapply" | "endset" => {
            Err(nom::Err::Error(TwigParsingErrorInformation {
                leftover: remaining,
                context: None,
                kind: ErrorKind::Tag,
            }))
        }
        "set" => {
            let set_capture_result = preceded(
                multispace0,
                map(
                    twig_set_capture_block::<SyntaxNode, DynamicChildParser>,
                    |i| SyntaxNode::TwigStructure(TwigStructure::TwigSetCapture(i)),
                ),
            )(remaining);

            return if let Ok((remaining, set_capture)) = set_capture_result {
                Ok((remaining, set_capture))
            } else {
                twig_statement(keyword, remaining)
                    .map(|(remaining, statement)| (remaining, SyntaxNode::TwigStatement(statement)))
            };
        }
        // everything else can be a [TwigStatement::Raw] for now
        _ => twig_statement(keyword, remaining)
            .map(|(remaining, statement)| (remaining, SyntaxNode::TwigStatement(statement))),
    };
}

/// Matches any {% ... %} syntax THAT HAS CHILDREN and decides what TwigStructure to use.
/// TODO: try to minify code duplication with the [twig_syntax] function.
pub(crate) fn twig_structure<R, T: GenericChildParser<R>>(
    input: Input,
) -> IResult<TwigStructure<R>> {
    let (remaining, _) = terminated(tag("{%"), multispace0)(input)?;
    let (remaining, keyword) = alpha1(remaining)?;

    match keyword {
        "block" => preceded(
            multispace0,
            map(twig_complete_block::<R, T>, TwigStructure::TwigBlock),
        )(remaining),
        // ignore these because they are indicators for hierarchical syntax
        "if" => preceded(
            multispace0,
            map(twig_if_block::<R, T>, TwigStructure::TwigIf),
        )(remaining),
        "for" => preceded(
            multispace0,
            map(twig_for_block::<R, T>, TwigStructure::TwigFor),
        )(remaining),
        "apply" => preceded(
            multispace0,
            map(twig_apply_block::<R, T>, TwigStructure::TwigApply),
        )(remaining),
        // every hierarchical closing block should not be parsed here
        "endblock" | "elseif" | "else" | "endif" | "endfor" | "endapply" | "endset" => {
            Err(nom::Err::Error(TwigParsingErrorInformation {
                leftover: remaining,
                context: None,
                kind: ErrorKind::Tag,
            }))
        }
        "set" => {
            let set_capture_result = preceded(
                multispace0,
                map(twig_set_capture_block::<R, T>, |i| {
                    TwigStructure::TwigSetCapture(i)
                }),
            )(remaining);

            if let Ok((remaining, set_capture)) = set_capture_result {
                Ok((remaining, set_capture))
            } else {
                Err(nom::Err::Error(TwigParsingErrorInformation {
                    leftover: remaining,
                    context: None,
                    kind: ErrorKind::Tag,
                }))
            }
        }
        _ => Err(nom::Err::Error(TwigParsingErrorInformation {
            leftover: remaining,
            context: None,
            kind: ErrorKind::Tag,
        })),
    }
}

pub(crate) fn twig_single_word_opening_block(input: Input) -> IResult<Input> {
    terminated(
        terminated(
            take_till1(|c| char::is_whitespace(c) || c == '%'),
            multispace0,
        ),
        tag("%}"),
    )(input)
}

pub(crate) fn twig_expression_opening_block(input: Input) -> IResult<String> {
    let (remaining, m) = verify(
        map(
            many_till(anychar, preceded(multispace0, tag("%}"))),
            |(v, _)| v.iter().collect::<String>(),
        ),
        |s: &str| !s.is_empty(),
    )(input)?;

    Ok((remaining, m))
}

pub(crate) fn twig_closing_structure<'a>(
    name: &'a str,
) -> impl FnMut(Input<'a>) -> IResult<Input<'a>> + 'a {
    delimited(
        tag("{%"),
        delimited(multispace0, tag(name), multispace0),
        tag("%}"),
    )
}

pub(crate) fn twig_complete_block<R, T: GenericChildParser<R>>(
    input: Input,
) -> IResult<TwigBlock<R>> {
    let (remaining, open) = twig_single_word_opening_block(input)?;
    let (remaining, children) = T::generic_parse_children(remaining)?;

    let (remaining, _close) = dynamic_context(
        || format!("Missing endblock for '{}' twig block", open),
        cut(twig_closing_structure("endblock")),
    )(remaining)?;

    Ok((
        remaining,
        TwigBlock {
            name: open.to_owned(),
            children,
        },
    ))
}

pub(crate) fn twig_for_block<R, T: GenericChildParser<R>>(input: Input) -> IResult<TwigFor<R>> {
    let (remaining, expression) = twig_expression_opening_block(input)?;
    let (remaining, children) = T::generic_parse_children(remaining)?;

    let (remaining, _close) = dynamic_context(
        || format!("Missing endfor for '{}' twig for expression", expression),
        cut(twig_closing_structure("endfor")),
    )(remaining)?;

    Ok((
        remaining,
        TwigFor {
            expression,
            children,
        },
    ))
}

pub(crate) fn twig_apply_block<R, T: GenericChildParser<R>>(input: Input) -> IResult<TwigApply<R>> {
    let (remaining, expression) = twig_expression_opening_block(input)?;
    let (remaining, children) = T::generic_parse_children(remaining)?;

    let (remaining, _close) = dynamic_context(
        || {
            format!(
                "Missing endapply for '{}' twig apply expression",
                expression
            )
        },
        cut(twig_closing_structure("endapply")),
    )(remaining)?;

    Ok((
        remaining,
        TwigApply {
            expression,
            children,
        },
    ))
}

pub(crate) fn twig_set_capture_block<R, T: GenericChildParser<R>>(
    input: Input,
) -> IResult<TwigSetCapture<R>> {
    let (remaining, name) = twig_single_word_opening_block(input)?;
    let (remaining, children) = T::generic_parse_children(remaining)?;

    let (remaining, _close) = dynamic_context(
        || format!("Missing endset for '{}' twig set expression", name),
        cut(twig_closing_structure("endset")),
    )(remaining)?;

    Ok((
        remaining,
        TwigSetCapture {
            name: name.to_owned(),
            children,
        },
    ))
}

pub(crate) fn twig_if_block<R, T: GenericChildParser<R>>(input: Input) -> IResult<TwigIf<R>> {
    let (remaining, expression) = twig_expression_opening_block(input)?;
    let (remaining, children) = T::generic_parse_children(remaining)?;
    let mut arms = vec![];

    arms.push(TwigIfArm {
        expression: Some(expression),
        children,
    });

    let mut outer_remaining = remaining;
    loop {
        let (remaining, _) = terminated(tag("{%"), multispace0)(outer_remaining)?;
        let (remaining, keyword) = terminated(alpha1, multispace0)(remaining)?;

        match keyword {
            "endif" => {
                let (remaining, _) = preceded(multispace0, tag("%}"))(remaining)?;
                outer_remaining = remaining;
                break;
            }
            "elseif" => {
                let (remaining, expression) = twig_expression_opening_block(remaining)?;
                let (remaining, children) = T::generic_parse_children(remaining)?;
                outer_remaining = remaining;

                arms.push(TwigIfArm {
                    expression: Some(expression),
                    children,
                });
            }
            "else" => {
                let (remaining, _) = preceded(multispace0, tag("%}"))(remaining)?;
                let (remaining, children) = T::generic_parse_children(remaining)?;
                outer_remaining = remaining;

                arms.push(TwigIfArm {
                    expression: None,
                    children,
                });
            }
            _ => {
                let error_info = TwigParsingErrorInformation {
                    leftover: remaining,
                    context: None,
                    kind: ErrorKind::Tag,
                };

                let error_info = TwigParsingErrorInformation::add_dynamic_context(
                    remaining,
                    format!(
                        "no 'endif' found for if block with expression '{}'",
                        // it's safe to unwrap here, because arms contains at least one element
                        arms.first().unwrap().expression.clone().unwrap_or_default()
                    ),
                    error_info,
                );

                return Err(nom::Err::Error(error_info));
            }
        };
    }

    Ok((outer_remaining, TwigIf { if_arms: arms }))
}

/// Parses any {% ... %} syntax and only the inner content is saved (with stripped whitespace around it).
/// An exception to this is the {% end... %} syntax, because there are
/// other parsers for this hierarchical nodes (and it is not allowed to steal that from them).
pub(crate) fn twig_statement<'a>(
    keyword: Input<'a>,
    input: Input<'a>,
) -> IResult<'a, TwigStatement> {
    let (remaining, spaces) = multispace0(input)?;

    let (remaining, m) = map(
        many_till(anychar, preceded(multispace0, tag("%}"))),
        |(v, _)| {
            let inner = v.iter().collect::<String>();
            if inner.is_empty() {
                TwigStatement::Raw(keyword.to_owned())
            } else {
                TwigStatement::Raw(format!("{}{}{}", keyword, spaces, inner))
            }
        },
    )(remaining)?;

    Ok((remaining, m))
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
    fn test_opening_twig_block() {
        let res = twig_single_word_opening_block(
            "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint"
            ))
        );
    }

    #[test]
    fn test_closing_twig_block() {
        let res = twig_closing_structure("endblock")("{% endblock %}");

        assert_eq!(res, Ok(("", "endblock")));
    }

    #[test]
    fn test_complete_twig_block() {
        let res = twig_syntax(
            "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>
                {% endblock %}",
        );

        assert_eq!(res, Ok(
            (
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock{
                        name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(),
                        children: vec![
                            SyntaxNode::Whitespace,
                            SyntaxNode::Tag(
                                Tag {
                                    name: "p".to_string(),
                                    self_closed: false,
                                    attributes: vec![
                                        TagAttribute::HtmlAttribute(HtmlAttribute {
                                            name: "class".to_string(),
                                            value: Some(
                                                "swag-migration-index-modal-abort-migration-confirm-dialog-hint".to_string(),
                                            ),
                                        }),
                                    ],
                                    children: vec![
                                        SyntaxNode::Whitespace,
                                        SyntaxNode::Plain(
                                            Plain {
                                                plain: "Hello world".to_string(),
                                            },
                                        ),
                                        SyntaxNode::Whitespace,
                                    ],
                                },
                            ),
                            SyntaxNode::Whitespace,
                        ],
                    }),
                ),
            ),
        ));
    }

    #[test]
    fn test_complete_twig_block_nested() {
        let res = twig_syntax("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                    {% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content %}
                        <div></div>
                    {% endblock %}
                {% endblock %}");

        assert_eq!(res, Ok(("", SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock{ name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(), children: vec![
            SyntaxNode::Whitespace,
            SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content".to_string(),
                children: vec![
                    SyntaxNode::Whitespace,
                    SyntaxNode::Tag(Tag {
                    name: "div".to_string(),
                    ..Default::default()
                }), SyntaxNode::Whitespace]
            })),
            SyntaxNode::Whitespace
        ] })))));
    }

    #[test]
    fn test_complete_twig_block_without_space_at_the_end() {
        let res = twig_syntax(
            "{% block swag_migration_history_detail_errors_grid_code%}
                    {% endblock%}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "swag_migration_history_detail_errors_grid_code".to_string(),
                    children: vec![SyntaxNode::Whitespace]
                }))
            ))
        )
    }

    #[test]
    fn test_complete_twig_block_with_many_whitespace() {
        let res = twig_syntax(
            "{%          block                swag_migration_history_detail_errors_grid_code                                    %}
                    {%             endblock             %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "swag_migration_history_detail_errors_grid_code".to_string(),
                    children: vec![SyntaxNode::Whitespace]
                }))
            ))
        )
    }

    #[test]
    fn test_parent_block_call() {
        let res = twig_syntax(
            "{% block sw_dashboard_index_content_intro_card %}
                {% parent %}
            {% endblock %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "sw_dashboard_index_content_intro_card".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStatement(TwigStatement::Raw("parent".to_string())),
                        SyntaxNode::Whitespace,
                    ]
                }))
            ))
        )
    }

    #[test]
    fn test_twig_parent_call_variations() {
        assert_eq!(
            twig_syntax("{%parent%}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("parent".to_string()))
            ))
        );

        assert_eq!(
            twig_syntax("{%           parent         %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("parent".to_string()))
            ))
        );

        assert_eq!(
            twig_syntax("{%parent()%}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("parent()".to_string()))
            ))
        );

        assert_eq!(
            twig_syntax("{%        parent()         %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("parent()".to_string()))
            ))
        );
    }

    #[test]
    fn test_twig_comment() {
        assert_eq!(
            twig_comment("{# @deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\". #}"),
            Ok(("", SyntaxNode::TwigComment(TwigComment{ content: "@deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\".".to_string() })))
        );

        assert_eq!(
            twig_comment("{#                   @deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\".                #}"),
            Ok(("", SyntaxNode::TwigComment(TwigComment{ content: "@deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\".".to_string() })))
        );

        assert_eq!(
            twig_comment("{#@deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\".#}"),
            Ok(("", SyntaxNode::TwigComment(TwigComment{ content: "@deprecated tag:v6.4.0 - Will be removed. Mail template assignment will be done via \"sw-event-action\".".to_string() })))
        );
    }

    #[test]
    fn test_twig_for() {
        assert_eq!(
            twig_syntax("{% for i in 0..10 %} {{ i }} {% endfor %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigFor(TwigFor {
                    expression: "i in 0..10".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "i".to_string()
                        }),
                        SyntaxNode::Whitespace
                    ]
                }))
            ))
        );

        assert_eq!(
            twig_syntax("{% for       i in 0..10      %}{{ i }}{%     endfor        %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigFor(TwigFor {
                    expression: "i in 0..10".to_string(),
                    children: vec![SyntaxNode::OutputExpression(OutputExpression {
                        content: "i".to_string()
                    })]
                }))
            ))
        );

        assert_eq!(
            twig_syntax("{% for letter in 'a'|upper..'z'|upper %} {{ letter }} {% endfor %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigFor(TwigFor {
                    expression: "letter in 'a'|upper..'z'|upper".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "letter".to_string()
                        }),
                        SyntaxNode::Whitespace
                    ]
                }))
            ))
        );
    }

    #[test]
    fn test_twig_if() {
        assert_eq!(
            twig_syntax("{% if users != null %}Hello Users{% endif %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![TwigIfArm {
                        expression: Some("users != null".to_string()),
                        children: vec![SyntaxNode::Plain(Plain {
                            plain: "Hello Users".to_string()
                        })]
                    }]
                }))
            ))
        );

        assert_eq!(
            twig_syntax(
                "{% if users != null %}Hello Users{% elseif account != null %}Hello Sir{% endif %}"
            ),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("users != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Users".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: Some("account != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Sir".to_string()
                            })]
                        },
                    ]
                }))
            ))
        );

        assert_eq!(
            twig_syntax(
                "{% if users != null %}Hello Users{% elseif account != null %}Hello Sir{% else %}Hello Other{% endif %}"
            ),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("users != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Users".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: Some("account != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Sir".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Other".to_string()
                            })]
                        },
                    ]
                }))
            ))
        );

        assert_eq!(
            twig_syntax(
                "{% if users != null %}Hello Users{% elseif account != null %}Hello Sir{% elseif something > 100 %}Hello Big{% else %}Hello Other{% endif %}"
            ),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("users != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Users".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: Some("account != null".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Sir".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: Some("something > 100".to_string()),
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Big".to_string()
                            })]
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "Hello Other".to_string()
                            })]
                        },
                    ]
                }))
            ))
        );
    }

    #[test]
    fn test_twig_structures_without_expression_or_word() {
        assert!(twig_syntax("{% block      %}Hello{% endblock %}").is_err());
        assert!(twig_syntax("{% for     %}Hello{% endfor %}").is_err());
        assert!(twig_syntax("{% if      %}Hello{% endif %}").is_err());
        assert!(twig_syntax("{% if something %}Hello{% elseif      %}{% endif %}").is_err());
    }

    #[test]
    fn test_twig_raw_statement() {
        assert_eq!(
            twig_syntax("{% some generic twig statement %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw(
                    "some generic twig statement".to_string()
                ))
            ))
        );

        assert_eq!(
            twig_syntax("{% some           generic     twig statement %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw(
                    "some           generic     twig statement".to_string()
                ))
            ))
        );

        assert_eq!(
            twig_syntax("{% set foo = 'bar' %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("set foo = 'bar'".to_string()))
            ))
        );

        assert_eq!(
            twig_syntax("{% set foo = [1, 2] %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw("set foo = [1, 2]".to_string()))
            ))
        );

        assert_eq!(
            twig_syntax("{% set foo = {'foo': 'bar'} %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw(
                    "set foo = {'foo': 'bar'}".to_string()
                ))
            ))
        );

        assert_eq!(
            twig_syntax("{% set foo = 'foo' ~ 'bar' %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw(
                    "set foo = 'foo' ~ 'bar'".to_string()
                ))
            ))
        );

        assert_eq!(
            twig_syntax("{% set foo, bar = 'foo', 'bar' %}"),
            Ok((
                "",
                SyntaxNode::TwigStatement(TwigStatement::Raw(
                    "set foo, bar = 'foo', 'bar'".to_string()
                ))
            ))
        );
    }

    #[test]
    fn test_twig_apply_block() {
        assert_eq!(
            twig_syntax("{% apply upper %} hello world {% endapply %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigApply(TwigApply {
                    expression: "upper".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "hello world".to_string()
                        }),
                        SyntaxNode::Whitespace
                    ]
                }))
            ))
        );

        assert_eq!(
            twig_syntax(
                "{% apply lower|escape('html') %} <strong>SOME TEXT</strong> {% endapply %}"
            ),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigApply(TwigApply {
                    expression: "lower|escape('html')".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Tag(Tag {
                            name: "strong".to_string(),
                            self_closed: false,
                            attributes: vec![],
                            children: vec![SyntaxNode::Plain(Plain {
                                plain: "SOME TEXT".to_string()
                            })],
                        }),
                        SyntaxNode::Whitespace
                    ]
                }))
            ))
        );
    }

    #[test]
    fn test_twig_set_capture() {
        assert_eq!(
            twig_syntax("{% set foo %} hello world {% endset %}"),
            Ok((
                "",
                SyntaxNode::TwigStructure(TwigStructure::TwigSetCapture(TwigSetCapture {
                    name: "foo".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "hello world".to_string()
                        }),
                        SyntaxNode::Whitespace
                    ]
                }))
            ))
        );
    }
}

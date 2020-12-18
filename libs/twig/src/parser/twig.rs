use super::IResult;
use crate::ast::*;
use crate::parser::general::{document_node, dynamic_context, Input};
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::{anychar, multispace0};
use nom::combinator::{cut, map, opt};
use nom::multi::{many0, many_till};
use nom::sequence::{delimited, preceded, terminated};

pub(crate) fn twig_opening_block(input: Input) -> IResult<Input> {
    delimited(
        tag("{%"),
        preceded(
            delimited(multispace0, tag("block"), multispace0),
            terminated(
                take_till1(|c| char::is_whitespace(c) || c == '%'),
                multispace0,
            ),
        ),
        tag("%}"),
    )(input)
}

pub(crate) fn twig_closing_block(input: Input) -> IResult<Input> {
    delimited(
        tag("{%"),
        delimited(multispace0, tag("endblock"), multispace0),
        tag("%}"),
    )(input)
}

pub(crate) fn twig_complete_block(input: Input) -> IResult<SyntaxNode> {
    let (remaining, open) = twig_opening_block(input)?;
    let (remaining, children) = many0(document_node)(remaining)?;

    let (remaining, _close) = dynamic_context(
        || format!("Missing endblock for '{}' twig block", open),
        cut(twig_closing_block),
    )(remaining)?;

    let block = TwigBlock {
        name: open.to_owned(),
        children,
    };

    Ok((remaining, SyntaxNode::TwigBlock(block)))
}

pub(crate) fn twig_parent_call(input: Input) -> IResult<SyntaxNode> {
    let (remaining, _) = delimited(
        tag("{%"),
        delimited(
            multispace0,
            terminated(tag("parent"), opt(tag("()"))),
            multispace0,
        ),
        tag("%}"),
    )(input)?;

    Ok((remaining, SyntaxNode::TwigParentCall))
}

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
        let res = twig_opening_block("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}");

        assert!(res.is_ok())
    }

    #[test]
    fn test_closing_twig_block() {
        let res = twig_closing_block("{% endblock %}");

        assert!(res.is_ok())
    }

    #[test]
    fn test_complete_twig_block() {
        let res = twig_complete_block("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>
                {% endblock %}");

        assert!(res.is_ok())
    }

    #[test]
    fn test_complete_twig_block_nested() {
        let res = twig_complete_block("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                    {% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content %}
                        <div></div>
                    {% endblock %}
                {% endblock %}");

        assert_eq!(res, Ok(("", SyntaxNode::TwigBlock(TwigBlock{name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(), children: vec![
            SyntaxNode::Whitespace,
            SyntaxNode::TwigBlock(TwigBlock{
                name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content".to_string(),
                children: vec![
                    SyntaxNode::Whitespace,
                    SyntaxNode::Tag(Tag {
                    name: "div".to_string(),
                    ..Default::default()
                }), SyntaxNode::Whitespace]
            }),
            SyntaxNode::Whitespace
        ] }))));
    }

    #[test]
    fn test_complete_twig_block_without_space_at_the_end() {
        let res = twig_complete_block(
            "{% block swag_migration_history_detail_errors_grid_code%}
                    {% endblock%}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "swag_migration_history_detail_errors_grid_code".to_string(),
                    children: vec![SyntaxNode::Whitespace]
                })
            ))
        )
    }

    #[test]
    fn test_complete_twig_block_with_many_whitespace() {
        let res = twig_complete_block(
            "{%          block                swag_migration_history_detail_errors_grid_code                                    %}
                    {%             endblock             %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "swag_migration_history_detail_errors_grid_code".to_string(),
                    children: vec![SyntaxNode::Whitespace]
                })
            ))
        )
    }

    #[test]
    fn test_parent_block_call() {
        let res = twig_complete_block(
            "{% block sw_dashboard_index_content_intro_card %}
                {% parent %}
            {% endblock %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "sw_dashboard_index_content_intro_card".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigParentCall,
                        SyntaxNode::Whitespace,
                    ]
                })
            ))
        )
    }

    #[test]
    fn test_twig_parent_call_variations() {
        assert_eq!(
            twig_parent_call("{%parent%}"),
            Ok(("", SyntaxNode::TwigParentCall))
        );

        assert_eq!(
            twig_parent_call("{%           parent         %}"),
            Ok(("", SyntaxNode::TwigParentCall))
        );

        assert_eq!(
            twig_parent_call("{%parent()%}"),
            Ok(("", SyntaxNode::TwigParentCall))
        );

        assert_eq!(
            twig_parent_call("{%        parent()         %}"),
            Ok(("", SyntaxNode::TwigParentCall))
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
}

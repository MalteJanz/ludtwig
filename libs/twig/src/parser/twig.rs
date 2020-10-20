use super::IResult;
use crate::ast::*;
use crate::parser::general::{document_node, dynamic_context};
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::multispace0;
use nom::combinator::cut;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated};

pub(crate) fn twig_opening_block(input: &str) -> IResult<&str> {
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

pub(crate) fn twig_closing_block(input: &str) -> IResult<&str> {
    // {% endblock %}
    delimited(
        tag("{%"),
        delimited(multispace0, tag("endblock"), multispace0),
        tag("%}"),
    )(input)
}

pub(crate) fn twig_parent_call(input: &str) -> IResult<HtmlNode> {
    let (remaining, _) = delimited(
        tag("{%"),
        delimited(multispace0, tag("parent"), multispace0),
        tag("%}"),
    )(input)?;

    Ok((remaining, HtmlNode::TwigParentCall))
}

pub(crate) fn twig_complete_block(input: &str) -> IResult<HtmlNode> {
    // TODO: also parser whitespace because it matters in rendering!: https://prettier.io/blog/2018/11/07/1.15.0.html
    let (remaining, open) = twig_opening_block(input)?;
    let (remaining, children) = many0(document_node)(remaining)?;

    let (remaining, _close) = dynamic_context(
        format!("Missing endblock for '{}' twig block", open),
        cut(twig_closing_block),
    )(remaining)?;

    let block = TwigBlock {
        name: open.to_owned(),
        children,
    };

    Ok((remaining, HtmlNode::TwigBlock(block)))
}

#[cfg(test)]
mod tests {
    use super::*;

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

        assert_eq!(res, Ok(("", HtmlNode::TwigBlock(TwigBlock{name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(), children: vec![
            HtmlNode::Whitespace,
            HtmlNode::TwigBlock(TwigBlock{
                name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content".to_string(),
                children: vec![
                    HtmlNode::Whitespace,
                    HtmlNode::Tag(HtmlTag{
                    name: "div".to_string(),
                    ..Default::default()
                }), HtmlNode::Whitespace]
            }),
            HtmlNode::Whitespace
        ] }))));
    }

    #[test]
    fn test_complete_twig_block_without_space_at_the_end() {
        let res = twig_complete_block(
            "{% block swag_migration_history_detail_errors_grid_code%}
                    {% endblock %}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                HtmlNode::TwigBlock(TwigBlock {
                    name: "swag_migration_history_detail_errors_grid_code".to_string(),
                    children: vec![HtmlNode::Whitespace]
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
                HtmlNode::TwigBlock(TwigBlock {
                    name: "sw_dashboard_index_content_intro_card".to_string(),
                    children: vec![
                        HtmlNode::Whitespace,
                        HtmlNode::TwigParentCall,
                        HtmlNode::Whitespace,
                    ]
                })
            ))
        )
    }
}

use super::IResult;
use crate::ast::*;
use crate::parser::general::document_node;
use nom::bytes::complete::{tag, take_till, take_till1};
use nom::character::complete::multispace0;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated};

pub(crate) fn twig_opening_block(input: &str) -> IResult<&str> {
    delimited(
        multispace0,
        delimited(
            tag("{%"),
            preceded(
                delimited(multispace0, tag("block"), multispace0),
                terminated(take_till1(char::is_whitespace), multispace0),
            ),
            tag("%}"),
        ),
        multispace0,
    )(input)
}

pub(crate) fn twig_closing_block(input: &str) -> IResult<&str> {
    // {% endblock %}
    delimited(
        multispace0,
        delimited(
            tag("{%"),
            delimited(multispace0, tag("endblock"), multispace0),
            tag("%}"),
        ),
        multispace0,
    )(input)
}

pub(crate) fn twig_complete_block(input: &str) -> IResult<HtmlNode> {
    // TODO: also parser whitespace because it matters in rendering!: https://prettier.io/blog/2018/11/07/1.15.0.html
    let (remaining, open) = twig_opening_block(input)?;
    let (remaining, children) = many0(document_node)(remaining)?;

    let (remaining, _close) = preceded(take_till(|c| c == '{'), twig_closing_block)(remaining)?;

    let block = TwigBlock {
        name: open,
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

        println!("{:#?}", res);
        assert!(res.is_ok())
    }

    #[test]
    fn test_complete_twig_block_nested() {
        let res = twig_complete_block("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                    {% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content %}
                        <div></div>
                    {% endblock %}
                {% endblock %}");

        assert_eq!(res, Ok(("", HtmlNode::TwigBlock(TwigBlock{name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint", children: vec![
            HtmlNode::TwigBlock(TwigBlock{
                name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint_content",
                children: vec![HtmlNode::Tag(HtmlTag{
                    name: "div",
                    self_closed: false,
                    arguments: Default::default(),
                    children: vec![]
                })]
            })
        ] }))));
    }
}

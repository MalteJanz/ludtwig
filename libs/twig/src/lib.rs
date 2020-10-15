pub mod ast;
mod error;
mod parser;

pub use error::TwigParseError;

use crate::ast::*;
use crate::parser::general::document_node_all;
use nom::combinator::all_consuming;

pub fn parse(input: &str) -> Result<HtmlNode, /*VerboseError<&str>*/ TwigParseError<&str>> {
    let (_, result) = all_consuming(document_node_all)(input)?; /*.map_err(|nom_err| match nom_err {
                                                                    nom::Err::Incomplete(_) => unreachable!(),
                                                                    nom::Err::Error(e) => e,
                                                                    nom::Err::Failure(f) => f,
                                                                })?;*/

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn it_works() {
        let result = parse("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>
                {% endblock %}");

        let mut attributes = HashMap::new();
        attributes.insert(
            "class",
            "swag-migration-index-modal-abort-migration-confirm-dialog-hint",
        );

        assert_eq!(
            result,
            Ok(HtmlNode::Tag(HtmlTag {
                name: "ROOT",
                self_closed: false,
                arguments: Default::default(),
                children: vec![
                    HtmlNode::TwigBlock(TwigBlock{
                        name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint",
                        children: vec![
                        HtmlNode::Tag(HtmlTag{
                            name: "p",
                            self_closed: false,
                            arguments: attributes,
                            children: vec![
                            HtmlNode::Plain(HtmlPlain{ plain: "Hello world" })]
                        })]
                    })
                ]
            }))
        )
    }

    #[test]
    fn test_missing_closing_tag_error() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                <div>
                </div>
                {% endblock %}";
        let result = parse(input).unwrap_err();

        println!("{:#?}", result);
        result.pretty_print_userfriendly_error(input);

        //println!("{}", nom::error::convert_error(input, result));
    }

    #[test]
    fn test_missing_closing_block_error() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>";
        let result = parse(input).unwrap_err();

        println!("{:#?}", result);
        result.pretty_print_userfriendly_error(input);

        //println!("{}", nom::error::convert_error(input, result));
    }

    #[test]
    fn test_missing_closing_block_error_asdf() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                </p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                <p></p>
                {% endblock %}";
        let result = parse(input).unwrap_err();

        println!("{:#?}", result);
        result.pretty_print_userfriendly_error(input);

        //println!("{}", nom::error::convert_error(input, result));
    }
}

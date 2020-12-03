pub mod ast;
mod error;
mod parser;

pub use error::TwigParseError;

use crate::ast::*;
use crate::parser::general::document_node_all;
use nom::combinator::all_consuming;

pub fn parse(input: &str) -> Result<HtmlNode, TwigParseError<&str>> {
    let (_, result) = all_consuming(document_node_all)(input)?;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = parse("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>
                {% endblock %}");

        let attributes = vec![("class".to_string(),
                                   "swag-migration-index-modal-abort-migration-confirm-dialog-hint".to_string())];

        assert_eq!(
            result,
            Ok(HtmlNode::Root(vec![
                    HtmlNode::TwigBlock(TwigBlock{
                        name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(),
                        children: vec![
                            HtmlNode::Whitespace,
                            HtmlNode::Tag(HtmlTag{
                                name: "p".to_string(),
                                self_closed: false,
                                attributes,
                                children: vec![
                                    HtmlNode::Whitespace,
                                    HtmlNode::Plain(HtmlPlain{ plain: "Hello world".to_string() }),
                                    HtmlNode::Whitespace,
                                ]
                            }),
                            HtmlNode::Whitespace
                        ]
                    })
                ])
            )
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

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 6 and column 17 :\n                {% endblock %}\n                ^\n                |\nMissing closing tag for opening tag \'p\' with attributes [(\"class\", \"swag-migration-index-modal-abort-migration-confirm-dialog-hint\")]");
    }

    #[test]
    fn test_missing_closing_block_error() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 4 and column 21 :\n                </p>\n                    ^\n                    |\nMissing endblock for \'swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint\' twig block");
    }

    #[test]
    fn test_missing_closing_tag_error_nested() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <div>
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">

                    Hello world
                <p></p>
                </div>
                {% endblock %}";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 7 and column 19 :\n                </div>\n                  ^\n                  |\nMissing closing tag for opening tag \'p\' with attributes [(\"class\", \"swag-migration-index-modal-abort-migration-confirm-dialog-hint\")]");
    }

    #[test]
    fn test_missing_quote_in_tag_argument() {
        let input = "
                <p class=swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 1 and column 26 :\n                <p class=swag-migration-index-modal-abort-migration-confirm-dialog-hint\">\n                         ^\n                         |\nmissing \'\"\' quote");
    }

    #[test]
    fn test_missing_quote_in_tag_argument_end() {
        let input = "<sw-button size=\"small @click=\"onCloseModal\">
                click me
            </sw-button>";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 1 and column 44 :\n<sw-button size=\"small @click=\"onCloseModal\">\n                                           ^\n                                           |\ninvalid attribute name");
    }
}

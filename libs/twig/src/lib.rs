pub mod ast;
pub mod error;
mod parser;

pub use ast::SyntaxNode;
pub use error::TwigParseError;

use crate::parser::general::{document_node_all, Input};
use nom::combinator::all_consuming;

/// Parses a template into an AST of the [SyntaxNode] type.
/// If it fails it will return a [TwigParseError] which contains a function
/// to generate a human readable error message [TwigParseError::pretty_helpful_error_string].
pub fn parse(input: Input) -> Result<SyntaxNode, TwigParseError<Input>> {
    let (_, result) = all_consuming(document_node_all)(input)?;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    #[test]
    fn it_works() {
        let result = parse("{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\">
                    Hello world
                </p>
                {% endblock %}");

        let attributes = vec![TagAttribute::HtmlAttribute(HtmlAttribute {
            name: "class".to_string(),
            value: Some(
                "swag-migration-index-modal-abort-migration-confirm-dialog-hint".to_string(),
            ),
        })];

        assert_eq!(
            result,
            Ok(SyntaxNode::Root(vec![
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock{
                        name: "swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint".to_string(),
                        children: vec![
                            SyntaxNode::Whitespace,
                            SyntaxNode::Tag(Tag {
                                name: "p".to_string(),
                                self_closed: false,
                                attributes,
                                children: vec![
                                    SyntaxNode::Whitespace,
                                    SyntaxNode::Plain(Plain { plain: "Hello world".to_string() }),
                                    SyntaxNode::Whitespace,
                                ]
                            }),
                            SyntaxNode::Whitespace
                        ]
                    }))
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
        assert_eq!(pretty, "Parsing goes wrong in line 6 and column 17 :\n                {% endblock %}\n                ^\n                |\nMissing closing tag for opening tag \'p\' with attributes [ class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" ]");
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
        assert_eq!(pretty, "Parsing goes wrong in line 7 and column 19 :\n                </div>\n                  ^\n                  |\nMissing closing tag for opening tag \'p\' with attributes [ class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" ]");
    }

    #[test]
    fn test_missing_closing_tag_error_with_twig_if_in_attributes() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" {# this is a comment #}
                   {% if isHidden %}
                       style=\"display: none;\"
                   {% endif %}>
                    Hello world
                <div>
                </div>
                {% endblock %}";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 9 and column 17 :\n                {% endblock %}\n                ^\n                |\nMissing closing tag for opening tag \'p\' with attributes [ class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" {# this is a comment #} {% if isHidden %} style=\"display: none;\" {% endif %} ]");
    }

    #[test]
    fn test_missing_closing_tag_error_with_twig_if_elseif_else_in_attributes() {
        let input = "{% block swag_migration_index_main_page_modal_abort_migration_confirmDialog_message_hint %}
                <p class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" {# this is a comment #}
                   {% if isHidden %}
                       style=\"display: none;\"
                   {% elseif isInline %}
                       style=\"display: inline;\"
                   {% else %}
                       style=\"display: block;\"
                   {% endif %}>
                    Hello world
                <div>
                </div>
                {% endblock %}";
        let result = parse(input).unwrap_err();

        let pretty = result.pretty_helpful_error_string(input);
        //println!("{}", pretty);
        assert_eq!(pretty, "Parsing goes wrong in line 13 and column 17 :\n                {% endblock %}\n                ^\n                |\nMissing closing tag for opening tag \'p\' with attributes [ class=\"swag-migration-index-modal-abort-migration-confirm-dialog-hint\" {# this is a comment #} {% if isHidden %} style=\"display: none;\" {% elseif isInline %} style=\"display: inline;\" {% else %} style=\"display: block;\" {% endif %} ]");
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

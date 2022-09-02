use crate::grammar::html::{parse_html_element, parse_html_text};
use crate::grammar::twig::parse_twig_block_statement;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

mod html;
mod twig;

pub(super) fn root(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();

    while parse_any_element(parser).is_some() {}

    parser.complete(m, SyntaxKind::ROOT)
}

fn parse_any_element(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at_end() {
        None
    } else if parser.at(T!["{%"]) {
        parse_twig_block_statement(parser)
    } else if parser.at(T!["<"]) {
        Some(parse_html_element(parser))
    } else if parser.at(T![word]) {
        Some(parse_html_text(parser))
    } else {
        None
    }
}

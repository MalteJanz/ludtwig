use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_twig_block(parser: &mut Parser) {
    // TODO: implement me
    let checkpoint = parser.checkpoint();

    match parser.peek() {
        Some(T!["{%"]) => parser.bump(),
        _ => {} // TODO: error handling
    }

    match parser.peek() {
        Some(T!["block"]) => parser.bump(),
        _ => {} // TODO: error handling
    }

    match parser.peek() {
        Some(T![word]) => parser.bump(),
        _ => {} // TODO: error handling
    }

    match parser.peek() {
        Some(T!["%}"]) => parser.bump(),
        _ => {} // TODO: error handling
    }

    parser.start_node_at(checkpoint, SyntaxKind::TWIG_STARTING_BLOCK);
    parser.finish_node();
}

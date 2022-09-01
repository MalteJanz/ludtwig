use crate::grammar::twig::parse_twig_block;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

mod twig;

pub(super) fn root(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();

    loop {
        if parser.at_end() {
            break;
        } else if parser.at(T!["{%"]) {
            parse_twig_block(parser)
        } else {
            parser.error();
        }
    }

    parser.complete(m, SyntaxKind::ROOT)
}

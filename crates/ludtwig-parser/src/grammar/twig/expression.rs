use crate::grammar::twig::literal::parse_twig_literal;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;

pub(super) fn parse_twig_expression(parser: &mut Parser) -> Option<CompletedMarker> {
    // TODO: implement me
    parse_twig_literal(parser).map(|m| {
        let preceded = parser.precede(m);
        parser.complete(preceded, SyntaxKind::TWIG_EXPRESSION)
    })
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;
}

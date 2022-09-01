use crate::grammar::parse_any_element;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_html_text(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T![word]));
    let m = parser.start();
    parser.bump();

    while parser.at(T![word]) {
        parser.bump();
    }

    parser.complete(m, SyntaxKind::BODY)
}

pub(super) fn parse_html_element(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<"]));
    let m = parser.start();
    parser.bump();

    parser.expect(T![word]);

    // todo attributes

    parser.expect(T![">"]);

    // parse all the children (including end tag if exists)
    while parse_any_element(parser).is_some() {}

    parser.complete(m, SyntaxKind::HTML_TAG)
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

    #[test]
    fn parse_html_element() {
        check_parse(
            "<div></div>",
            expect![[r#"
                ROOT@0..5
                  HTML_TAG@0..5
                    TK_LESS_THAN@0..1 "<"
                    TK_WORD@1..4 "div"
                    TK_GREATER_THAN@4..5 ">""#]],
        );
    }
}

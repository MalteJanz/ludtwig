use crate::grammar::parse_any_element;
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_twig_block_statement(parser: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(parser.at(T!["{%"]));
    let m = parser.start();
    parser.bump();

    if parser.at(T!["block"]) {
        Some(parse_twig_block(parser, m))
    } else if parser.at(T!["endblock"]) {
        Some(parse_twig_end_block(parser, m))
    } else {
        // TODO: implement other twig block statements like if, for, and so on
        parser.error();
        parser.complete(m, SyntaxKind::ERROR);
        None
    }
}

fn parse_twig_block(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["block"]));

    parser.expect(T!["block"]);
    parser.expect(T![word]);
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children (including twig end block if exists)
    while parse_any_element(parser).is_some() {}

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::TWIG_BLOCK)
}

fn parse_twig_end_block(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["endblock"]));
    parser.bump();

    parser.expect(T!["%}"]);
    parser.complete(outer, SyntaxKind::TWIG_ENDING_BLOCK)
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

    #[test]
    fn parse_twig_block() {
        check_parse(
            "{% block block_name %} hello world {% endblock %}",
            expect![[r#"
                ROOT@0..49
                  TWIG_BLOCK@0..49
                    TWIG_STARTING_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..19 "block_name"
                      TK_WHITESPACE@19..20 " "
                      TK_PERCENT_CURLY@20..22 "%}"
                      TK_WHITESPACE@22..23 " "
                    BODY@23..35
                      TK_WORD@23..28 "hello"
                      TK_WHITESPACE@28..29 " "
                      TK_WORD@29..34 "world"
                      TK_WHITESPACE@34..35 " "
                    TWIG_ENDING_BLOCK@35..49
                      TK_CURLY_PERCENT@35..37 "{%"
                      TK_WHITESPACE@37..38 " "
                      TK_ENDBLOCK@38..46 "endblock"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_error() {
        check_parse(
            "{% asdf",
            expect![[r#"
                ROOT@0..7
                  ERROR@0..7
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    ERROR@3..7
                      TK_WORD@3..7 "asdf"
                error at 3..3: expected block or endblock, but found word"#]],
        )
    }
}

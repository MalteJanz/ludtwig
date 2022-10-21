use crate::grammar::twig::literal::parse_twig_string;
use crate::grammar::ParseFunction;
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(crate) enum BlockParseResult {
    Successful(CompletedMarker),
    NothingFound(Marker),
}

pub(crate) fn parse_shopware_twig_block_statement(
    parser: &mut Parser,
    outer: Marker,
    _child_parser: ParseFunction,
) -> BlockParseResult {
    // {% already consumed
    if parser.at(T!["sw_extends"]) {
        BlockParseResult::Successful(parse_twig_sw_extends(parser, outer))
    } else {
        // error will be thrown by calling function
        BlockParseResult::NothingFound(outer)
    }
}

fn parse_twig_sw_extends(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_extends"]));
    parser.bump();

    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else {
        parser.add_error(ParseErrorBuilder::new(
            "twig string as template (shopware doesn't allow expressions here)",
        ));
    }

    parser.expect(T!["%}"]);
    parser.complete(outer, SyntaxKind::SHOPWARE_TWIG_SW_EXTENDS)
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

    #[test]
    fn parse_sw_extends() {
        check_parse(
            "{% sw_extends '@Storefront/storefront/base.html.twig' %}",
            expect![[r#"
                ROOT@0..56
                  SHOPWARE_TWIG_SW_EXTENDS@0..56
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_EXTENDS@3..13 "sw_extends"
                    TWIG_LITERAL_STRING@13..53
                      TK_WHITESPACE@13..14 " "
                      TK_SINGLE_QUOTES@14..15 "'"
                      TWIG_LITERAL_STRING_INNER@15..52
                        TK_WORD@15..26 "@Storefront"
                        TK_FORWARD_SLASH@26..27 "/"
                        TK_WORD@27..37 "storefront"
                        TK_FORWARD_SLASH@37..38 "/"
                        TK_WORD@38..42 "base"
                        TK_DOT@42..43 "."
                        TK_WORD@43..47 "html"
                        TK_DOT@47..48 "."
                        TK_WORD@48..52 "twig"
                      TK_SINGLE_QUOTES@52..53 "'"
                    TK_WHITESPACE@53..54 " "
                    TK_PERCENT_CURLY@54..56 "%}""#]],
        )
    }
}

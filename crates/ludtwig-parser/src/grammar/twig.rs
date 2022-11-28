mod expression;
pub(crate) mod literal;
mod shopware;
mod tags;

pub(crate) use tags::at_twig_termination_tag;

use crate::grammar::twig::expression::parse_twig_expression;
use crate::grammar::{parse_ludtwig_directive, parse_many, ParseFunction};
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub use literal::TWIG_NAME_REGEX;

pub(super) fn parse_any_twig(
    parser: &mut Parser,
    child_parser: ParseFunction,
) -> Option<CompletedMarker> {
    if parser.at(T!["{%"]) {
        tags::parse_twig_block_statement(parser, child_parser)
    } else if parser.at(T!["{{"]) {
        Some(parse_twig_var_statement(parser))
    } else if parser.at(T!["{#"]) {
        Some(parse_twig_comment_statement(parser))
    } else {
        None
    }
}

fn parse_twig_comment_statement(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["{#"]));
    let m = parser.start();
    parser.bump();

    if parser.at_set(&[T!["ludtwig-ignore-file"], T!["ludtwig-ignore"]]) {
        parse_ludtwig_directive(parser, m, T!["#}"])
    } else {
        parse_twig_plain_comment(parser, m)
    }
}

fn parse_twig_plain_comment(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    parse_many(
        parser,
        |p| p.at(T!["#}"]),
        |p| {
            p.bump();
        },
    );

    parser.expect(T!["#}"], &[]);
    parser.complete(outer, SyntaxKind::TWIG_COMMENT)
}

pub(crate) fn parse_twig_var_statement(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["{{"]));
    let m = parser.start();
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
    }

    parser.expect(T!["}}"], &[]);
    parser.complete(m, SyntaxKind::TWIG_VAR)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;

    #[test]
    fn parse_twig_var() {
        check_parse(
            "{{ something }} plain {{ else }}",
            expect![[r#"
                ROOT@0..32
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..12
                      TWIG_LITERAL_NAME@2..12
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..12 "something"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}"
                  HTML_TEXT@15..21
                    TK_WHITESPACE@15..16 " "
                    TK_WORD@16..21 "plain"
                  TWIG_VAR@21..32
                    TK_WHITESPACE@21..22 " "
                    TK_OPEN_CURLY_CURLY@22..24 "{{"
                    TWIG_EXPRESSION@24..29
                      TWIG_LITERAL_NAME@24..29
                        TK_WHITESPACE@24..25 " "
                        TK_WORD@25..29 "else"
                    TK_WHITESPACE@29..30 " "
                    TK_CLOSE_CURLY_CURLY@30..32 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_comment() {
        check_parse(
            "{# something #} plain {# {{ comment }} {% block asdf %} #}",
            expect![[r##"
                ROOT@0..58
                  TWIG_COMMENT@0..15
                    TK_OPEN_CURLY_HASHTAG@0..2 "{#"
                    TK_WHITESPACE@2..3 " "
                    TK_WORD@3..12 "something"
                    TK_WHITESPACE@12..13 " "
                    TK_HASHTAG_CLOSE_CURLY@13..15 "#}"
                  HTML_TEXT@15..21
                    TK_WHITESPACE@15..16 " "
                    TK_WORD@16..21 "plain"
                  TWIG_COMMENT@21..58
                    TK_WHITESPACE@21..22 " "
                    TK_OPEN_CURLY_HASHTAG@22..24 "{#"
                    TK_WHITESPACE@24..25 " "
                    TK_OPEN_CURLY_CURLY@25..27 "{{"
                    TK_WHITESPACE@27..28 " "
                    TK_WORD@28..35 "comment"
                    TK_WHITESPACE@35..36 " "
                    TK_CLOSE_CURLY_CURLY@36..38 "}}"
                    TK_WHITESPACE@38..39 " "
                    TK_CURLY_PERCENT@39..41 "{%"
                    TK_WHITESPACE@41..42 " "
                    TK_BLOCK@42..47 "block"
                    TK_WHITESPACE@47..48 " "
                    TK_WORD@48..52 "asdf"
                    TK_WHITESPACE@52..53 " "
                    TK_PERCENT_CURLY@53..55 "%}"
                    TK_WHITESPACE@55..56 " "
                    TK_HASHTAG_CLOSE_CURLY@56..58 "#}""##]],
        )
    }
}

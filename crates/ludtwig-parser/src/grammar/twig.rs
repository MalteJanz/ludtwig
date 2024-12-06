mod expression;
pub(crate) mod literal;
mod shopware;
mod tags;

pub(crate) use tags::at_twig_termination_tag;

use crate::grammar::twig::expression::{parse_twig_expression, TWIG_EXPRESSION_RECOVERY_SET};
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
        parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
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
        );
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
        );
    }

    #[test]
    fn parse_twig_component_call() {
        check_parse(
            "{{ component('Alert', { message: 'Hello Twig Components!' }) }}",
            expect![[r#"
                ROOT@0..63
                  TWIG_VAR@0..63
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..60
                      TWIG_FUNCTION_CALL@2..60
                        TWIG_OPERAND@2..12
                          TWIG_LITERAL_NAME@2..12
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..12 "component"
                        TWIG_ARGUMENTS@12..60
                          TK_OPEN_PARENTHESIS@12..13 "("
                          TWIG_EXPRESSION@13..20
                            TWIG_LITERAL_STRING@13..20
                              TK_SINGLE_QUOTES@13..14 "'"
                              TWIG_LITERAL_STRING_INNER@14..19
                                TK_WORD@14..19 "Alert"
                              TK_SINGLE_QUOTES@19..20 "'"
                          TK_COMMA@20..21 ","
                          TWIG_EXPRESSION@21..59
                            TWIG_LITERAL_HASH@21..59
                              TK_WHITESPACE@21..22 " "
                              TK_OPEN_CURLY@22..23 "{"
                              TWIG_LITERAL_HASH_ITEMS@23..57
                                TWIG_LITERAL_HASH_PAIR@23..57
                                  TWIG_LITERAL_HASH_KEY@23..31
                                    TK_WHITESPACE@23..24 " "
                                    TK_WORD@24..31 "message"
                                  TK_COLON@31..32 ":"
                                  TWIG_EXPRESSION@32..57
                                    TWIG_LITERAL_STRING@32..57
                                      TK_WHITESPACE@32..33 " "
                                      TK_SINGLE_QUOTES@33..34 "'"
                                      TWIG_LITERAL_STRING_INNER@34..56
                                        TK_WORD@34..39 "Hello"
                                        TK_WHITESPACE@39..40 " "
                                        TK_WORD@40..44 "Twig"
                                        TK_WHITESPACE@44..45 " "
                                        TK_WORD@45..55 "Components"
                                        TK_EXCLAMATION_MARK@55..56 "!"
                                      TK_SINGLE_QUOTES@56..57 "'"
                              TK_WHITESPACE@57..58 " "
                              TK_CLOSE_CURLY@58..59 "}"
                          TK_CLOSE_PARENTHESIS@59..60 ")"
                    TK_WHITESPACE@60..61 " "
                    TK_CLOSE_CURLY_CURLY@61..63 "}}""#]],
        );
    }
}

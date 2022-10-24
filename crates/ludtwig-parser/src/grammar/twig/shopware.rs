use crate::grammar::twig::expression::parse_twig_expression;
use crate::grammar::twig::literal::parse_twig_string;
use crate::grammar::{parse_many, ParseFunction};
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
    child_parser: ParseFunction,
) -> BlockParseResult {
    // {% already consumed
    if parser.at(T!["sw_extends"]) {
        BlockParseResult::Successful(parse_twig_sw_extends(parser, outer))
    } else if parser.at(T!["sw_include"]) {
        BlockParseResult::Successful(parse_twig_sw_include(parser, outer))
    } else if parser.at(T!["sw_silent_feature_call"]) {
        BlockParseResult::Successful(parse_twig_sw_silent_feature_call(
            parser,
            outer,
            child_parser,
        ))
    } else {
        // error will be thrown by calling function
        BlockParseResult::NothingFound(outer)
    }
}

fn parse_twig_sw_silent_feature_call(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_silent_feature_call"]));
    parser.bump();
    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else {
        parser.add_error(ParseErrorBuilder::new(
            "twig string as feature flag (shopware doesn't allow expressions here)",
        ));
    }
    parser.expect(T!["%}"]);
    let wrapper_m = parser.complete(
        outer,
        SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL_STARTING_BLOCK,
    );
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endsw_silent_feature_call
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endsw_silent_feature_call"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"]);
    parser.expect(T!["endsw_silent_feature_call"]);
    parser.expect(T!["%}"]);
    parser.complete(
        end_block_m,
        SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL_ENDING_BLOCK,
    );

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL)
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

fn parse_twig_sw_include(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_include"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template name"));
    }

    if parser.at(T!["ignore missing"]) {
        parser.bump();
    }

    if parser.at(T!["with"]) {
        let with_value_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression as with value"));
        }
        parser.complete(with_value_m, SyntaxKind::TWIG_INCLUDE_WITH);
    }

    if parser.at(T!["only"]) {
        parser.bump();
    }

    parser.expect(T!["%}"]);

    parser.complete(outer, SyntaxKind::SHOPWARE_TWIG_SW_INCLUDE)
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

    #[test]
    fn parse_sw_include() {
        check_parse(
            "{% sw_include '@Storefront/storefront/layout/meta.html.twig' %}",
            expect![[r#"
                ROOT@0..63
                  SHOPWARE_TWIG_SW_INCLUDE@0..63
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_INCLUDE@3..13 "sw_include"
                    TWIG_EXPRESSION@13..60
                      TWIG_LITERAL_STRING@13..60
                        TK_WHITESPACE@13..14 " "
                        TK_SINGLE_QUOTES@14..15 "'"
                        TWIG_LITERAL_STRING_INNER@15..59
                          TK_WORD@15..26 "@Storefront"
                          TK_FORWARD_SLASH@26..27 "/"
                          TK_WORD@27..37 "storefront"
                          TK_FORWARD_SLASH@37..38 "/"
                          TK_WORD@38..44 "layout"
                          TK_FORWARD_SLASH@44..45 "/"
                          TK_WORD@45..49 "meta"
                          TK_DOT@49..50 "."
                          TK_WORD@50..54 "html"
                          TK_DOT@54..55 "."
                          TK_WORD@55..59 "twig"
                        TK_SINGLE_QUOTES@59..60 "'"
                    TK_WHITESPACE@60..61 " "
                    TK_PERCENT_CURLY@61..63 "%}""#]],
        )
    }

    #[test]
    fn parse_sw_include_with() {
        check_parse(
            r#"{% sw_include "@Storefront/storefront/utilities/alert.html.twig" with { 'type': message.type, 'content': message.text} %}"#,
            expect![[r#"
                ROOT@0..121
                  SHOPWARE_TWIG_SW_INCLUDE@0..121
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_INCLUDE@3..13 "sw_include"
                    TWIG_EXPRESSION@13..64
                      TWIG_LITERAL_STRING@13..64
                        TK_WHITESPACE@13..14 " "
                        TK_DOUBLE_QUOTES@14..15 "\""
                        TWIG_LITERAL_STRING_INNER@15..63
                          TK_WORD@15..26 "@Storefront"
                          TK_FORWARD_SLASH@26..27 "/"
                          TK_WORD@27..37 "storefront"
                          TK_FORWARD_SLASH@37..38 "/"
                          TK_WORD@38..47 "utilities"
                          TK_FORWARD_SLASH@47..48 "/"
                          TK_WORD@48..53 "alert"
                          TK_DOT@53..54 "."
                          TK_WORD@54..58 "html"
                          TK_DOT@58..59 "."
                          TK_WORD@59..63 "twig"
                        TK_DOUBLE_QUOTES@63..64 "\""
                    TWIG_INCLUDE_WITH@64..118
                      TK_WHITESPACE@64..65 " "
                      TK_WITH@65..69 "with"
                      TWIG_EXPRESSION@69..118
                        TWIG_LITERAL_HASH@69..118
                          TK_WHITESPACE@69..70 " "
                          TK_OPEN_CURLY@70..71 "{"
                          TWIG_LITERAL_HASH_PAIR@71..92
                            TWIG_LITERAL_HASH_KEY@71..78
                              TWIG_LITERAL_STRING@71..78
                                TK_WHITESPACE@71..72 " "
                                TK_SINGLE_QUOTES@72..73 "'"
                                TWIG_LITERAL_STRING_INNER@73..77
                                  TK_WORD@73..77 "type"
                                TK_SINGLE_QUOTES@77..78 "'"
                            TK_COLON@78..79 ":"
                            TWIG_EXPRESSION@79..92
                              TWIG_ACCESSOR@79..92
                                TWIG_OPERAND@79..87
                                  TWIG_LITERAL_NAME@79..87
                                    TK_WHITESPACE@79..80 " "
                                    TK_WORD@80..87 "message"
                                TK_DOT@87..88 "."
                                TWIG_OPERAND@88..92
                                  TWIG_LITERAL_NAME@88..92
                                    TK_WORD@88..92 "type"
                          TK_COMMA@92..93 ","
                          TWIG_LITERAL_HASH_PAIR@93..117
                            TWIG_LITERAL_HASH_KEY@93..103
                              TWIG_LITERAL_STRING@93..103
                                TK_WHITESPACE@93..94 " "
                                TK_SINGLE_QUOTES@94..95 "'"
                                TWIG_LITERAL_STRING_INNER@95..102
                                  TK_WORD@95..102 "content"
                                TK_SINGLE_QUOTES@102..103 "'"
                            TK_COLON@103..104 ":"
                            TWIG_EXPRESSION@104..117
                              TWIG_ACCESSOR@104..117
                                TWIG_OPERAND@104..112
                                  TWIG_LITERAL_NAME@104..112
                                    TK_WHITESPACE@104..105 " "
                                    TK_WORD@105..112 "message"
                                TK_DOT@112..113 "."
                                TWIG_OPERAND@113..117
                                  TWIG_LITERAL_NAME@113..117
                                    TK_WORD@113..117 "text"
                          TK_CLOSE_CURLY@117..118 "}"
                    TK_WHITESPACE@118..119 " "
                    TK_PERCENT_CURLY@119..121 "%}""#]],
        )
    }

    #[test]
    fn parse_sw_silent_feature_call() {
        check_parse(
            r#"{% sw_silent_feature_call "v6.5.0.0" %}
            {{ counter.incrementPage() }}
{% endsw_silent_feature_call %}"#,
            expect![[r#"
                ROOT@0..113
                  SHOPWARE_SILENT_FEATURE_CALL@0..113
                    SHOPWARE_SILENT_FEATURE_CALL_STARTING_BLOCK@0..39
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SW_SILENT_FEATURE_CALL@3..25 "sw_silent_feature_call"
                      TWIG_LITERAL_STRING@25..36
                        TK_WHITESPACE@25..26 " "
                        TK_DOUBLE_QUOTES@26..27 "\""
                        TWIG_LITERAL_STRING_INNER@27..35
                          TK_WORD@27..29 "v6"
                          TK_DOT@29..30 "."
                          TK_NUMBER@30..33 "5.0"
                          TK_DOT@33..34 "."
                          TK_NUMBER@34..35 "0"
                        TK_DOUBLE_QUOTES@35..36 "\""
                      TK_WHITESPACE@36..37 " "
                      TK_PERCENT_CURLY@37..39 "%}"
                    BODY@39..81
                      TWIG_VAR@39..81
                        TK_LINE_BREAK@39..40 "\n"
                        TK_WHITESPACE@40..52 "            "
                        TK_OPEN_CURLY_CURLY@52..54 "{{"
                        TWIG_EXPRESSION@54..78
                          TWIG_FUNCTION_CALL@54..78
                            TWIG_OPERAND@54..76
                              TWIG_ACCESSOR@54..76
                                TWIG_OPERAND@54..62
                                  TWIG_LITERAL_NAME@54..62
                                    TK_WHITESPACE@54..55 " "
                                    TK_WORD@55..62 "counter"
                                TK_DOT@62..63 "."
                                TWIG_OPERAND@63..76
                                  TWIG_LITERAL_NAME@63..76
                                    TK_WORD@63..76 "incrementPage"
                            TK_OPEN_PARENTHESIS@76..77 "("
                            TWIG_ARGUMENTS@77..77
                            TK_CLOSE_PARENTHESIS@77..78 ")"
                        TK_WHITESPACE@78..79 " "
                        TK_CLOSE_CURLY_CURLY@79..81 "}}"
                    SHOPWARE_SILENT_FEATURE_CALL_ENDING_BLOCK@81..113
                      TK_LINE_BREAK@81..82 "\n"
                      TK_CURLY_PERCENT@82..84 "{%"
                      TK_WHITESPACE@84..85 " "
                      TK_ENDSW_SILENT_FEATURE_CALL@85..110 "endsw_silent_feature_ ..."
                      TK_WHITESPACE@110..111 " "
                      TK_PERCENT_CURLY@111..113 "%}""#]],
        )
    }
}

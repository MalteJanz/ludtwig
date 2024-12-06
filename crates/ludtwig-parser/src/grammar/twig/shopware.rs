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
    } else if parser.at(T!["return"]) {
        BlockParseResult::Successful(parse_twig_sw_return(parser, outer))
    } else if parser.at(T!["sw_icon"]) {
        BlockParseResult::Successful(parse_twig_sw_icon(parser, outer))
    } else if parser.at(T!["sw_thumbnails"]) {
        BlockParseResult::Successful(parse_twig_sw_thumbnails(parser, outer))
    } else {
        // error will be thrown by calling function
        BlockParseResult::NothingFound(outer)
    }
}

fn parse_twig_sw_thumbnails(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_thumbnails"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as thumbnail name"));
        parser.recover(&[T!["with"], T!["%}"]]);
    }

    if parser.at(T!["with"]) {
        let style_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new(
                "twig expression as thumbnail variables",
            ));
            parser.recover(&[T!["with"], T!["%}"]]);
        }
        parser.complete(style_m, SyntaxKind::SHOPWARE_THUMBNAILS_WITH);
    }

    parser.expect(T!["%}"], &[]);
    parser.complete(outer, SyntaxKind::SHOPWARE_THUMBNAILS)
}

fn parse_twig_sw_icon(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_icon"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as icon name"));
        parser.recover(&[T!["style"], T!["%}"]]);
    }

    if parser.at(T!["style"]) {
        let style_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new(
                "twig expression as icon style variables",
            ));
            parser.recover(&[T!["%}"]]);
        }
        parser.complete(style_m, SyntaxKind::SHOPWARE_ICON_STYLE);
    }

    parser.expect(T!["%}"], &[]);
    parser.complete(outer, SyntaxKind::SHOPWARE_ICON)
}

fn parse_twig_sw_return(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["return"]));
    parser.bump();

    parse_twig_expression(parser);

    parser.expect(T!["%}"], &[]);
    parser.complete(outer, SyntaxKind::SHOPWARE_RETURN)
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
        parser.recover(&[T!["endsw_silent_feature_call"], T!["%}"]]);
    }
    parser.expect(T!["%}"], &[T!["endsw_silent_feature_call"], T!["%}"]]);
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
    parser.expect(T!["{%"], &[T!["endsw_silent_feature_call"], T!["%}"]]);
    parser.expect(T!["endsw_silent_feature_call"], &[T!["%}"]]);
    parser.expect(T!["%}"], &[]);
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
        parser.recover(&[T!["%}"]]);
    }

    parser.expect(T!["%}"], &[]);
    parser.complete(outer, SyntaxKind::SHOPWARE_TWIG_SW_EXTENDS)
}

fn parse_twig_sw_include(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["sw_include"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template name"));
        parser.recover(&[T!["ignore missing"], T!["with"], T!["only"], T!["%}"]]);
    }

    if parser.at(T!["ignore missing"]) {
        parser.bump();
    }

    if parser.at(T!["with"]) {
        let with_value_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression as with value"));
            parser.recover(&[T!["only"], T!["%}"]]);
        }
        parser.complete(with_value_m, SyntaxKind::TWIG_INCLUDE_WITH);
    }

    if parser.at(T!["only"]) {
        parser.bump();
    }

    parser.expect(T!["%}"], &[]);

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
        );
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
        );
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
                          TWIG_LITERAL_HASH_ITEMS@71..117
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
        );
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
                            TWIG_ARGUMENTS@76..78
                              TK_OPEN_PARENTHESIS@76..77 "("
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
        );
    }

    #[test]
    fn parse_shopware_return() {
        check_parse(
            r#"{% return %}"#,
            expect![[r#"
            ROOT@0..12
              SHOPWARE_RETURN@0..12
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_RETURN@3..9 "return"
                TK_WHITESPACE@9..10 " "
                TK_PERCENT_CURLY@10..12 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_return_value() {
        check_parse(
            r#"{% return 5 %}"#,
            expect![[r#"
            ROOT@0..14
              SHOPWARE_RETURN@0..14
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_RETURN@3..9 "return"
                TWIG_EXPRESSION@9..11
                  TWIG_LITERAL_NUMBER@9..11
                    TK_WHITESPACE@9..10 " "
                    TK_NUMBER@10..11 "5"
                TK_WHITESPACE@11..12 " "
                TK_PERCENT_CURLY@12..14 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_return_expression() {
        check_parse(
            r#"{% return not compare('=', 'foo', test) %}"#,
            expect![[r#"
                ROOT@0..42
                  SHOPWARE_RETURN@0..42
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_RETURN@3..9 "return"
                    TWIG_EXPRESSION@9..39
                      TWIG_UNARY_EXPRESSION@9..39
                        TK_WHITESPACE@9..10 " "
                        TK_NOT@10..13 "not"
                        TWIG_EXPRESSION@13..39
                          TWIG_FUNCTION_CALL@13..39
                            TWIG_OPERAND@13..21
                              TWIG_LITERAL_NAME@13..21
                                TK_WHITESPACE@13..14 " "
                                TK_WORD@14..21 "compare"
                            TWIG_ARGUMENTS@21..39
                              TK_OPEN_PARENTHESIS@21..22 "("
                              TWIG_EXPRESSION@22..25
                                TWIG_LITERAL_STRING@22..25
                                  TK_SINGLE_QUOTES@22..23 "'"
                                  TWIG_LITERAL_STRING_INNER@23..24
                                    TK_EQUAL@23..24 "="
                                  TK_SINGLE_QUOTES@24..25 "'"
                              TK_COMMA@25..26 ","
                              TWIG_EXPRESSION@26..32
                                TWIG_LITERAL_STRING@26..32
                                  TK_WHITESPACE@26..27 " "
                                  TK_SINGLE_QUOTES@27..28 "'"
                                  TWIG_LITERAL_STRING_INNER@28..31
                                    TK_WORD@28..31 "foo"
                                  TK_SINGLE_QUOTES@31..32 "'"
                              TK_COMMA@32..33 ","
                              TWIG_EXPRESSION@33..38
                                TWIG_LITERAL_NAME@33..38
                                  TK_WHITESPACE@33..34 " "
                                  TK_WORD@34..38 "test"
                              TK_CLOSE_PARENTHESIS@38..39 ")"
                    TK_WHITESPACE@39..40 " "
                    TK_PERCENT_CURLY@40..42 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_icon() {
        check_parse(
            r#"{% sw_icon 'clock' %}"#,
            expect![[r#"
            ROOT@0..21
              SHOPWARE_ICON@0..21
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_SW_ICON@3..10 "sw_icon"
                TWIG_EXPRESSION@10..18
                  TWIG_LITERAL_STRING@10..18
                    TK_WHITESPACE@10..11 " "
                    TK_SINGLE_QUOTES@11..12 "'"
                    TWIG_LITERAL_STRING_INNER@12..17
                      TK_WORD@12..17 "clock"
                    TK_SINGLE_QUOTES@17..18 "'"
                TK_WHITESPACE@18..19 " "
                TK_PERCENT_CURLY@19..21 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_icon_with_style() {
        check_parse(
            r#"{% sw_icon 'minus' style { 'size': 'xs' } %}"#,
            expect![[r#"
                ROOT@0..44
                  SHOPWARE_ICON@0..44
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_ICON@3..10 "sw_icon"
                    TWIG_EXPRESSION@10..18
                      TWIG_LITERAL_STRING@10..18
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..17
                          TK_WORD@12..17 "minus"
                        TK_SINGLE_QUOTES@17..18 "'"
                    SHOPWARE_ICON_STYLE@18..41
                      TK_WHITESPACE@18..19 " "
                      TK_STYLE@19..24 "style"
                      TWIG_EXPRESSION@24..41
                        TWIG_LITERAL_HASH@24..41
                          TK_WHITESPACE@24..25 " "
                          TK_OPEN_CURLY@25..26 "{"
                          TWIG_LITERAL_HASH_ITEMS@26..39
                            TWIG_LITERAL_HASH_PAIR@26..39
                              TWIG_LITERAL_HASH_KEY@26..33
                                TWIG_LITERAL_STRING@26..33
                                  TK_WHITESPACE@26..27 " "
                                  TK_SINGLE_QUOTES@27..28 "'"
                                  TWIG_LITERAL_STRING_INNER@28..32
                                    TK_WORD@28..32 "size"
                                  TK_SINGLE_QUOTES@32..33 "'"
                              TK_COLON@33..34 ":"
                              TWIG_EXPRESSION@34..39
                                TWIG_LITERAL_STRING@34..39
                                  TK_WHITESPACE@34..35 " "
                                  TK_SINGLE_QUOTES@35..36 "'"
                                  TWIG_LITERAL_STRING_INNER@36..38
                                    TK_WORD@36..38 "xs"
                                  TK_SINGLE_QUOTES@38..39 "'"
                          TK_WHITESPACE@39..40 " "
                          TK_CLOSE_CURLY@40..41 "}"
                    TK_WHITESPACE@41..42 " "
                    TK_PERCENT_CURLY@42..44 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_thumbnails() {
        check_parse(
            r#"{% sw_thumbnails 'cart-item-img-thumbnails' %}"#,
            expect![[r#"
                ROOT@0..46
                  SHOPWARE_THUMBNAILS@0..46
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_THUMBNAILS@3..16 "sw_thumbnails"
                    TWIG_EXPRESSION@16..43
                      TWIG_LITERAL_STRING@16..43
                        TK_WHITESPACE@16..17 " "
                        TK_SINGLE_QUOTES@17..18 "'"
                        TWIG_LITERAL_STRING_INNER@18..42
                          TK_WORD@18..42 "cart-item-img-thumbnails"
                        TK_SINGLE_QUOTES@42..43 "'"
                    TK_WHITESPACE@43..44 " "
                    TK_PERCENT_CURLY@44..46 "%}""#]],
        );
    }

    #[test]
    fn parse_shopware_thumbnails_with() {
        check_parse(
            r#"{% sw_thumbnails 'product-image-thumbnails' with { media: product.cover.media } %}"#,
            expect![[r#"
                ROOT@0..82
                  SHOPWARE_THUMBNAILS@0..82
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_SW_THUMBNAILS@3..16 "sw_thumbnails"
                    TWIG_EXPRESSION@16..43
                      TWIG_LITERAL_STRING@16..43
                        TK_WHITESPACE@16..17 " "
                        TK_SINGLE_QUOTES@17..18 "'"
                        TWIG_LITERAL_STRING_INNER@18..42
                          TK_WORD@18..42 "product-image-thumbnails"
                        TK_SINGLE_QUOTES@42..43 "'"
                    SHOPWARE_THUMBNAILS_WITH@43..79
                      TK_WHITESPACE@43..44 " "
                      TK_WITH@44..48 "with"
                      TWIG_EXPRESSION@48..79
                        TWIG_LITERAL_HASH@48..79
                          TK_WHITESPACE@48..49 " "
                          TK_OPEN_CURLY@49..50 "{"
                          TWIG_LITERAL_HASH_ITEMS@50..77
                            TWIG_LITERAL_HASH_PAIR@50..77
                              TWIG_LITERAL_HASH_KEY@50..56
                                TK_WHITESPACE@50..51 " "
                                TK_WORD@51..56 "media"
                              TK_COLON@56..57 ":"
                              TWIG_EXPRESSION@57..77
                                TWIG_ACCESSOR@57..77
                                  TWIG_OPERAND@57..71
                                    TWIG_ACCESSOR@57..71
                                      TWIG_OPERAND@57..65
                                        TWIG_LITERAL_NAME@57..65
                                          TK_WHITESPACE@57..58 " "
                                          TK_WORD@58..65 "product"
                                      TK_DOT@65..66 "."
                                      TWIG_OPERAND@66..71
                                        TWIG_LITERAL_NAME@66..71
                                          TK_WORD@66..71 "cover"
                                  TK_DOT@71..72 "."
                                  TWIG_OPERAND@72..77
                                    TWIG_LITERAL_NAME@72..77
                                      TK_WORD@72..77 "media"
                          TK_WHITESPACE@77..78 " "
                          TK_CLOSE_CURLY@78..79 "}"
                    TK_WHITESPACE@79..80 " "
                    TK_PERCENT_CURLY@80..82 "%}""#]],
        );
    }
}

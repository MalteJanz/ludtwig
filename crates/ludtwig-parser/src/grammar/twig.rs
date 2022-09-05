use crate::grammar::parse_any_element;
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_any_twig(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["{%"]) {
        parse_twig_block_statement(parser)
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

    loop {
        if parser.at_end() || parser.at(T!["#}"]) {
            break;
        }

        parser.bump();
    }

    parser.expect(T!["#}"]);
    parser.complete(m, SyntaxKind::TWIG_COMMENT)
}

fn parse_twig_var_statement(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["{{"]));
    let m = parser.start();
    parser.bump();

    loop {
        if parser.at_end() || parser.at(T!["}}"]) {
            break;
        }

        parser.bump();
    }

    parser.expect(T!["}}"]);
    parser.complete(m, SyntaxKind::TWIG_VAR)
}

fn parse_twig_block_statement(parser: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(parser.at(T!["{%"]));
    let m = parser.start();
    parser.bump();

    if parser.at(T!["block"]) {
        Some(parse_twig_block(parser, m))
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

    // parse all the children except endblock
    let body_m = parser.start();
    loop {
        if parser.at_following(&[T!["{%"], T!["endblock"]]) {
            break;
        }
        if parse_any_element(parser).is_none() {
            break;
        };
    }
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"]);
    parser.expect(T!["endblock"]);
    parser.expect(T!["%}"]);
    parser.complete(end_block_m, SyntaxKind::TWIG_ENDING_BLOCK);

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::TWIG_BLOCK)
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
                      HTML_TEXT@23..35
                        TK_WORD@23..28 "hello"
                        TK_WHITESPACE@28..29 " "
                        TK_WORD@29..34 "world"
                        TK_WHITESPACE@34..35 " "
                    TWIG_ENDING_BLOCK@35..49
                      TK_CURLY_PERCENT@35..37 "{%"
                      TK_WHITESPACE@37..38 " "
                      TK_ENDBLOCK@38..46 "endblock"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}"
                parsing consumed all tokens: true"#]],
        );
    }

    #[test]
    fn parse_nested_twig_blocks() {
        check_parse(
            "{% block outer %}\
                    out\
                    {% block middle %}\
                        mid\
                        {% block inner %}\
                        in\
                        {% endblock %}\
                        mid\
                    {% endblock %}\
                    out\
                    {% endblock %}",
            expect![[r#"
                ROOT@0..108
                  TWIG_BLOCK@0..108
                    TWIG_STARTING_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "outer"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                    BODY@17..94
                      HTML_TEXT@17..20
                        TK_WORD@17..20 "out"
                      TWIG_BLOCK@20..91
                        TWIG_STARTING_BLOCK@20..38
                          TK_CURLY_PERCENT@20..22 "{%"
                          TK_WHITESPACE@22..23 " "
                          TK_BLOCK@23..28 "block"
                          TK_WHITESPACE@28..29 " "
                          TK_WORD@29..35 "middle"
                          TK_WHITESPACE@35..36 " "
                          TK_PERCENT_CURLY@36..38 "%}"
                        BODY@38..77
                          HTML_TEXT@38..41
                            TK_WORD@38..41 "mid"
                          TWIG_BLOCK@41..74
                            TWIG_STARTING_BLOCK@41..58
                              TK_CURLY_PERCENT@41..43 "{%"
                              TK_WHITESPACE@43..44 " "
                              TK_BLOCK@44..49 "block"
                              TK_WHITESPACE@49..50 " "
                              TK_WORD@50..55 "inner"
                              TK_WHITESPACE@55..56 " "
                              TK_PERCENT_CURLY@56..58 "%}"
                            BODY@58..60
                              HTML_TEXT@58..60
                                TK_WORD@58..60 "in"
                            TWIG_ENDING_BLOCK@60..74
                              TK_CURLY_PERCENT@60..62 "{%"
                              TK_WHITESPACE@62..63 " "
                              TK_ENDBLOCK@63..71 "endblock"
                              TK_WHITESPACE@71..72 " "
                              TK_PERCENT_CURLY@72..74 "%}"
                          HTML_TEXT@74..77
                            TK_WORD@74..77 "mid"
                        TWIG_ENDING_BLOCK@77..91
                          TK_CURLY_PERCENT@77..79 "{%"
                          TK_WHITESPACE@79..80 " "
                          TK_ENDBLOCK@80..88 "endblock"
                          TK_WHITESPACE@88..89 " "
                          TK_PERCENT_CURLY@89..91 "%}"
                      HTML_TEXT@91..94
                        TK_WORD@91..94 "out"
                    TWIG_ENDING_BLOCK@94..108
                      TK_CURLY_PERCENT@94..96 "{%"
                      TK_WHITESPACE@96..97 " "
                      TK_ENDBLOCK@97..105 "endblock"
                      TK_WHITESPACE@105..106 " "
                      TK_PERCENT_CURLY@106..108 "%}"
                parsing consumed all tokens: true"#]],
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
                parsing consumed all tokens: true
                error at 3..3: expected block, but found word"#]],
        )
    }

    #[test]
    fn parse_twig_var() {
        check_parse(
            "{{ something }} plain {{ else }}",
            expect![[r#"
                ROOT@0..32
                  TWIG_VAR@0..16
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TK_WHITESPACE@2..3 " "
                    TK_WORD@3..12 "something"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}"
                    TK_WHITESPACE@15..16 " "
                  HTML_TEXT@16..22
                    TK_WORD@16..21 "plain"
                    TK_WHITESPACE@21..22 " "
                  TWIG_VAR@22..32
                    TK_OPEN_CURLY_CURLY@22..24 "{{"
                    TK_WHITESPACE@24..25 " "
                    TK_WORD@25..29 "else"
                    TK_WHITESPACE@29..30 " "
                    TK_CLOSE_CURLY_CURLY@30..32 "}}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_comment() {
        check_parse(
            "{# something #} plain {# {{ comment }} {% block asdf %} #}",
            expect![[r##"
                ROOT@0..58
                  TWIG_COMMENT@0..16
                    TK_OPEN_CURLY_HASHTAG@0..2 "{#"
                    TK_WHITESPACE@2..3 " "
                    TK_WORD@3..12 "something"
                    TK_WHITESPACE@12..13 " "
                    TK_HASHTAG_CLOSE_CURLY@13..15 "#}"
                    TK_WHITESPACE@15..16 " "
                  HTML_TEXT@16..22
                    TK_WORD@16..21 "plain"
                    TK_WHITESPACE@21..22 " "
                  TWIG_COMMENT@22..58
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
                    TK_HASHTAG_CLOSE_CURLY@56..58 "#}"
                parsing consumed all tokens: true"##]],
        )
    }
}

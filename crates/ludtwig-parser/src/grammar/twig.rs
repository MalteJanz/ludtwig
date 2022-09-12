use crate::grammar::ParseFunction;
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_any_twig(
    parser: &mut Parser,
    child_parser: ParseFunction,
) -> Option<CompletedMarker> {
    if parser.at(T!["{%"]) {
        parse_twig_block_statement(parser, child_parser)
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

fn parse_twig_block_statement(
    parser: &mut Parser,
    child_parser: ParseFunction,
) -> Option<CompletedMarker> {
    debug_assert!(parser.at(T!["{%"]));
    let m = parser.start();
    parser.bump();

    if parser.at(T!["block"]) {
        Some(parse_twig_block(parser, m, child_parser))
    } else if parser.at(T!["if"]) {
        Some(parse_twig_if(parser, m, child_parser))
    } else {
        // TODO: implement other twig block statements like if, for, and so on
        parser.error();
        parser.complete(m, SyntaxKind::ERROR);
        None
    }
}

fn parse_twig_block(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
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
        if child_parser(parser).is_none() {
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

fn parse_twig_if(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["if"]));

    parser.expect(T!["if"]);
    parse_twig_condition_expression(parser);
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_IF_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse branches
    loop {
        // parse body (all the children)
        let body_m = parser.start();
        loop {
            if parser.at_following(&[T!["{%"], T!["endif"]])
                || parser.at_following(&[T!["{%"], T!["elseif"]])
                || parser.at_following(&[T!["{%"], T!["else"]])
            {
                break;
            }
            if child_parser(parser).is_none() {
                break;
            };
        }
        parser.complete(body_m, SyntaxKind::BODY);

        if parser.at_following(&[T!["{%"], T!["endif"]]) {
            break; // no more branches
        }

        // parse next branch header
        if parser.at_following(&[T!["{%"], T!["elseif"]]) {
            let branch_m = parser.start();
            parser.bump();
            parser.bump();
            parse_twig_condition_expression(parser);
            parser.expect(T!["%}"]);
            parser.complete(branch_m, SyntaxKind::TWIG_ELSE_IF_BLOCK);
        } else if parser.at_following(&[T!["{%"], T!["else"]]) {
            let branch_m = parser.start();
            parser.bump();
            parser.bump();
            parser.expect(T!["%}"]);
            parser.complete(branch_m, SyntaxKind::TWIG_ELSE_BLOCK);
        } else {
            // not an actual branch found, the child parser has ended
            break;
        }
    }

    let end_block_m = parser.start();
    parser.expect(T!["{%"]);
    parser.expect(T!["endif"]);
    parser.expect(T!["%}"]);
    parser.complete(end_block_m, SyntaxKind::TWIG_ENDIF_BLOCK);

    parser.complete(wrapper_m, SyntaxKind::TWIG_IF)
}

fn parse_twig_condition_expression(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();

    loop {
        if parser.at_end() || parser.at(T!["%}"]) {
            break;
        }

        parser.bump();
    }

    parser.complete(m, SyntaxKind::TWIG_CONDITION_EXPRESSION)
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
                    TWIG_STARTING_BLOCK@0..22
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..19 "block_name"
                      TK_WHITESPACE@19..20 " "
                      TK_PERCENT_CURLY@20..22 "%}"
                    BODY@22..34
                      HTML_TEXT@22..34
                        TK_WHITESPACE@22..23 " "
                        TK_WORD@23..28 "hello"
                        TK_WHITESPACE@28..29 " "
                        TK_WORD@29..34 "world"
                    TWIG_ENDING_BLOCK@34..49
                      TK_WHITESPACE@34..35 " "
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
                    ERROR@2..7
                      TK_WHITESPACE@2..3 " "
                      TK_WORD@3..7 "asdf"
                parsing consumed all tokens: true
                error at 3..3: expected block or if, but found word"#]],
        )
    }

    #[test]
    fn parse_twig_var() {
        check_parse(
            "{{ something }} plain {{ else }}",
            expect![[r#"
                ROOT@0..32
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
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
                    TK_WHITESPACE@24..25 " "
                    TK_ELSE@25..29 "else"
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
                    TK_HASHTAG_CLOSE_CURLY@56..58 "#}"
                parsing consumed all tokens: true"##]],
        )
    }

    #[test]
    fn parse_twig_if() {
        check_parse(
            "{% if isTrue %} true {% endif %}",
            expect![[r#"
                ROOT@0..32
                  TWIG_IF@0..32
                    TWIG_IF_BLOCK@0..15
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..12
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..12 "isTrue"
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                    BODY@15..20
                      HTML_TEXT@15..20
                        TK_WHITESPACE@15..16 " "
                        TK_WORD@16..20 "true"
                    TWIG_ENDIF_BLOCK@20..32
                      TK_WHITESPACE@20..21 " "
                      TK_CURLY_PERCENT@21..23 "{%"
                      TK_WHITESPACE@23..24 " "
                      TK_ENDIF@24..29 "endif"
                      TK_WHITESPACE@29..30 " "
                      TK_PERCENT_CURLY@30..32 "%}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_if_condition_expression() {
        check_parse(
            "{% if temperature > 18 and temperature < 27 %} true {% endif %}",
            expect![[r#"
                ROOT@0..63
                  TWIG_IF@0..63
                    TWIG_IF_BLOCK@0..46
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..43
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..17 "temperature"
                        TK_WHITESPACE@17..18 " "
                        TK_GREATER_THAN@18..19 ">"
                        TK_WHITESPACE@19..20 " "
                        TK_WORD@20..22 "18"
                        TK_WHITESPACE@22..23 " "
                        TK_WORD@23..26 "and"
                        TK_WHITESPACE@26..27 " "
                        TK_WORD@27..38 "temperature"
                        TK_WHITESPACE@38..39 " "
                        TK_LESS_THAN@39..40 "<"
                        TK_WHITESPACE@40..41 " "
                        TK_WORD@41..43 "27"
                      TK_WHITESPACE@43..44 " "
                      TK_PERCENT_CURLY@44..46 "%}"
                    BODY@46..51
                      HTML_TEXT@46..51
                        TK_WHITESPACE@46..47 " "
                        TK_WORD@47..51 "true"
                    TWIG_ENDIF_BLOCK@51..63
                      TK_WHITESPACE@51..52 " "
                      TK_CURLY_PERCENT@52..54 "{%"
                      TK_WHITESPACE@54..55 " "
                      TK_ENDIF@55..60 "endif"
                      TK_WHITESPACE@60..61 " "
                      TK_PERCENT_CURLY@61..63 "%}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_if_else() {
        check_parse(
            "{% if isTrue %} true {% else %} false {% endif %}",
            expect![[r#"
                ROOT@0..49
                  TWIG_IF@0..49
                    TWIG_IF_BLOCK@0..15
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..12
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..12 "isTrue"
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                    BODY@15..20
                      HTML_TEXT@15..20
                        TK_WHITESPACE@15..16 " "
                        TK_WORD@16..20 "true"
                    TWIG_ELSE_BLOCK@20..31
                      TK_WHITESPACE@20..21 " "
                      TK_CURLY_PERCENT@21..23 "{%"
                      TK_WHITESPACE@23..24 " "
                      TK_ELSE@24..28 "else"
                      TK_WHITESPACE@28..29 " "
                      TK_PERCENT_CURLY@29..31 "%}"
                    BODY@31..37
                      HTML_TEXT@31..37
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..37 "false"
                    TWIG_ENDIF_BLOCK@37..49
                      TK_WHITESPACE@37..38 " "
                      TK_CURLY_PERCENT@38..40 "{%"
                      TK_WHITESPACE@40..41 " "
                      TK_ENDIF@41..46 "endif"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_if_elseif() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% endif %}",
            expect![[r#"
                ROOT@0..45
                  TWIG_IF@0..45
                    TWIG_IF_BLOCK@0..12
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..9
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..9 "isA"
                      TK_WHITESPACE@9..10 " "
                      TK_PERCENT_CURLY@10..12 "%}"
                    BODY@12..14
                      HTML_TEXT@12..14
                        TK_WHITESPACE@12..13 " "
                        TK_WORD@13..14 "A"
                    TWIG_ELSE_IF_BLOCK@14..31
                      TK_WHITESPACE@14..15 " "
                      TK_CURLY_PERCENT@15..17 "{%"
                      TK_WHITESPACE@17..18 " "
                      TK_ELSE_IF@18..24 "elseif"
                      TWIG_CONDITION_EXPRESSION@24..28
                        TK_WHITESPACE@24..25 " "
                        TK_WORD@25..28 "isB"
                      TK_WHITESPACE@28..29 " "
                      TK_PERCENT_CURLY@29..31 "%}"
                    BODY@31..33
                      HTML_TEXT@31..33
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..33 "B"
                    TWIG_ENDIF_BLOCK@33..45
                      TK_WHITESPACE@33..34 " "
                      TK_CURLY_PERCENT@34..36 "{%"
                      TK_WHITESPACE@36..37 " "
                      TK_ENDIF@37..42 "endif"
                      TK_WHITESPACE@42..43 " "
                      TK_PERCENT_CURLY@43..45 "%}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_if_elseif_else() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% else %} other {% endif %}",
            expect![[r#"
                ROOT@0..62
                  TWIG_IF@0..62
                    TWIG_IF_BLOCK@0..12
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..9
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..9 "isA"
                      TK_WHITESPACE@9..10 " "
                      TK_PERCENT_CURLY@10..12 "%}"
                    BODY@12..14
                      HTML_TEXT@12..14
                        TK_WHITESPACE@12..13 " "
                        TK_WORD@13..14 "A"
                    TWIG_ELSE_IF_BLOCK@14..31
                      TK_WHITESPACE@14..15 " "
                      TK_CURLY_PERCENT@15..17 "{%"
                      TK_WHITESPACE@17..18 " "
                      TK_ELSE_IF@18..24 "elseif"
                      TWIG_CONDITION_EXPRESSION@24..28
                        TK_WHITESPACE@24..25 " "
                        TK_WORD@25..28 "isB"
                      TK_WHITESPACE@28..29 " "
                      TK_PERCENT_CURLY@29..31 "%}"
                    BODY@31..33
                      HTML_TEXT@31..33
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..33 "B"
                    TWIG_ELSE_BLOCK@33..44
                      TK_WHITESPACE@33..34 " "
                      TK_CURLY_PERCENT@34..36 "{%"
                      TK_WHITESPACE@36..37 " "
                      TK_ELSE@37..41 "else"
                      TK_WHITESPACE@41..42 " "
                      TK_PERCENT_CURLY@42..44 "%}"
                    BODY@44..50
                      HTML_TEXT@44..50
                        TK_WHITESPACE@44..45 " "
                        TK_WORD@45..50 "other"
                    TWIG_ENDIF_BLOCK@50..62
                      TK_WHITESPACE@50..51 " "
                      TK_CURLY_PERCENT@51..53 "{%"
                      TK_WHITESPACE@53..54 " "
                      TK_ENDIF@54..59 "endif"
                      TK_WHITESPACE@59..60 " "
                      TK_PERCENT_CURLY@60..62 "%}"
                parsing consumed all tokens: true"#]],
        )
    }

    #[test]
    fn parse_twig_if_elseif_elseif_else() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% elseif isC %} C {% else %} other {% endif %}",
            expect![[r#"
                ROOT@0..81
                  TWIG_IF@0..81
                    TWIG_IF_BLOCK@0..12
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_CONDITION_EXPRESSION@5..9
                        TK_WHITESPACE@5..6 " "
                        TK_WORD@6..9 "isA"
                      TK_WHITESPACE@9..10 " "
                      TK_PERCENT_CURLY@10..12 "%}"
                    BODY@12..14
                      HTML_TEXT@12..14
                        TK_WHITESPACE@12..13 " "
                        TK_WORD@13..14 "A"
                    TWIG_ELSE_IF_BLOCK@14..31
                      TK_WHITESPACE@14..15 " "
                      TK_CURLY_PERCENT@15..17 "{%"
                      TK_WHITESPACE@17..18 " "
                      TK_ELSE_IF@18..24 "elseif"
                      TWIG_CONDITION_EXPRESSION@24..28
                        TK_WHITESPACE@24..25 " "
                        TK_WORD@25..28 "isB"
                      TK_WHITESPACE@28..29 " "
                      TK_PERCENT_CURLY@29..31 "%}"
                    BODY@31..33
                      HTML_TEXT@31..33
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..33 "B"
                    TWIG_ELSE_IF_BLOCK@33..50
                      TK_WHITESPACE@33..34 " "
                      TK_CURLY_PERCENT@34..36 "{%"
                      TK_WHITESPACE@36..37 " "
                      TK_ELSE_IF@37..43 "elseif"
                      TWIG_CONDITION_EXPRESSION@43..47
                        TK_WHITESPACE@43..44 " "
                        TK_WORD@44..47 "isC"
                      TK_WHITESPACE@47..48 " "
                      TK_PERCENT_CURLY@48..50 "%}"
                    BODY@50..52
                      HTML_TEXT@50..52
                        TK_WHITESPACE@50..51 " "
                        TK_WORD@51..52 "C"
                    TWIG_ELSE_BLOCK@52..63
                      TK_WHITESPACE@52..53 " "
                      TK_CURLY_PERCENT@53..55 "{%"
                      TK_WHITESPACE@55..56 " "
                      TK_ELSE@56..60 "else"
                      TK_WHITESPACE@60..61 " "
                      TK_PERCENT_CURLY@61..63 "%}"
                    BODY@63..69
                      HTML_TEXT@63..69
                        TK_WHITESPACE@63..64 " "
                        TK_WORD@64..69 "other"
                    TWIG_ENDIF_BLOCK@69..81
                      TK_WHITESPACE@69..70 " "
                      TK_CURLY_PERCENT@70..72 "{%"
                      TK_WHITESPACE@72..73 " "
                      TK_ENDIF@73..78 "endif"
                      TK_WHITESPACE@78..79 " "
                      TK_PERCENT_CURLY@79..81 "%}"
                parsing consumed all tokens: true"#]],
        )
    }
}

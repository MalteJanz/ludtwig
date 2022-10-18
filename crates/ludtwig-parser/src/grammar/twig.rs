mod expression;
pub(crate) mod literal;

use crate::grammar::twig::expression::parse_twig_expression;
use crate::grammar::twig::literal::parse_twig_name;
use crate::grammar::{parse_ludtwig_directive, parse_many, ParseFunction};
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser};
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

    parser.expect(T!["#}"]);
    parser.complete(outer, SyntaxKind::TWIG_COMMENT)
}

fn parse_twig_var_statement(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["{{"]));
    let m = parser.start();
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"))
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
    } else if parser.at(T!["set"]) {
        Some(parse_twig_set(parser, m, child_parser))
    } else {
        // TODO: implement other twig block statements like if, for, and so on
        parser.add_error(ParseErrorBuilder::new(
            "'block' or 'if' (nothing else supported yet)".to_string(),
        ));

        parser.complete(m, SyntaxKind::ERROR);
        None
    }
}

fn parse_twig_set(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["set"]));
    parser.bump();

    // parse any amount of words seperated by comma
    let assignment_m = parser.start();
    let mut declaration_count = 0;
    parse_many(
        parser,
        |p| p.at_set(&[T!["="], T!["%}"]]),
        |p| {
            if parse_twig_name(p).is_some() {
                declaration_count += 1;
            } else {
                p.add_error(ParseErrorBuilder::new("twig variable name"));
            }

            if p.at(T![","]) {
                p.bump();
            }
        },
    );
    if declaration_count == 0 {
        parser.add_error(ParseErrorBuilder::new("twig variable name"));
    }

    // check for equal assignment
    let mut is_assigned_by_equal = false;
    if parser.at(T!["="]) {
        parser.bump();
        is_assigned_by_equal = true;

        let mut assignment_count = 0;
        parse_many(
            parser,
            |p| p.at(T!["%}"]),
            |p| {
                if parse_twig_expression(p).is_some() {
                    assignment_count += 1;
                } else {
                    p.add_error(ParseErrorBuilder::new("twig expression"));
                }

                if p.at(T![","]) {
                    p.bump();
                }
            },
        );

        if declaration_count != assignment_count {
            parser.add_error(ParseErrorBuilder::new(format!(
                "a total of {} twig expressions (same amount as declarations) instead of {}",
                declaration_count, assignment_count
            )));
        }
    } else if declaration_count > 1 {
        parser.add_error(ParseErrorBuilder::new(format!(
            "= followed by {} twig expressions",
            declaration_count
        )));
    }

    parser.complete(assignment_m, SyntaxKind::TWIG_ASSIGNMENT);
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_SET_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    if !is_assigned_by_equal {
        // children and a closing endset should be there

        // parse all the children except endset
        let body_m = parser.start();
        parse_many(
            parser,
            |p| p.at_following(&[T!["{%"], T!["endset"]]),
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);

        let end_block_m = parser.start();
        parser.expect(T!["{%"]);
        parser.expect(T!["endset"]);
        parser.expect(T!["%}"]);
        parser.complete(end_block_m, SyntaxKind::TWIG_ENDSET_BLOCK);
    }

    // close overall twig set
    parser.complete(wrapper_m, SyntaxKind::TWIG_SET)
}

fn parse_twig_block(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["block"]));
    parser.bump();
    parser.expect(T![word]);
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endblock
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endblock"]]),
        |p| {
            child_parser(p);
        },
    );
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
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"))
    }
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_IF_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse branches
    loop {
        // parse body (all the children)
        let body_m = parser.start();
        parse_many(
            parser,
            |p| {
                p.at_following(&[T!["{%"], T!["endif"]])
                    || p.at_following(&[T!["{%"], T!["elseif"]])
                    || p.at_following(&[T!["{%"], T!["else"]])
            },
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);

        if parser.at_following(&[T!["{%"], T!["endif"]]) {
            break; // no more branches
        }

        // parse next branch header
        if parser.at_following(&[T!["{%"], T!["elseif"]]) {
            let branch_m = parser.start();
            parser.bump();
            parser.bump();
            if parse_twig_expression(parser).is_none() {
                parser.add_error(ParseErrorBuilder::new("twig expression"))
            }
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;

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
                      TK_PERCENT_CURLY@47..49 "%}""#]],
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
                                TK_IN@58..60 "in"
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
                      TK_PERCENT_CURLY@106..108 "%}""#]],
        );
    }

    #[test]
    fn parse_error() {
        check_parse(
            "{% asdf",
            expect![[r#"
                ROOT@0..7
                  ERROR@0..2
                    TK_CURLY_PERCENT@0..2 "{%"
                  HTML_TEXT@2..7
                    TK_WHITESPACE@2..3 " "
                    TK_WORD@3..7 "asdf"
                error at 3..7: expected 'block' or 'if' (nothing else supported yet) but found word"#]],
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
                      TWIG_EXPRESSION@5..12
                        TWIG_LITERAL_NAME@5..12
                          TK_WHITESPACE@5..6 " "
                          TK_WORD@6..12 "isTrue"
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                    BODY@15..20
                      HTML_TEXT@15..20
                        TK_WHITESPACE@15..16 " "
                        TK_TRUE@16..20 "true"
                    TWIG_ENDIF_BLOCK@20..32
                      TK_WHITESPACE@20..21 " "
                      TK_CURLY_PERCENT@21..23 "{%"
                      TK_WHITESPACE@23..24 " "
                      TK_ENDIF@24..29 "endif"
                      TK_WHITESPACE@29..30 " "
                      TK_PERCENT_CURLY@30..32 "%}""#]],
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
                      TWIG_EXPRESSION@5..43
                        TWIG_BINARY_EXPRESSION@5..43
                          TWIG_BINARY_EXPRESSION@5..22
                            TWIG_EXPRESSION@5..17
                              TWIG_LITERAL_NAME@5..17
                                TK_WHITESPACE@5..6 " "
                                TK_WORD@6..17 "temperature"
                            TK_WHITESPACE@17..18 " "
                            TK_GREATER_THAN@18..19 ">"
                            TWIG_EXPRESSION@19..22
                              TWIG_LITERAL_NUMBER@19..22
                                TK_WHITESPACE@19..20 " "
                                TK_NUMBER@20..22 "18"
                          TK_WHITESPACE@22..23 " "
                          TK_AND@23..26 "and"
                          TWIG_EXPRESSION@26..43
                            TWIG_BINARY_EXPRESSION@26..43
                              TWIG_EXPRESSION@26..38
                                TWIG_LITERAL_NAME@26..38
                                  TK_WHITESPACE@26..27 " "
                                  TK_WORD@27..38 "temperature"
                              TK_WHITESPACE@38..39 " "
                              TK_LESS_THAN@39..40 "<"
                              TWIG_EXPRESSION@40..43
                                TWIG_LITERAL_NUMBER@40..43
                                  TK_WHITESPACE@40..41 " "
                                  TK_NUMBER@41..43 "27"
                      TK_WHITESPACE@43..44 " "
                      TK_PERCENT_CURLY@44..46 "%}"
                    BODY@46..51
                      HTML_TEXT@46..51
                        TK_WHITESPACE@46..47 " "
                        TK_TRUE@47..51 "true"
                    TWIG_ENDIF_BLOCK@51..63
                      TK_WHITESPACE@51..52 " "
                      TK_CURLY_PERCENT@52..54 "{%"
                      TK_WHITESPACE@54..55 " "
                      TK_ENDIF@55..60 "endif"
                      TK_WHITESPACE@60..61 " "
                      TK_PERCENT_CURLY@61..63 "%}""#]],
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
                      TWIG_EXPRESSION@5..12
                        TWIG_LITERAL_NAME@5..12
                          TK_WHITESPACE@5..6 " "
                          TK_WORD@6..12 "isTrue"
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                    BODY@15..20
                      HTML_TEXT@15..20
                        TK_WHITESPACE@15..16 " "
                        TK_TRUE@16..20 "true"
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
                        TK_FALSE@32..37 "false"
                    TWIG_ENDIF_BLOCK@37..49
                      TK_WHITESPACE@37..38 " "
                      TK_CURLY_PERCENT@38..40 "{%"
                      TK_WHITESPACE@40..41 " "
                      TK_ENDIF@41..46 "endif"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}""#]],
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
                      TWIG_EXPRESSION@5..9
                        TWIG_LITERAL_NAME@5..9
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
                      TWIG_EXPRESSION@24..28
                        TWIG_LITERAL_NAME@24..28
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
                      TK_PERCENT_CURLY@43..45 "%}""#]],
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
                      TWIG_EXPRESSION@5..9
                        TWIG_LITERAL_NAME@5..9
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
                      TWIG_EXPRESSION@24..28
                        TWIG_LITERAL_NAME@24..28
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
                      TK_PERCENT_CURLY@60..62 "%}""#]],
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
                      TWIG_EXPRESSION@5..9
                        TWIG_LITERAL_NAME@5..9
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
                      TWIG_EXPRESSION@24..28
                        TWIG_LITERAL_NAME@24..28
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
                      TWIG_EXPRESSION@43..47
                        TWIG_LITERAL_NAME@43..47
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
                      TK_PERCENT_CURLY@79..81 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_block_with_unknown_body() {
        check_parse(
            "{% block my_block %} \\t unknown token {% endblock %}",
            expect![[r#"
                ROOT@0..52
                  TWIG_BLOCK@0..52
                    TWIG_STARTING_BLOCK@0..20
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..17 "my_block"
                      TK_WHITESPACE@17..18 " "
                      TK_PERCENT_CURLY@18..20 "%}"
                    BODY@20..37
                      HTML_TEXT@20..37
                        TK_WHITESPACE@20..21 " "
                        TK_BACKWARD_SLASH@21..22 "\\"
                        TK_WORD@22..23 "t"
                        TK_WHITESPACE@23..24 " "
                        TK_WORD@24..31 "unknown"
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..37 "token"
                    TWIG_ENDING_BLOCK@37..52
                      TK_WHITESPACE@37..38 " "
                      TK_CURLY_PERCENT@38..40 "{%"
                      TK_WHITESPACE@40..41 " "
                      TK_ENDBLOCK@41..49 "endblock"
                      TK_WHITESPACE@49..50 " "
                      TK_PERCENT_CURLY@50..52 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_block_without_endblock() {
        check_parse(
            "{% block my_block %}",
            expect![[r#"
                ROOT@0..20
                  TWIG_BLOCK@0..20
                    TWIG_STARTING_BLOCK@0..20
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..17 "my_block"
                      TK_WHITESPACE@17..18 " "
                      TK_PERCENT_CURLY@18..20 "%}"
                    BODY@20..20
                    TWIG_ENDING_BLOCK@20..20
                error at 18..20: expected {% but reached end of file
                error at 18..20: expected endblock but reached end of file
                error at 18..20: expected %} but reached end of file"#]],
        )
    }

    #[test]
    fn parse_twig_single_set() {
        check_parse(
            "{% set foo = 'bar' %}",
            expect![[r#"
            ROOT@0..21
              TWIG_SET@0..21
                TWIG_SET_BLOCK@0..21
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_SET@3..6 "set"
                  TWIG_ASSIGNMENT@6..18
                    TWIG_LITERAL_NAME@6..10
                      TK_WHITESPACE@6..7 " "
                      TK_WORD@7..10 "foo"
                    TK_WHITESPACE@10..11 " "
                    TK_EQUAL@11..12 "="
                    TWIG_EXPRESSION@12..18
                      TWIG_LITERAL_STRING@12..18
                        TK_WHITESPACE@12..13 " "
                        TK_SINGLE_QUOTES@13..14 "'"
                        TWIG_LITERAL_STRING_INNER@14..17
                          TK_WORD@14..17 "bar"
                        TK_SINGLE_QUOTES@17..18 "'"
                  TK_WHITESPACE@18..19 " "
                  TK_PERCENT_CURLY@19..21 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_capturing_set() {
        check_parse(
            r#"{% set foo %}
            hello world
            {% endset %}
        "#,
            expect![[r#"
                ROOT@0..71
                  TWIG_SET@0..62
                    TWIG_SET_BLOCK@0..13
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..10
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                      TK_WHITESPACE@10..11 " "
                      TK_PERCENT_CURLY@11..13 "%}"
                    BODY@13..37
                      HTML_TEXT@13..37
                        TK_LINE_BREAK@13..14 "\n"
                        TK_WHITESPACE@14..26 "            "
                        TK_WORD@26..31 "hello"
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..37 "world"
                    TWIG_ENDSET_BLOCK@37..62
                      TK_LINE_BREAK@37..38 "\n"
                      TK_WHITESPACE@38..50 "            "
                      TK_CURLY_PERCENT@50..52 "{%"
                      TK_WHITESPACE@52..53 " "
                      TK_ENDSET@53..59 "endset"
                      TK_WHITESPACE@59..60 " "
                      TK_PERCENT_CURLY@60..62 "%}"
                  TK_LINE_BREAK@62..63 "\n"
                  TK_WHITESPACE@63..71 "        ""#]],
        )
    }

    #[test]
    fn parse_twig_multi_set() {
        check_parse(
            r#"{% set foo, bar, baz = 'foo', 'bar', 'baz' %}"#,
            expect![[r#"
                ROOT@0..45
                  TWIG_SET@0..45
                    TWIG_SET_BLOCK@0..45
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..42
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                        TK_WHITESPACE@20..21 " "
                        TK_EQUAL@21..22 "="
                        TWIG_EXPRESSION@22..28
                          TWIG_LITERAL_STRING@22..28
                            TK_WHITESPACE@22..23 " "
                            TK_SINGLE_QUOTES@23..24 "'"
                            TWIG_LITERAL_STRING_INNER@24..27
                              TK_WORD@24..27 "foo"
                            TK_SINGLE_QUOTES@27..28 "'"
                        TK_COMMA@28..29 ","
                        TWIG_EXPRESSION@29..35
                          TWIG_LITERAL_STRING@29..35
                            TK_WHITESPACE@29..30 " "
                            TK_SINGLE_QUOTES@30..31 "'"
                            TWIG_LITERAL_STRING_INNER@31..34
                              TK_WORD@31..34 "bar"
                            TK_SINGLE_QUOTES@34..35 "'"
                        TK_COMMA@35..36 ","
                        TWIG_EXPRESSION@36..42
                          TWIG_LITERAL_STRING@36..42
                            TK_WHITESPACE@36..37 " "
                            TK_SINGLE_QUOTES@37..38 "'"
                            TWIG_LITERAL_STRING_INNER@38..41
                              TK_WORD@38..41 "baz"
                            TK_SINGLE_QUOTES@41..42 "'"
                      TK_WHITESPACE@42..43 " "
                      TK_PERCENT_CURLY@43..45 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_multi_set_non_equal_declarations() {
        check_parse(
            r#"{% set foo, bar, baz = 'foo', 'bar' %}"#,
            expect![[r#"
                ROOT@0..38
                  TWIG_SET@0..38
                    TWIG_SET_BLOCK@0..38
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..35
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                        TK_WHITESPACE@20..21 " "
                        TK_EQUAL@21..22 "="
                        TWIG_EXPRESSION@22..28
                          TWIG_LITERAL_STRING@22..28
                            TK_WHITESPACE@22..23 " "
                            TK_SINGLE_QUOTES@23..24 "'"
                            TWIG_LITERAL_STRING_INNER@24..27
                              TK_WORD@24..27 "foo"
                            TK_SINGLE_QUOTES@27..28 "'"
                        TK_COMMA@28..29 ","
                        TWIG_EXPRESSION@29..35
                          TWIG_LITERAL_STRING@29..35
                            TK_WHITESPACE@29..30 " "
                            TK_SINGLE_QUOTES@30..31 "'"
                            TWIG_LITERAL_STRING_INNER@31..34
                              TK_WORD@31..34 "bar"
                            TK_SINGLE_QUOTES@34..35 "'"
                      TK_WHITESPACE@35..36 " "
                      TK_PERCENT_CURLY@36..38 "%}"
                error at 36..38: expected a total of 3 twig expressions (same amount as declarations) instead of 2 but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_multi_with_capturing() {
        check_parse(
            r#"{% set foo, bar, baz %}
                hello world
            {% endset %}
            "#,
            expect![[r#"
                ROOT@0..89
                  TWIG_SET@0..76
                    TWIG_SET_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..20
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..51
                      HTML_TEXT@23..51
                        TK_LINE_BREAK@23..24 "\n"
                        TK_WHITESPACE@24..40 "                "
                        TK_WORD@40..45 "hello"
                        TK_WHITESPACE@45..46 " "
                        TK_WORD@46..51 "world"
                    TWIG_ENDSET_BLOCK@51..76
                      TK_LINE_BREAK@51..52 "\n"
                      TK_WHITESPACE@52..64 "            "
                      TK_CURLY_PERCENT@64..66 "{%"
                      TK_WHITESPACE@66..67 " "
                      TK_ENDSET@67..73 "endset"
                      TK_WHITESPACE@73..74 " "
                      TK_PERCENT_CURLY@74..76 "%}"
                  TK_LINE_BREAK@76..77 "\n"
                  TK_WHITESPACE@77..89 "            "
                error at 21..23: expected = followed by 3 twig expressions but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_set_missing_declaration() {
        check_parse(
            r#"{% set = 'foo' %}
            "#,
            expect![[r#"
                ROOT@0..30
                  TWIG_SET@0..17
                    TWIG_SET_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..14
                        TK_WHITESPACE@6..7 " "
                        TK_EQUAL@7..8 "="
                        TWIG_EXPRESSION@8..14
                          TWIG_LITERAL_STRING@8..14
                            TK_WHITESPACE@8..9 " "
                            TK_SINGLE_QUOTES@9..10 "'"
                            TWIG_LITERAL_STRING_INNER@10..13
                              TK_WORD@10..13 "foo"
                            TK_SINGLE_QUOTES@13..14 "'"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                  TK_LINE_BREAK@17..18 "\n"
                  TK_WHITESPACE@18..30 "            "
                error at 7..8: expected twig variable name but found =
                error at 15..17: expected a total of 0 twig expressions (same amount as declarations) instead of 1 but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_set_missing_assignment() {
        check_parse(
            r#"{% set foo = %}
            "#,
            expect![[r#"
                ROOT@0..28
                  TWIG_SET@0..15
                    TWIG_SET_BLOCK@0..15
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..12
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_WHITESPACE@10..11 " "
                        TK_EQUAL@11..12 "="
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                  TK_LINE_BREAK@15..16 "\n"
                  TK_WHITESPACE@16..28 "            "
                error at 13..15: expected a total of 1 twig expressions (same amount as declarations) instead of 0 but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_set_missing_equal() {
        check_parse(
            r#"{% set foo, bar, baz %}
            "#,
            expect![[r#"
                ROOT@0..36
                  TWIG_SET@0..23
                    TWIG_SET_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..20
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..23
                    TWIG_ENDSET_BLOCK@23..23
                  TK_LINE_BREAK@23..24 "\n"
                  TK_WHITESPACE@24..36 "            "
                error at 21..23: expected = followed by 3 twig expressions but found %}
                error at 24..36: expected {% but reached end of file
                error at 24..36: expected endset but reached end of file
                error at 24..36: expected %} but reached end of file"#]],
        )
    }
}

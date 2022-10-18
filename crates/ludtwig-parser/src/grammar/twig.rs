mod expression;
pub(crate) mod literal;

use crate::grammar::twig::expression::parse_twig_expression;
use crate::grammar::twig::literal::{parse_twig_name, parse_twig_string};
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
    } else if parser.at(T!["for"]) {
        Some(parse_twig_for(parser, m, child_parser))
    } else if parser.at(T!["extends"]) {
        Some(parse_twig_extends(parser, m))
    } else if parser.at(T!["include"]) {
        Some(parse_twig_include(parser, m))
    } else if parser.at(T!["use"]) {
        Some(parse_twig_use(parser, m))
    } else {
        // TODO: implement other twig block statements like if, for, and so on
        parser.add_error(ParseErrorBuilder::new(
            "'block', 'if', 'set' or 'for' (nothing else supported yet)".to_string(),
        ));

        parser.complete(m, SyntaxKind::ERROR);
        None
    }
}

fn parse_twig_use(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["use"]));
    parser.bump();

    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else {
        parser.add_error(ParseErrorBuilder::new("twig string as template"));
    }

    if parser.at(T!["with"]) {
        parser.bump();

        let mut override_count = 0;
        parse_many(
            parser,
            |p| p.at(T!["%}"]),
            |p| {
                let override_m = p.start();
                override_count += 1;
                if parse_twig_name(p).is_none() {
                    p.add_error(ParseErrorBuilder::new("block name"));
                }
                p.expect(T!["as"]);
                if parse_twig_name(p).is_none() {
                    p.add_error(ParseErrorBuilder::new("block name"));
                }
                p.complete(override_m, SyntaxKind::TWIG_USE_OVERRIDE);

                if p.at(T![","]) {
                    // consume optional comma
                    p.bump();
                }
            },
        );

        if override_count < 1 {
            parser.add_error(ParseErrorBuilder::new(
                "at least one block name as block name",
            ));
        }
    }

    parser.expect(T!["%}"]);

    parser.complete(outer, SyntaxKind::TWIG_USE)
}

fn parse_twig_include(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["include"]));
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

    parser.complete(outer, SyntaxKind::TWIG_INCLUDE)
}

fn parse_twig_extends(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["extends"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
    }

    parser.expect(T!["%}"]);

    parser.complete(outer, SyntaxKind::TWIG_EXTENDS)
}

fn parse_twig_for(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["for"]));
    parser.bump();

    // parse key, value identifiers
    if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("variable name"));
    }
    if parser.at(T![","]) {
        parser.bump();
        if parse_twig_name(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("variable name"));
        }
    }

    parser.expect(T!["in"]);

    // parse expression after in
    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
    }

    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_FOR_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except else or endfor
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endfor"]]) || p.at_following(&[T!["{%"], T!["else"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    // check for else block
    if parser.at_following(&[T!["{%"], T!["else"]]) {
        let else_m = parser.start();
        parser.bump();
        parser.bump();
        parser.expect(T!["%}"]);
        parser.complete(else_m, SyntaxKind::TWIG_FOR_ELSE_BLOCK);

        // parse all the children except endfor
        let body_m = parser.start();
        parse_many(
            parser,
            |p| p.at_following(&[T!["{%"], T!["endfor"]]),
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);
    }

    let end_block_m = parser.start();
    parser.expect(T!["{%"]);
    parser.expect(T!["endfor"]);
    parser.expect(T!["%}"]);
    parser.complete(end_block_m, SyntaxKind::TWIG_ENDFOR_BLOCK);

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::TWIG_FOR)
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
    let block_name = parser.expect(T![word]).map(|t| t.text.to_owned());
    // look for optional shortcut
    let mut found_shortcut = false;
    if !parser.at(T!["%}"]) {
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression or '%}'"));
        } else {
            found_shortcut = true;
        }
    }
    parser.expect(T!["%}"]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    if !found_shortcut {
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
        // check for optional name behind endblock
        if parser.at(T![word]) {
            let end_block_name_token = parser.bump();
            if let Some(block_name) = block_name {
                if end_block_name_token.text != block_name {
                    let parser_err = ParseErrorBuilder::new(format!(
                        "nothing or same twig block name as opening ({})",
                        block_name
                    ))
                    .at_token(end_block_name_token);

                    parser.add_error(parser_err);
                }
            }
        }
        parser.expect(T!["%}"]);
        parser.complete(end_block_m, SyntaxKind::TWIG_ENDING_BLOCK);
    }

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
                error at 3..7: expected 'block', 'if', 'set' or 'for' (nothing else supported yet) but found word"#]],
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
    fn parse_twig_block_with_named_endlbock() {
        check_parse(
            "{% block sidebar %}
    {% block inner_sidebar %}
        ...
    {% endblock inner_sidebar %}
{% endblock sidebar %}",
            expect![[r#"
                ROOT@0..117
                  TWIG_BLOCK@0..117
                    TWIG_STARTING_BLOCK@0..19
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..16 "sidebar"
                      TK_WHITESPACE@16..17 " "
                      TK_PERCENT_CURLY@17..19 "%}"
                    BODY@19..94
                      TWIG_BLOCK@19..94
                        TWIG_STARTING_BLOCK@19..49
                          TK_LINE_BREAK@19..20 "\n"
                          TK_WHITESPACE@20..24 "    "
                          TK_CURLY_PERCENT@24..26 "{%"
                          TK_WHITESPACE@26..27 " "
                          TK_BLOCK@27..32 "block"
                          TK_WHITESPACE@32..33 " "
                          TK_WORD@33..46 "inner_sidebar"
                          TK_WHITESPACE@46..47 " "
                          TK_PERCENT_CURLY@47..49 "%}"
                        BODY@49..61
                          HTML_TEXT@49..61
                            TK_LINE_BREAK@49..50 "\n"
                            TK_WHITESPACE@50..58 "        "
                            TK_DOUBLE_DOT@58..60 ".."
                            TK_DOT@60..61 "."
                        TWIG_ENDING_BLOCK@61..94
                          TK_LINE_BREAK@61..62 "\n"
                          TK_WHITESPACE@62..66 "    "
                          TK_CURLY_PERCENT@66..68 "{%"
                          TK_WHITESPACE@68..69 " "
                          TK_ENDBLOCK@69..77 "endblock"
                          TK_WHITESPACE@77..78 " "
                          TK_WORD@78..91 "inner_sidebar"
                          TK_WHITESPACE@91..92 " "
                          TK_PERCENT_CURLY@92..94 "%}"
                    TWIG_ENDING_BLOCK@94..117
                      TK_LINE_BREAK@94..95 "\n"
                      TK_CURLY_PERCENT@95..97 "{%"
                      TK_WHITESPACE@97..98 " "
                      TK_ENDBLOCK@98..106 "endblock"
                      TK_WHITESPACE@106..107 " "
                      TK_WORD@107..114 "sidebar"
                      TK_WHITESPACE@114..115 " "
                      TK_PERCENT_CURLY@115..117 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_block_with_named_endlbock_mismatch() {
        check_parse(
            "{% block sidebar %}
    {% block inner_sidebar %}
        ...
    {% endblock sidebar %}
{% endblock sidebar %}",
            expect![[r#"
                ROOT@0..111
                  TWIG_BLOCK@0..111
                    TWIG_STARTING_BLOCK@0..19
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..16 "sidebar"
                      TK_WHITESPACE@16..17 " "
                      TK_PERCENT_CURLY@17..19 "%}"
                    BODY@19..88
                      TWIG_BLOCK@19..88
                        TWIG_STARTING_BLOCK@19..49
                          TK_LINE_BREAK@19..20 "\n"
                          TK_WHITESPACE@20..24 "    "
                          TK_CURLY_PERCENT@24..26 "{%"
                          TK_WHITESPACE@26..27 " "
                          TK_BLOCK@27..32 "block"
                          TK_WHITESPACE@32..33 " "
                          TK_WORD@33..46 "inner_sidebar"
                          TK_WHITESPACE@46..47 " "
                          TK_PERCENT_CURLY@47..49 "%}"
                        BODY@49..61
                          HTML_TEXT@49..61
                            TK_LINE_BREAK@49..50 "\n"
                            TK_WHITESPACE@50..58 "        "
                            TK_DOUBLE_DOT@58..60 ".."
                            TK_DOT@60..61 "."
                        TWIG_ENDING_BLOCK@61..88
                          TK_LINE_BREAK@61..62 "\n"
                          TK_WHITESPACE@62..66 "    "
                          TK_CURLY_PERCENT@66..68 "{%"
                          TK_WHITESPACE@68..69 " "
                          TK_ENDBLOCK@69..77 "endblock"
                          TK_WHITESPACE@77..78 " "
                          TK_WORD@78..85 "sidebar"
                          TK_WHITESPACE@85..86 " "
                          TK_PERCENT_CURLY@86..88 "%}"
                    TWIG_ENDING_BLOCK@88..111
                      TK_LINE_BREAK@88..89 "\n"
                      TK_CURLY_PERCENT@89..91 "{%"
                      TK_WHITESPACE@91..92 " "
                      TK_ENDBLOCK@92..100 "endblock"
                      TK_WHITESPACE@100..101 " "
                      TK_WORD@101..108 "sidebar"
                      TK_WHITESPACE@108..109 " "
                      TK_PERCENT_CURLY@109..111 "%}"
                error at 78..85: expected nothing or same twig block name as opening (inner_sidebar) but found word"#]],
        )
    }

    #[test]
    fn parse_twig_block_shortcut() {
        check_parse(
            "{% block title page_title|title %}",
            expect![[r#"
            ROOT@0..34
              TWIG_BLOCK@0..34
                TWIG_STARTING_BLOCK@0..34
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..14 "title"
                  TWIG_EXPRESSION@14..31
                    TWIG_PIPE@14..31
                      TWIG_OPERAND@14..25
                        TWIG_LITERAL_NAME@14..25
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..25 "page_title"
                      TK_SINGLE_PIPE@25..26 "|"
                      TWIG_OPERAND@26..31
                        TWIG_LITERAL_NAME@26..31
                          TK_WORD@26..31 "title"
                  TK_WHITESPACE@31..32 " "
                  TK_PERCENT_CURLY@32..34 "%}""#]],
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

    #[test]
    fn parse_twig_for_in_users() {
        check_parse(
            r#"{% for user in users %}
        <li>{{ user.username }}</li>
    {% endfor %}"#,
            expect![[r#"
                ROOT@0..77
                  TWIG_FOR@0..77
                    TWIG_FOR_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TWIG_EXPRESSION@14..20
                        TWIG_LITERAL_NAME@14..20
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..20 "users"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..60
                      HTML_TAG@23..60
                        HTML_STARTING_TAG@23..36
                          TK_LINE_BREAK@23..24 "\n"
                          TK_WHITESPACE@24..32 "        "
                          TK_LESS_THAN@32..33 "<"
                          TK_WORD@33..35 "li"
                          TK_GREATER_THAN@35..36 ">"
                        BODY@36..55
                          TWIG_VAR@36..55
                            TK_OPEN_CURLY_CURLY@36..38 "{{"
                            TWIG_EXPRESSION@38..52
                              TWIG_ACCESSOR@38..52
                                TWIG_OPERAND@38..43
                                  TWIG_LITERAL_NAME@38..43
                                    TK_WHITESPACE@38..39 " "
                                    TK_WORD@39..43 "user"
                                TK_DOT@43..44 "."
                                TWIG_OPERAND@44..52
                                  TWIG_LITERAL_NAME@44..52
                                    TK_WORD@44..52 "username"
                            TK_WHITESPACE@52..53 " "
                            TK_CLOSE_CURLY_CURLY@53..55 "}}"
                        HTML_ENDING_TAG@55..60
                          TK_LESS_THAN_SLASH@55..57 "</"
                          TK_WORD@57..59 "li"
                          TK_GREATER_THAN@59..60 ">"
                    TWIG_ENDFOR_BLOCK@60..77
                      TK_LINE_BREAK@60..61 "\n"
                      TK_WHITESPACE@61..65 "    "
                      TK_CURLY_PERCENT@65..67 "{%"
                      TK_WHITESPACE@67..68 " "
                      TK_ENDFOR@68..74 "endfor"
                      TK_WHITESPACE@74..75 " "
                      TK_PERCENT_CURLY@75..77 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_for_in_number_range() {
        check_parse(
            r#"{% for i in 0..10 %}
    * {{ i }}
{% endfor %}"#,
            expect![[r#"
                ROOT@0..47
                  TWIG_FOR@0..47
                    TWIG_FOR_BLOCK@0..20
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..8
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..8 "i"
                      TK_WHITESPACE@8..9 " "
                      TK_IN@9..11 "in"
                      TWIG_EXPRESSION@11..17
                        TWIG_BINARY_EXPRESSION@11..17
                          TWIG_EXPRESSION@11..13
                            TWIG_LITERAL_NUMBER@11..13
                              TK_WHITESPACE@11..12 " "
                              TK_NUMBER@12..13 "0"
                          TK_DOUBLE_DOT@13..15 ".."
                          TWIG_EXPRESSION@15..17
                            TWIG_LITERAL_NUMBER@15..17
                              TK_NUMBER@15..17 "10"
                      TK_WHITESPACE@17..18 " "
                      TK_PERCENT_CURLY@18..20 "%}"
                    BODY@20..34
                      HTML_TEXT@20..26
                        TK_LINE_BREAK@20..21 "\n"
                        TK_WHITESPACE@21..25 "    "
                        TK_STAR@25..26 "*"
                      TWIG_VAR@26..34
                        TK_WHITESPACE@26..27 " "
                        TK_OPEN_CURLY_CURLY@27..29 "{{"
                        TWIG_EXPRESSION@29..31
                          TWIG_LITERAL_NAME@29..31
                            TK_WHITESPACE@29..30 " "
                            TK_WORD@30..31 "i"
                        TK_WHITESPACE@31..32 " "
                        TK_CLOSE_CURLY_CURLY@32..34 "}}"
                    TWIG_ENDFOR_BLOCK@34..47
                      TK_LINE_BREAK@34..35 "\n"
                      TK_CURLY_PERCENT@35..37 "{%"
                      TK_WHITESPACE@37..38 " "
                      TK_ENDFOR@38..44 "endfor"
                      TK_WHITESPACE@44..45 " "
                      TK_PERCENT_CURLY@45..47 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_for_in_letter_range_with_filters() {
        check_parse(
            r#"{% for letter in 'a'|upper..'z'|upper %}
    * {{ letter }}
{% endfor %}"#,
            expect![[r#"
                ROOT@0..72
                  TWIG_FOR@0..72
                    TWIG_FOR_BLOCK@0..40
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..13
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..13 "letter"
                      TK_WHITESPACE@13..14 " "
                      TK_IN@14..16 "in"
                      TWIG_EXPRESSION@16..37
                        TWIG_BINARY_EXPRESSION@16..37
                          TWIG_EXPRESSION@16..26
                            TWIG_PIPE@16..26
                              TWIG_OPERAND@16..20
                                TWIG_LITERAL_STRING@16..20
                                  TK_WHITESPACE@16..17 " "
                                  TK_SINGLE_QUOTES@17..18 "'"
                                  TWIG_LITERAL_STRING_INNER@18..19
                                    TK_WORD@18..19 "a"
                                  TK_SINGLE_QUOTES@19..20 "'"
                              TK_SINGLE_PIPE@20..21 "|"
                              TWIG_OPERAND@21..26
                                TWIG_LITERAL_NAME@21..26
                                  TK_WORD@21..26 "upper"
                          TK_DOUBLE_DOT@26..28 ".."
                          TWIG_EXPRESSION@28..37
                            TWIG_PIPE@28..37
                              TWIG_OPERAND@28..31
                                TWIG_LITERAL_STRING@28..31
                                  TK_SINGLE_QUOTES@28..29 "'"
                                  TWIG_LITERAL_STRING_INNER@29..30
                                    TK_WORD@29..30 "z"
                                  TK_SINGLE_QUOTES@30..31 "'"
                              TK_SINGLE_PIPE@31..32 "|"
                              TWIG_OPERAND@32..37
                                TWIG_LITERAL_NAME@32..37
                                  TK_WORD@32..37 "upper"
                      TK_WHITESPACE@37..38 " "
                      TK_PERCENT_CURLY@38..40 "%}"
                    BODY@40..59
                      HTML_TEXT@40..46
                        TK_LINE_BREAK@40..41 "\n"
                        TK_WHITESPACE@41..45 "    "
                        TK_STAR@45..46 "*"
                      TWIG_VAR@46..59
                        TK_WHITESPACE@46..47 " "
                        TK_OPEN_CURLY_CURLY@47..49 "{{"
                        TWIG_EXPRESSION@49..56
                          TWIG_LITERAL_NAME@49..56
                            TK_WHITESPACE@49..50 " "
                            TK_WORD@50..56 "letter"
                        TK_WHITESPACE@56..57 " "
                        TK_CLOSE_CURLY_CURLY@57..59 "}}"
                    TWIG_ENDFOR_BLOCK@59..72
                      TK_LINE_BREAK@59..60 "\n"
                      TK_CURLY_PERCENT@60..62 "{%"
                      TK_WHITESPACE@62..63 " "
                      TK_ENDFOR@63..69 "endfor"
                      TK_WHITESPACE@69..70 " "
                      TK_PERCENT_CURLY@70..72 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_for_key_value_in_users() {
        check_parse(
            r#"{% for key, user in users %}
        <li>{{ key }}: {{ user.username|e }}</li>
    {% endfor %}"#,
            expect![[r#"
                ROOT@0..95
                  TWIG_FOR@0..95
                    TWIG_FOR_BLOCK@0..28
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..10
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..10 "key"
                      TK_COMMA@10..11 ","
                      TWIG_LITERAL_NAME@11..16
                        TK_WHITESPACE@11..12 " "
                        TK_WORD@12..16 "user"
                      TK_WHITESPACE@16..17 " "
                      TK_IN@17..19 "in"
                      TWIG_EXPRESSION@19..25
                        TWIG_LITERAL_NAME@19..25
                          TK_WHITESPACE@19..20 " "
                          TK_WORD@20..25 "users"
                      TK_WHITESPACE@25..26 " "
                      TK_PERCENT_CURLY@26..28 "%}"
                    BODY@28..78
                      HTML_TAG@28..78
                        HTML_STARTING_TAG@28..41
                          TK_LINE_BREAK@28..29 "\n"
                          TK_WHITESPACE@29..37 "        "
                          TK_LESS_THAN@37..38 "<"
                          TK_WORD@38..40 "li"
                          TK_GREATER_THAN@40..41 ">"
                        BODY@41..73
                          TWIG_VAR@41..50
                            TK_OPEN_CURLY_CURLY@41..43 "{{"
                            TWIG_EXPRESSION@43..47
                              TWIG_LITERAL_NAME@43..47
                                TK_WHITESPACE@43..44 " "
                                TK_WORD@44..47 "key"
                            TK_WHITESPACE@47..48 " "
                            TK_CLOSE_CURLY_CURLY@48..50 "}}"
                          HTML_TEXT@50..51
                            TK_COLON@50..51 ":"
                          TWIG_VAR@51..73
                            TK_WHITESPACE@51..52 " "
                            TK_OPEN_CURLY_CURLY@52..54 "{{"
                            TWIG_EXPRESSION@54..70
                              TWIG_PIPE@54..70
                                TWIG_OPERAND@54..68
                                  TWIG_ACCESSOR@54..68
                                    TWIG_OPERAND@54..59
                                      TWIG_LITERAL_NAME@54..59
                                        TK_WHITESPACE@54..55 " "
                                        TK_WORD@55..59 "user"
                                    TK_DOT@59..60 "."
                                    TWIG_OPERAND@60..68
                                      TWIG_LITERAL_NAME@60..68
                                        TK_WORD@60..68 "username"
                                TK_SINGLE_PIPE@68..69 "|"
                                TWIG_OPERAND@69..70
                                  TWIG_LITERAL_NAME@69..70
                                    TK_WORD@69..70 "e"
                            TK_WHITESPACE@70..71 " "
                            TK_CLOSE_CURLY_CURLY@71..73 "}}"
                        HTML_ENDING_TAG@73..78
                          TK_LESS_THAN_SLASH@73..75 "</"
                          TK_WORD@75..77 "li"
                          TK_GREATER_THAN@77..78 ">"
                    TWIG_ENDFOR_BLOCK@78..95
                      TK_LINE_BREAK@78..79 "\n"
                      TK_WHITESPACE@79..83 "    "
                      TK_CURLY_PERCENT@83..85 "{%"
                      TK_WHITESPACE@85..86 " "
                      TK_ENDFOR@86..92 "endfor"
                      TK_WHITESPACE@92..93 " "
                      TK_PERCENT_CURLY@93..95 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_for_with_else() {
        check_parse(
            r#"{% for user in users %}
        <li>{{ user.username }}</li>
    {% else %}
        <li><em>no user found</em></li>
    {% endfor %}"#,
            expect![[r#"
                ROOT@0..132
                  TWIG_FOR@0..132
                    TWIG_FOR_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TWIG_EXPRESSION@14..20
                        TWIG_LITERAL_NAME@14..20
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..20 "users"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..60
                      HTML_TAG@23..60
                        HTML_STARTING_TAG@23..36
                          TK_LINE_BREAK@23..24 "\n"
                          TK_WHITESPACE@24..32 "        "
                          TK_LESS_THAN@32..33 "<"
                          TK_WORD@33..35 "li"
                          TK_GREATER_THAN@35..36 ">"
                        BODY@36..55
                          TWIG_VAR@36..55
                            TK_OPEN_CURLY_CURLY@36..38 "{{"
                            TWIG_EXPRESSION@38..52
                              TWIG_ACCESSOR@38..52
                                TWIG_OPERAND@38..43
                                  TWIG_LITERAL_NAME@38..43
                                    TK_WHITESPACE@38..39 " "
                                    TK_WORD@39..43 "user"
                                TK_DOT@43..44 "."
                                TWIG_OPERAND@44..52
                                  TWIG_LITERAL_NAME@44..52
                                    TK_WORD@44..52 "username"
                            TK_WHITESPACE@52..53 " "
                            TK_CLOSE_CURLY_CURLY@53..55 "}}"
                        HTML_ENDING_TAG@55..60
                          TK_LESS_THAN_SLASH@55..57 "</"
                          TK_WORD@57..59 "li"
                          TK_GREATER_THAN@59..60 ">"
                    TWIG_FOR_ELSE_BLOCK@60..75
                      TK_LINE_BREAK@60..61 "\n"
                      TK_WHITESPACE@61..65 "    "
                      TK_CURLY_PERCENT@65..67 "{%"
                      TK_WHITESPACE@67..68 " "
                      TK_ELSE@68..72 "else"
                      TK_WHITESPACE@72..73 " "
                      TK_PERCENT_CURLY@73..75 "%}"
                    BODY@75..115
                      HTML_TAG@75..115
                        HTML_STARTING_TAG@75..88
                          TK_LINE_BREAK@75..76 "\n"
                          TK_WHITESPACE@76..84 "        "
                          TK_LESS_THAN@84..85 "<"
                          TK_WORD@85..87 "li"
                          TK_GREATER_THAN@87..88 ">"
                        BODY@88..110
                          HTML_TAG@88..110
                            HTML_STARTING_TAG@88..92
                              TK_LESS_THAN@88..89 "<"
                              TK_WORD@89..91 "em"
                              TK_GREATER_THAN@91..92 ">"
                            BODY@92..105
                              HTML_TEXT@92..105
                                TK_WORD@92..94 "no"
                                TK_WHITESPACE@94..95 " "
                                TK_WORD@95..99 "user"
                                TK_WHITESPACE@99..100 " "
                                TK_WORD@100..105 "found"
                            HTML_ENDING_TAG@105..110
                              TK_LESS_THAN_SLASH@105..107 "</"
                              TK_WORD@107..109 "em"
                              TK_GREATER_THAN@109..110 ">"
                        HTML_ENDING_TAG@110..115
                          TK_LESS_THAN_SLASH@110..112 "</"
                          TK_WORD@112..114 "li"
                          TK_GREATER_THAN@114..115 ">"
                    TWIG_ENDFOR_BLOCK@115..132
                      TK_LINE_BREAK@115..116 "\n"
                      TK_WHITESPACE@116..120 "    "
                      TK_CURLY_PERCENT@120..122 "{%"
                      TK_WHITESPACE@122..123 " "
                      TK_ENDFOR@123..129 "endfor"
                      TK_WHITESPACE@129..130 " "
                      TK_PERCENT_CURLY@130..132 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_for_with_missing_variable() {
        check_parse(
            r#"{% for in users %}
        <li>{{ user.username }}</li>
    {% endfor %}"#,
            expect![[r#"
                ROOT@0..72
                  TWIG_FOR@0..72
                    TWIG_FOR_BLOCK@0..18
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..9
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..9 "in"
                      ERROR@9..15
                        TK_WHITESPACE@9..10 " "
                        TK_WORD@10..15 "users"
                      TK_WHITESPACE@15..16 " "
                      TK_PERCENT_CURLY@16..18 "%}"
                    BODY@18..55
                      HTML_TAG@18..55
                        HTML_STARTING_TAG@18..31
                          TK_LINE_BREAK@18..19 "\n"
                          TK_WHITESPACE@19..27 "        "
                          TK_LESS_THAN@27..28 "<"
                          TK_WORD@28..30 "li"
                          TK_GREATER_THAN@30..31 ">"
                        BODY@31..50
                          TWIG_VAR@31..50
                            TK_OPEN_CURLY_CURLY@31..33 "{{"
                            TWIG_EXPRESSION@33..47
                              TWIG_ACCESSOR@33..47
                                TWIG_OPERAND@33..38
                                  TWIG_LITERAL_NAME@33..38
                                    TK_WHITESPACE@33..34 " "
                                    TK_WORD@34..38 "user"
                                TK_DOT@38..39 "."
                                TWIG_OPERAND@39..47
                                  TWIG_LITERAL_NAME@39..47
                                    TK_WORD@39..47 "username"
                            TK_WHITESPACE@47..48 " "
                            TK_CLOSE_CURLY_CURLY@48..50 "}}"
                        HTML_ENDING_TAG@50..55
                          TK_LESS_THAN_SLASH@50..52 "</"
                          TK_WORD@52..54 "li"
                          TK_GREATER_THAN@54..55 ">"
                    TWIG_ENDFOR_BLOCK@55..72
                      TK_LINE_BREAK@55..56 "\n"
                      TK_WHITESPACE@56..60 "    "
                      TK_CURLY_PERCENT@60..62 "{%"
                      TK_WHITESPACE@62..63 " "
                      TK_ENDFOR@63..69 "endfor"
                      TK_WHITESPACE@69..70 " "
                      TK_PERCENT_CURLY@70..72 "%}"
                error at 10..15: expected in but found word
                error at 16..18: expected twig expression but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_for_with_missing_expression() {
        check_parse(
            r#"{% for user in %}
        <li>{{ user.username }}</li>
    {% endfor %}"#,
            expect![[r#"
                ROOT@0..71
                  TWIG_FOR@0..71
                    TWIG_FOR_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                    BODY@17..54
                      HTML_TAG@17..54
                        HTML_STARTING_TAG@17..30
                          TK_LINE_BREAK@17..18 "\n"
                          TK_WHITESPACE@18..26 "        "
                          TK_LESS_THAN@26..27 "<"
                          TK_WORD@27..29 "li"
                          TK_GREATER_THAN@29..30 ">"
                        BODY@30..49
                          TWIG_VAR@30..49
                            TK_OPEN_CURLY_CURLY@30..32 "{{"
                            TWIG_EXPRESSION@32..46
                              TWIG_ACCESSOR@32..46
                                TWIG_OPERAND@32..37
                                  TWIG_LITERAL_NAME@32..37
                                    TK_WHITESPACE@32..33 " "
                                    TK_WORD@33..37 "user"
                                TK_DOT@37..38 "."
                                TWIG_OPERAND@38..46
                                  TWIG_LITERAL_NAME@38..46
                                    TK_WORD@38..46 "username"
                            TK_WHITESPACE@46..47 " "
                            TK_CLOSE_CURLY_CURLY@47..49 "}}"
                        HTML_ENDING_TAG@49..54
                          TK_LESS_THAN_SLASH@49..51 "</"
                          TK_WORD@51..53 "li"
                          TK_GREATER_THAN@53..54 ">"
                    TWIG_ENDFOR_BLOCK@54..71
                      TK_LINE_BREAK@54..55 "\n"
                      TK_WHITESPACE@55..59 "    "
                      TK_CURLY_PERCENT@59..61 "{%"
                      TK_WHITESPACE@61..62 " "
                      TK_ENDFOR@62..68 "endfor"
                      TK_WHITESPACE@68..69 " "
                      TK_PERCENT_CURLY@69..71 "%}"
                error at 15..17: expected twig expression but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_extends_with_string() {
        check_parse(
            r#"{% extends "base.html" %}"#,
            expect![[r#"
            ROOT@0..25
              TWIG_EXTENDS@0..25
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_EXTENDS@3..10 "extends"
                TWIG_EXPRESSION@10..22
                  TWIG_LITERAL_STRING@10..22
                    TK_WHITESPACE@10..11 " "
                    TK_DOUBLE_QUOTES@11..12 "\""
                    TWIG_LITERAL_STRING_INNER@12..21
                      TK_WORD@12..16 "base"
                      TK_DOT@16..17 "."
                      TK_WORD@17..21 "html"
                    TK_DOUBLE_QUOTES@21..22 "\""
                TK_WHITESPACE@22..23 " "
                TK_PERCENT_CURLY@23..25 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_extends_with_variable() {
        check_parse(
            r#"{% extends some_var %}"#,
            expect![[r#"
            ROOT@0..22
              TWIG_EXTENDS@0..22
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_EXTENDS@3..10 "extends"
                TWIG_EXPRESSION@10..19
                  TWIG_LITERAL_NAME@10..19
                    TK_WHITESPACE@10..11 " "
                    TK_WORD@11..19 "some_var"
                TK_WHITESPACE@19..20 " "
                TK_PERCENT_CURLY@20..22 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_extends_with_array() {
        check_parse(
            r#"{% extends ['layout.html', 'base_layout.html'] %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_EXTENDS@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_EXTENDS@3..10 "extends"
                    TWIG_EXPRESSION@10..46
                      TWIG_LITERAL_ARRAY@10..46
                        TK_WHITESPACE@10..11 " "
                        TK_OPEN_SQUARE@11..12 "["
                        TWIG_EXPRESSION@12..25
                          TWIG_LITERAL_STRING@12..25
                            TK_SINGLE_QUOTES@12..13 "'"
                            TWIG_LITERAL_STRING_INNER@13..24
                              TK_WORD@13..19 "layout"
                              TK_DOT@19..20 "."
                              TK_WORD@20..24 "html"
                            TK_SINGLE_QUOTES@24..25 "'"
                        TK_COMMA@25..26 ","
                        TWIG_EXPRESSION@26..45
                          TWIG_LITERAL_STRING@26..45
                            TK_WHITESPACE@26..27 " "
                            TK_SINGLE_QUOTES@27..28 "'"
                            TWIG_LITERAL_STRING_INNER@28..44
                              TK_WORD@28..39 "base_layout"
                              TK_DOT@39..40 "."
                              TK_WORD@40..44 "html"
                            TK_SINGLE_QUOTES@44..45 "'"
                        TK_CLOSE_SQUARE@45..46 "]"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_extends_with_conditional() {
        check_parse(
            r#"{% extends standalone ? "minimum.html" : "base.html" %}"#,
            expect![[r#"
                ROOT@0..55
                  TWIG_EXTENDS@0..55
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_EXTENDS@3..10 "extends"
                    TWIG_EXPRESSION@10..52
                      TWIG_CONDITIONAL_EXPRESSION@10..52
                        TWIG_EXPRESSION@10..21
                          TWIG_LITERAL_NAME@10..21
                            TK_WHITESPACE@10..11 " "
                            TK_WORD@11..21 "standalone"
                        TK_WHITESPACE@21..22 " "
                        TK_QUESTION_MARK@22..23 "?"
                        TWIG_EXPRESSION@23..38
                          TWIG_LITERAL_STRING@23..38
                            TK_WHITESPACE@23..24 " "
                            TK_DOUBLE_QUOTES@24..25 "\""
                            TWIG_LITERAL_STRING_INNER@25..37
                              TK_WORD@25..32 "minimum"
                              TK_DOT@32..33 "."
                              TK_WORD@33..37 "html"
                            TK_DOUBLE_QUOTES@37..38 "\""
                        TK_WHITESPACE@38..39 " "
                        TK_COLON@39..40 ":"
                        TWIG_EXPRESSION@40..52
                          TWIG_LITERAL_STRING@40..52
                            TK_WHITESPACE@40..41 " "
                            TK_DOUBLE_QUOTES@41..42 "\""
                            TWIG_LITERAL_STRING_INNER@42..51
                              TK_WORD@42..46 "base"
                              TK_DOT@46..47 "."
                              TK_WORD@47..51 "html"
                            TK_DOUBLE_QUOTES@51..52 "\""
                    TK_WHITESPACE@52..53 " "
                    TK_PERCENT_CURLY@53..55 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_extends_missing_expression() {
        check_parse(
            r#"{% extends %}"#,
            expect![[r#"
            ROOT@0..13
              TWIG_EXTENDS@0..13
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_EXTENDS@3..10 "extends"
                TK_WHITESPACE@10..11 " "
                TK_PERCENT_CURLY@11..13 "%}"
            error at 11..13: expected twig expression but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_include_string() {
        check_parse(
            r#"{% include 'header.html' %}"#,
            expect![[r#"
            ROOT@0..27
              TWIG_INCLUDE@0..27
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TWIG_EXPRESSION@10..24
                  TWIG_LITERAL_STRING@10..24
                    TK_WHITESPACE@10..11 " "
                    TK_SINGLE_QUOTES@11..12 "'"
                    TWIG_LITERAL_STRING_INNER@12..23
                      TK_WORD@12..18 "header"
                      TK_DOT@18..19 "."
                      TK_WORD@19..23 "html"
                    TK_SINGLE_QUOTES@23..24 "'"
                TK_WHITESPACE@24..25 " "
                TK_PERCENT_CURLY@25..27 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_with_variable() {
        check_parse(
            r#"{% include 'template.html' with vars %}"#,
            expect![[r#"
                ROOT@0..39
                  TWIG_INCLUDE@0..39
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..36
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                      TWIG_EXPRESSION@31..36
                        TWIG_LITERAL_NAME@31..36
                          TK_WHITESPACE@31..32 " "
                          TK_WORD@32..36 "vars"
                    TK_WHITESPACE@36..37 " "
                    TK_PERCENT_CURLY@37..39 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_with_hash() {
        check_parse(
            r#"{% include 'template.html' with {'foo': 'bar'} %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_INCLUDE@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..46
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                      TWIG_EXPRESSION@31..46
                        TWIG_LITERAL_HASH@31..46
                          TK_WHITESPACE@31..32 " "
                          TK_OPEN_CURLY@32..33 "{"
                          TWIG_LITERAL_HASH_PAIR@33..45
                            TWIG_LITERAL_HASH_KEY@33..38
                              TWIG_LITERAL_STRING@33..38
                                TK_SINGLE_QUOTES@33..34 "'"
                                TWIG_LITERAL_STRING_INNER@34..37
                                  TK_WORD@34..37 "foo"
                                TK_SINGLE_QUOTES@37..38 "'"
                            TK_COLON@38..39 ":"
                            TWIG_EXPRESSION@39..45
                              TWIG_LITERAL_STRING@39..45
                                TK_WHITESPACE@39..40 " "
                                TK_SINGLE_QUOTES@40..41 "'"
                                TWIG_LITERAL_STRING_INNER@41..44
                                  TK_WORD@41..44 "bar"
                                TK_SINGLE_QUOTES@44..45 "'"
                          TK_CLOSE_CURLY@45..46 "}"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_with_hash_only() {
        check_parse(
            r#"{% include 'template.html' with {'foo': 'bar'} only %}"#,
            expect![[r#"
                ROOT@0..54
                  TWIG_INCLUDE@0..54
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..46
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                      TWIG_EXPRESSION@31..46
                        TWIG_LITERAL_HASH@31..46
                          TK_WHITESPACE@31..32 " "
                          TK_OPEN_CURLY@32..33 "{"
                          TWIG_LITERAL_HASH_PAIR@33..45
                            TWIG_LITERAL_HASH_KEY@33..38
                              TWIG_LITERAL_STRING@33..38
                                TK_SINGLE_QUOTES@33..34 "'"
                                TWIG_LITERAL_STRING_INNER@34..37
                                  TK_WORD@34..37 "foo"
                                TK_SINGLE_QUOTES@37..38 "'"
                            TK_COLON@38..39 ":"
                            TWIG_EXPRESSION@39..45
                              TWIG_LITERAL_STRING@39..45
                                TK_WHITESPACE@39..40 " "
                                TK_SINGLE_QUOTES@40..41 "'"
                                TWIG_LITERAL_STRING_INNER@41..44
                                  TK_WORD@41..44 "bar"
                                TK_SINGLE_QUOTES@44..45 "'"
                          TK_CLOSE_CURLY@45..46 "}"
                    TK_WHITESPACE@46..47 " "
                    TK_ONLY@47..51 "only"
                    TK_WHITESPACE@51..52 " "
                    TK_PERCENT_CURLY@52..54 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_only() {
        check_parse(
            r#"{% include 'template.html' only %}"#,
            expect![[r#"
            ROOT@0..34
              TWIG_INCLUDE@0..34
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TWIG_EXPRESSION@10..26
                  TWIG_LITERAL_STRING@10..26
                    TK_WHITESPACE@10..11 " "
                    TK_SINGLE_QUOTES@11..12 "'"
                    TWIG_LITERAL_STRING_INNER@12..25
                      TK_WORD@12..20 "template"
                      TK_DOT@20..21 "."
                      TK_WORD@21..25 "html"
                    TK_SINGLE_QUOTES@25..26 "'"
                TK_WHITESPACE@26..27 " "
                TK_ONLY@27..31 "only"
                TK_WHITESPACE@31..32 " "
                TK_PERCENT_CURLY@32..34 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_expression() {
        check_parse(
            r#"{% include ajax ? 'ajax.html' : 'not_ajax.html' %}"#,
            expect![[r#"
                ROOT@0..50
                  TWIG_INCLUDE@0..50
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..47
                      TWIG_CONDITIONAL_EXPRESSION@10..47
                        TWIG_EXPRESSION@10..15
                          TWIG_LITERAL_NAME@10..15
                            TK_WHITESPACE@10..11 " "
                            TK_WORD@11..15 "ajax"
                        TK_WHITESPACE@15..16 " "
                        TK_QUESTION_MARK@16..17 "?"
                        TWIG_EXPRESSION@17..29
                          TWIG_LITERAL_STRING@17..29
                            TK_WHITESPACE@17..18 " "
                            TK_SINGLE_QUOTES@18..19 "'"
                            TWIG_LITERAL_STRING_INNER@19..28
                              TK_WORD@19..23 "ajax"
                              TK_DOT@23..24 "."
                              TK_WORD@24..28 "html"
                            TK_SINGLE_QUOTES@28..29 "'"
                        TK_WHITESPACE@29..30 " "
                        TK_COLON@30..31 ":"
                        TWIG_EXPRESSION@31..47
                          TWIG_LITERAL_STRING@31..47
                            TK_WHITESPACE@31..32 " "
                            TK_SINGLE_QUOTES@32..33 "'"
                            TWIG_LITERAL_STRING_INNER@33..46
                              TK_WORD@33..41 "not_ajax"
                              TK_DOT@41..42 "."
                              TK_WORD@42..46 "html"
                            TK_SINGLE_QUOTES@46..47 "'"
                    TK_WHITESPACE@47..48 " "
                    TK_PERCENT_CURLY@48..50 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_variable_ignore_missing_with_hash_only() {
        check_parse(
            r#"{% include some_var ignore missing with {'foo': 'bar'} only %}"#,
            expect![[r#"
                ROOT@0..62
                  TWIG_INCLUDE@0..62
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..19
                      TWIG_LITERAL_NAME@10..19
                        TK_WHITESPACE@10..11 " "
                        TK_WORD@11..19 "some_var"
                    TK_WHITESPACE@19..20 " "
                    TK_IGNORE_MISSING@20..34 "ignore missing"
                    TWIG_INCLUDE_WITH@34..54
                      TK_WHITESPACE@34..35 " "
                      TK_WITH@35..39 "with"
                      TWIG_EXPRESSION@39..54
                        TWIG_LITERAL_HASH@39..54
                          TK_WHITESPACE@39..40 " "
                          TK_OPEN_CURLY@40..41 "{"
                          TWIG_LITERAL_HASH_PAIR@41..53
                            TWIG_LITERAL_HASH_KEY@41..46
                              TWIG_LITERAL_STRING@41..46
                                TK_SINGLE_QUOTES@41..42 "'"
                                TWIG_LITERAL_STRING_INNER@42..45
                                  TK_WORD@42..45 "foo"
                                TK_SINGLE_QUOTES@45..46 "'"
                            TK_COLON@46..47 ":"
                            TWIG_EXPRESSION@47..53
                              TWIG_LITERAL_STRING@47..53
                                TK_WHITESPACE@47..48 " "
                                TK_SINGLE_QUOTES@48..49 "'"
                                TWIG_LITERAL_STRING_INNER@49..52
                                  TK_WORD@49..52 "bar"
                                TK_SINGLE_QUOTES@52..53 "'"
                          TK_CLOSE_CURLY@53..54 "}"
                    TK_WHITESPACE@54..55 " "
                    TK_ONLY@55..59 "only"
                    TK_WHITESPACE@59..60 " "
                    TK_PERCENT_CURLY@60..62 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_include_missing_template() {
        check_parse(
            r#"{% include %}"#,
            expect![[r#"
            ROOT@0..13
              TWIG_INCLUDE@0..13
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TK_WHITESPACE@10..11 " "
                TK_PERCENT_CURLY@11..13 "%}"
            error at 11..13: expected twig expression as template name but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_include_missing_with_value() {
        check_parse(
            r#"{% include 'template.html' with %}"#,
            expect![[r#"
                ROOT@0..34
                  TWIG_INCLUDE@0..34
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..31
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                    TK_WHITESPACE@31..32 " "
                    TK_PERCENT_CURLY@32..34 "%}"
                error at 32..34: expected twig expression as with value but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_include_array() {
        check_parse(
            r#"{% include ['page_detailed.html', 'page.html'] %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_INCLUDE@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..46
                      TWIG_LITERAL_ARRAY@10..46
                        TK_WHITESPACE@10..11 " "
                        TK_OPEN_SQUARE@11..12 "["
                        TWIG_EXPRESSION@12..32
                          TWIG_LITERAL_STRING@12..32
                            TK_SINGLE_QUOTES@12..13 "'"
                            TWIG_LITERAL_STRING_INNER@13..31
                              TK_WORD@13..26 "page_detailed"
                              TK_DOT@26..27 "."
                              TK_WORD@27..31 "html"
                            TK_SINGLE_QUOTES@31..32 "'"
                        TK_COMMA@32..33 ","
                        TWIG_EXPRESSION@33..45
                          TWIG_LITERAL_STRING@33..45
                            TK_WHITESPACE@33..34 " "
                            TK_SINGLE_QUOTES@34..35 "'"
                            TWIG_LITERAL_STRING_INNER@35..44
                              TK_WORD@35..39 "page"
                              TK_DOT@39..40 "."
                              TK_WORD@40..44 "html"
                            TK_SINGLE_QUOTES@44..45 "'"
                        TK_CLOSE_SQUARE@45..46 "]"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_use_string() {
        check_parse(
            r#"{% use "blocks.html" %}"#,
            expect![[r#"
            ROOT@0..23
              TWIG_USE@0..23
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_USE@3..6 "use"
                TWIG_LITERAL_STRING@6..20
                  TK_WHITESPACE@6..7 " "
                  TK_DOUBLE_QUOTES@7..8 "\""
                  TWIG_LITERAL_STRING_INNER@8..19
                    TK_WORD@8..14 "blocks"
                    TK_DOT@14..15 "."
                    TK_WORD@15..19 "html"
                  TK_DOUBLE_QUOTES@19..20 "\""
                TK_WHITESPACE@20..21 " "
                TK_PERCENT_CURLY@21..23 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_use_interpolated_string() {
        // should not! be parsed as an interpolated string, because here only plain strings are allowed
        // also should add a parser error that this is not supported
        check_parse(
            r#"{% use "blocks#{1+1}.html" %}"#,
            expect![[r##"
            ROOT@0..29
              TWIG_USE@0..29
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_USE@3..6 "use"
                TWIG_LITERAL_STRING@6..26
                  TK_WHITESPACE@6..7 " "
                  TK_DOUBLE_QUOTES@7..8 "\""
                  TWIG_LITERAL_STRING_INNER@8..25
                    TK_WORD@8..14 "blocks"
                    TK_HASHTAG_OPEN_CURLY@14..16 "#{"
                    TK_NUMBER@16..17 "1"
                    TK_PLUS@17..18 "+"
                    TK_NUMBER@18..19 "1"
                    TK_CLOSE_CURLY@19..20 "}"
                    TK_DOT@20..21 "."
                    TK_WORD@21..25 "html"
                  TK_DOUBLE_QUOTES@25..26 "\""
                TK_WHITESPACE@26..27 " "
                TK_PERCENT_CURLY@27..29 "%}"
            error at 14..16: expected no string interpolation, because it isn't allowed here but found #{"##]],
        )
    }

    #[test]
    fn parse_twig_use_string_with_as() {
        check_parse(
            r#"{% use "blocks.html" with sidebar as base_sidebar, title as base_title %}"#,
            expect![[r#"
                ROOT@0..73
                  TWIG_USE@0..73
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_USE@3..6 "use"
                    TWIG_LITERAL_STRING@6..20
                      TK_WHITESPACE@6..7 " "
                      TK_DOUBLE_QUOTES@7..8 "\""
                      TWIG_LITERAL_STRING_INNER@8..19
                        TK_WORD@8..14 "blocks"
                        TK_DOT@14..15 "."
                        TK_WORD@15..19 "html"
                      TK_DOUBLE_QUOTES@19..20 "\""
                    TK_WHITESPACE@20..21 " "
                    TK_WITH@21..25 "with"
                    TWIG_USE_OVERRIDE@25..49
                      TWIG_LITERAL_NAME@25..33
                        TK_WHITESPACE@25..26 " "
                        TK_WORD@26..33 "sidebar"
                      TK_WHITESPACE@33..34 " "
                      TK_AS@34..36 "as"
                      TWIG_LITERAL_NAME@36..49
                        TK_WHITESPACE@36..37 " "
                        TK_WORD@37..49 "base_sidebar"
                    TK_COMMA@49..50 ","
                    TWIG_USE_OVERRIDE@50..70
                      TWIG_LITERAL_NAME@50..56
                        TK_WHITESPACE@50..51 " "
                        TK_WORD@51..56 "title"
                      TK_WHITESPACE@56..57 " "
                      TK_AS@57..59 "as"
                      TWIG_LITERAL_NAME@59..70
                        TK_WHITESPACE@59..60 " "
                        TK_WORD@60..70 "base_title"
                    TK_WHITESPACE@70..71 " "
                    TK_PERCENT_CURLY@71..73 "%}""#]],
        )
    }

    #[test]
    fn parse_twig_use_string_with_missing() {
        check_parse(
            r#"{% use "blocks.html" with %}"#,
            expect![[r#"
            ROOT@0..28
              TWIG_USE@0..28
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_USE@3..6 "use"
                TWIG_LITERAL_STRING@6..20
                  TK_WHITESPACE@6..7 " "
                  TK_DOUBLE_QUOTES@7..8 "\""
                  TWIG_LITERAL_STRING_INNER@8..19
                    TK_WORD@8..14 "blocks"
                    TK_DOT@14..15 "."
                    TK_WORD@15..19 "html"
                  TK_DOUBLE_QUOTES@19..20 "\""
                TK_WHITESPACE@20..21 " "
                TK_WITH@21..25 "with"
                TK_WHITESPACE@25..26 " "
                TK_PERCENT_CURLY@26..28 "%}"
            error at 26..28: expected at least one block name as block name but found %}"#]],
        )
    }

    #[test]
    fn parse_twig_use_string_with_name_as_missing() {
        check_parse(
            r#"{% use "blocks.html" with a %}"#,
            expect![[r#"
            ROOT@0..30
              TWIG_USE@0..30
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_USE@3..6 "use"
                TWIG_LITERAL_STRING@6..20
                  TK_WHITESPACE@6..7 " "
                  TK_DOUBLE_QUOTES@7..8 "\""
                  TWIG_LITERAL_STRING_INNER@8..19
                    TK_WORD@8..14 "blocks"
                    TK_DOT@14..15 "."
                    TK_WORD@15..19 "html"
                  TK_DOUBLE_QUOTES@19..20 "\""
                TK_WHITESPACE@20..21 " "
                TK_WITH@21..25 "with"
                TWIG_USE_OVERRIDE@25..27
                  TWIG_LITERAL_NAME@25..27
                    TK_WHITESPACE@25..26 " "
                    TK_WORD@26..27 "a"
                TK_WHITESPACE@27..28 " "
                TK_PERCENT_CURLY@28..30 "%}"
            error at 28..30: expected as but found %}
            error at 28..30: expected block name but found %}"#]],
        )
    }
}

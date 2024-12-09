use crate::grammar::parse_many;
use crate::grammar::twig::expression::{parse_twig_expression, TWIG_EXPRESSION_RECOVERY_SET};
use crate::parser::event::CompletedMarker;
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;
use regex::Regex;
use std::sync::LazyLock;

// TODO: maybe allow more here to partly support twig.js. Needs testing on real world templates
pub static TWIG_NAME_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*$").unwrap());

pub(crate) fn parse_twig_literal(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T![number]) {
        Some(parse_twig_number(parser))
    } else if parser.at_set(&[T!["\""], T!["'"]]) {
        Some(parse_twig_string(parser, true))
    } else if parser.at(T!["["]) {
        Some(parse_twig_array(parser))
    } else if parser.at(T!["null"]) {
        Some(parse_twig_null(parser))
    } else if parser.at_set(&[T!["true"], T!["false"]]) {
        Some(parse_twig_boolean(parser))
    } else if parser.at(T!["{"]) {
        Some(parse_twig_hash(parser))
    } else {
        // literal name
        let mut node = parse_twig_name(parser)?;

        // check for optional function call or arrow function
        if parser.at(T!["("]) {
            node = parse_twig_function(parser, node);
        } else if parser.at(T!["=>"]) {
            // wrap literal name node
            let m = parser.precede(node);
            let last_node = parser.complete(m, SyntaxKind::TWIG_ARGUMENTS);
            node = parse_twig_arrow_function(parser, last_node);
        }

        Some(node)
    }
}

/// Parses any amount of postfix operators like accessors (.), indexer ([]), function `calls()` or filters(|)
pub(crate) fn parse_postfix_operators(
    parser: &mut Parser,
    mut node: CompletedMarker,
) -> CompletedMarker {
    parse_many(
        parser,
        |_| false,
        |p| {
            if p.at(T!["."]) {
                node = parse_twig_accessor(p, node.clone());
            } else if p.at(T!["["]) {
                node = parse_twig_indexer(p, node.clone());
            } else if p.at(T!["|"]) {
                node = parse_twig_filter(p, node.clone());
            }
        },
    );

    node
}

fn parse_twig_number(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T![number]));
    let m = parser.start();
    parser.bump();

    parser.complete(m, SyntaxKind::TWIG_LITERAL_NUMBER)
}

pub(crate) fn parse_twig_string(
    parser: &mut Parser,
    mut interpolation_allowed: bool,
) -> CompletedMarker {
    debug_assert!(parser.at_set(&[T!["\""], T!["'"]]));
    let m = parser.start();
    let starting_quote_token = parser.bump();
    let quote_kind = starting_quote_token.kind;
    interpolation_allowed = match quote_kind {
        T!["\""] => interpolation_allowed,
        _ => false, // interpolation only allowed in double quoted strings,
    };

    let m_inner = parser.start();
    parse_many(
        parser,
        |p| p.at(quote_kind),
        |p| {
            if p.at_following(&[T!["\\"], quote_kind]) {
                // escaped quote should be consumed
                p.bump();
                p.bump();
            } else if p.at_following(&[T!["#"], T!["{"]]) {
                if !interpolation_allowed {
                    let opening_token = p.bump();
                    let interpolation_error = ParseErrorBuilder::new(
                        "no string interpolation, because it isn't allowed here",
                    )
                    .at_token(opening_token);
                    p.add_error(interpolation_error);
                    return;
                }

                // found twig expression in string (string interpolation)
                p.explicitly_consume_trivia(); // keep trivia out of interpolation node (its part of the raw string)
                let interpolation_m = p.start();
                p.bump(); // bump both starting tokens
                p.bump();
                if parse_twig_expression(p).is_none() {
                    p.add_error(ParseErrorBuilder::new("twig expression"));
                }
                p.expect(T!["}"], TWIG_EXPRESSION_RECOVERY_SET);
                p.complete(
                    interpolation_m,
                    SyntaxKind::TWIG_LITERAL_STRING_INTERPOLATION,
                );
            } else {
                // bump the token inside the string
                p.bump();
            }
        },
    );
    parser.explicitly_consume_trivia(); // consume any trailing trivia inside the string
    parser.complete(m_inner, SyntaxKind::TWIG_LITERAL_STRING_INNER);

    parser.expect(quote_kind, TWIG_EXPRESSION_RECOVERY_SET);
    parser.complete(m, SyntaxKind::TWIG_LITERAL_STRING)
}

fn parse_twig_array(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["["]));
    let m = parser.start();
    parser.bump();

    // parse any amount of array values
    let list_m = parser.start();
    parse_many(
        parser,
        |p| p.at(T!["]"]),
        |p| {
            parse_twig_expression(p);

            if p.at(T![","]) {
                // consume separator
                p.bump();
            } else if !p.at(T!["]"]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );
    parser.complete(list_m, SyntaxKind::TWIG_LITERAL_ARRAY_INNER);

    parser.expect(T!["]"], TWIG_EXPRESSION_RECOVERY_SET);
    parser.complete(m, SyntaxKind::TWIG_LITERAL_ARRAY)
}

fn parse_twig_null(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["null"]));
    let m = parser.start();
    parser.bump();

    parser.complete(m, SyntaxKind::TWIG_LITERAL_NULL)
}

fn parse_twig_boolean(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at_set(&[T!["true"], T!["false"]]));
    let m = parser.start();
    parser.bump();

    parser.complete(m, SyntaxKind::TWIG_LITERAL_BOOLEAN)
}

fn parse_twig_hash(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["{"]));
    let m = parser.start();
    parser.bump();

    // parse any amount of hash pairs
    let list_m = parser.start();
    parse_many(
        parser,
        |p| p.at(T!["}"]),
        |p| {
            parse_twig_hash_pair(p);

            if p.at(T![","]) {
                // consume separator
                p.bump();
            } else if !p.at(T!["}"]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );
    parser.complete(list_m, SyntaxKind::TWIG_LITERAL_HASH_ITEMS);

    parser.expect(T!["}"], TWIG_EXPRESSION_RECOVERY_SET);
    parser.complete(m, SyntaxKind::TWIG_LITERAL_HASH)
}

fn parse_twig_hash_pair(parser: &mut Parser) -> Option<CompletedMarker> {
    let key = if parser.at(T![number]) {
        let m = parse_twig_number(parser);
        let preceded = parser.precede(m);
        parser.complete(preceded, SyntaxKind::TWIG_LITERAL_HASH_KEY)
    } else if parser.at_set(&[T!["'"], T!["\""]]) {
        let m = parse_twig_string(parser, false); // no interpolation in keys
        let preceded = parser.precede(m);
        parser.complete(preceded, SyntaxKind::TWIG_LITERAL_HASH_KEY)
    } else if parser.at(T!["("]) {
        let m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression"));
            parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
        }
        parser.expect(T![")"], TWIG_EXPRESSION_RECOVERY_SET);
        parser.complete(m, SyntaxKind::TWIG_LITERAL_HASH_KEY)
    } else {
        let token_text = parser.peek_token()?.text;
        if TWIG_NAME_REGEX.is_match(token_text) {
            let m = parser.start();
            parser.bump_as(SyntaxKind::TK_WORD);
            parser.complete(m, SyntaxKind::TWIG_LITERAL_HASH_KEY)
        } else {
            return None;
        }
    };

    // check if key exists
    if parser.at(T![":"]) {
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("value as twig expression"));
            parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
        }
    }

    let preceded = parser.precede(key);
    Some(parser.complete(preceded, SyntaxKind::TWIG_LITERAL_HASH_PAIR))
}

pub(crate) fn parse_twig_filter(
    parser: &mut Parser,
    mut last_node: CompletedMarker,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["|"]));

    // wrap last_node in an operand and create outer marker
    let m = parser.precede(last_node);
    last_node = parser.complete(m, SyntaxKind::TWIG_OPERAND);
    let outer = parser.precede(last_node);

    // bump the operator
    parser.bump();

    // parse the rhs and wrap it also in an operand
    let m = parser.start();
    if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig filter"));
        parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
    } else if parser.at(T!["("]) {
        // parse any amount of arguments
        let arguments_m = parser.start();
        parser.bump();
        parse_many(
            parser,
            |p| p.at(T![")"]),
            |p| {
                parse_twig_function_argument(p);
                if p.at(T![","]) {
                    p.bump();
                } else if !p.at(T![")"]) {
                    p.add_error(ParseErrorBuilder::new(","));
                }
            },
        );
        parser.expect(T![")"], TWIG_EXPRESSION_RECOVERY_SET);
        parser.complete(arguments_m, SyntaxKind::TWIG_ARGUMENTS);
    }
    parser.complete(m, SyntaxKind::TWIG_OPERAND);

    // complete the outer marker
    parser.complete(outer, SyntaxKind::TWIG_FILTER)
}

fn parse_twig_indexer(parser: &mut Parser, mut last_node: CompletedMarker) -> CompletedMarker {
    debug_assert!(parser.at(T!["["]));

    // wrap last_node in an operand and create outer marker
    let m = parser.precede(last_node);
    last_node = parser.complete(m, SyntaxKind::TWIG_OPERAND);
    let outer = parser.precede(last_node);

    // bump the opening '['
    parser.bump();

    let index_m = parser.start();

    // try to parse the first and maybe only expression
    let missing_lower_slice_bound = parse_twig_expression(parser).is_none();

    // look for range syntax and try to parse the second and maybe only expression
    let mut is_slice = false;
    let missing_upper_slice_bound = if parser.at(T![":"]) {
        parser.bump();
        is_slice = true; // now it is a slice instead of a simple lookup
        parse_twig_expression(parser).is_none()
    } else {
        true
    };

    if missing_lower_slice_bound && missing_upper_slice_bound {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
    }

    parser.complete(
        index_m,
        if is_slice {
            SyntaxKind::TWIG_INDEX_RANGE
        } else {
            SyntaxKind::TWIG_INDEX
        },
    );

    parser.expect(T!["]"], TWIG_EXPRESSION_RECOVERY_SET);

    // complete the outer marker
    parser.complete(outer, SyntaxKind::TWIG_INDEX_LOOKUP)
}

fn parse_twig_accessor(parser: &mut Parser, mut last_node: CompletedMarker) -> CompletedMarker {
    debug_assert!(parser.at(T!["."]));

    // wrap last_node in an operand and create outer marker
    let m = parser.precede(last_node);
    last_node = parser.complete(m, SyntaxKind::TWIG_OPERAND);
    let outer = parser.precede(last_node);

    // bump the operator
    parser.bump();

    // parse the rhs and wrap it also in an operand
    let m = parser.start();
    if parser.at(T![number]) {
        // found an array index lookup instead of name accessor!
        let n = parse_twig_number(parser);
        let n = parser.precede(n);
        parser.complete(n, SyntaxKind::TWIG_EXPRESSION);
        parser.complete(m, SyntaxKind::TWIG_INDEX);
        let node = parser.complete(outer, SyntaxKind::TWIG_INDEX_LOOKUP);
        return node;
    } else if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new(
            "twig variable property, key or method",
        ));
        parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
    }
    parser.complete(m, SyntaxKind::TWIG_OPERAND);

    // complete the outer marker
    let mut node = parser.complete(outer, SyntaxKind::TWIG_ACCESSOR);

    // check for optional function call
    if parser.at(T!["("]) {
        node = parse_twig_function(parser, node);
    }

    node
}

pub(crate) fn parse_twig_function(
    parser: &mut Parser,
    mut last_node: CompletedMarker,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["("]));

    // wrap last_node in an operand and create outer marker
    let m = parser.precede(last_node);
    last_node = parser.complete(m, SyntaxKind::TWIG_OPERAND);
    let outer = parser.precede(last_node);

    // parse any amount of arguments
    let arguments_m = parser.start();
    // bump the opening '('
    parser.bump();
    parse_many(
        parser,
        |p| p.at(T![")"]),
        |p| {
            parse_twig_function_argument(p);
            if p.at(T![","]) {
                p.bump();
            } else if !p.at(T![")"]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );
    parser.expect(T![")"], TWIG_EXPRESSION_RECOVERY_SET);
    parser.complete(arguments_m, SyntaxKind::TWIG_ARGUMENTS);

    // complete the outer marker
    parser.complete(outer, SyntaxKind::TWIG_FUNCTION_CALL)
}

pub(crate) fn parse_twig_arrow_function(
    parser: &mut Parser,
    last_node: CompletedMarker,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["=>"]));

    let outer = parser.precede(last_node);

    // bump the arrow
    parser.bump();

    // parse closure expression
    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new(
            "single twig expression as the body of the closure",
        ));
        parser.recover(TWIG_EXPRESSION_RECOVERY_SET);
    }

    // complete the outer marker
    parser.complete(outer, SyntaxKind::TWIG_ARROW_FUNCTION)
}

pub(crate) fn parse_twig_function_argument(parser: &mut Parser) -> Option<CompletedMarker> {
    // must be specific here with word followed by equal, because otherwise it could
    // be a normal variable or another function call or something else..
    if parser.at_following(&[T![word], T!["="]]) {
        let named_arg_m = parser.start();
        parser.bump();
        parser.expect(T!["="], TWIG_EXPRESSION_RECOVERY_SET);
        parse_twig_expression(parser);
        Some(parser.complete(named_arg_m, SyntaxKind::TWIG_NAMED_ARGUMENT))
    } else {
        parse_twig_expression(parser)
    }
}

pub(crate) fn parse_twig_name(parser: &mut Parser) -> Option<CompletedMarker> {
    // special case to allow for 'same as' and 'divisible by' twig test ('is' / 'is not' operator)
    let is_at_special = parser.at_set(&[T!["same as"], T!["divisible by"]]);
    let token_text = parser.peek_token()?.text;
    if !is_at_special && !TWIG_NAME_REGEX.is_match(token_text) {
        return None;
    }

    let m = parser.start();
    parser.bump_as(SyntaxKind::TK_WORD);
    let m = parser.complete(m, SyntaxKind::TWIG_LITERAL_NAME);
    Some(m)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;

    #[test]
    fn parse_twig_string_single_quotes() {
        check_parse(
            r#"{{ 'hel"lo world' }}"#,
            expect![[r#"
                ROOT@0..20
                  TWIG_VAR@0..20
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..17
                      TWIG_LITERAL_STRING@2..17
                        TK_WHITESPACE@2..3 " "
                        TK_SINGLE_QUOTES@3..4 "'"
                        TWIG_LITERAL_STRING_INNER@4..16
                          TK_WORD@4..7 "hel"
                          TK_DOUBLE_QUOTES@7..8 "\""
                          TK_WORD@8..10 "lo"
                          TK_WHITESPACE@10..11 " "
                          TK_WORD@11..16 "world"
                        TK_SINGLE_QUOTES@16..17 "'"
                    TK_WHITESPACE@17..18 " "
                    TK_CLOSE_CURLY_CURLY@18..20 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_string_double_quotes() {
        check_parse(
            r#"{{ "hel'lo world" }}"#,
            expect![[r#"
                ROOT@0..20
                  TWIG_VAR@0..20
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..17
                      TWIG_LITERAL_STRING@2..17
                        TK_WHITESPACE@2..3 " "
                        TK_DOUBLE_QUOTES@3..4 "\""
                        TWIG_LITERAL_STRING_INNER@4..16
                          TK_WORD@4..7 "hel"
                          TK_SINGLE_QUOTES@7..8 "'"
                          TK_WORD@8..10 "lo"
                          TK_WHITESPACE@10..11 " "
                          TK_WORD@11..16 "world"
                        TK_DOUBLE_QUOTES@16..17 "\""
                    TK_WHITESPACE@17..18 " "
                    TK_CLOSE_CURLY_CURLY@18..20 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_string_escaped_double_quotes() {
        check_parse(
            r#"{{ "hel\"lo world" }}"#,
            expect![[r#"
                ROOT@0..21
                  TWIG_VAR@0..21
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..18
                      TWIG_LITERAL_STRING@2..18
                        TK_WHITESPACE@2..3 " "
                        TK_DOUBLE_QUOTES@3..4 "\""
                        TWIG_LITERAL_STRING_INNER@4..17
                          TK_WORD@4..7 "hel"
                          TK_BACKWARD_SLASH@7..8 "\\"
                          TK_DOUBLE_QUOTES@8..9 "\""
                          TK_WORD@9..11 "lo"
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..17 "world"
                        TK_DOUBLE_QUOTES@17..18 "\""
                    TK_WHITESPACE@18..19 " "
                    TK_CLOSE_CURLY_CURLY@19..21 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_string_with_leading_and_trailing_trivia() {
        check_parse(
            r#"{{ " , " }}"#,
            expect![[r#"
                ROOT@0..11
                  TWIG_VAR@0..11
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..8
                      TWIG_LITERAL_STRING@2..8
                        TK_WHITESPACE@2..3 " "
                        TK_DOUBLE_QUOTES@3..4 "\""
                        TWIG_LITERAL_STRING_INNER@4..7
                          TK_WHITESPACE@4..5 " "
                          TK_COMMA@5..6 ","
                          TK_WHITESPACE@6..7 " "
                        TK_DOUBLE_QUOTES@7..8 "\""
                    TK_WHITESPACE@8..9 " "
                    TK_CLOSE_CURLY_CURLY@9..11 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_string_interpolation() {
        check_parse(
            r#"{{ "foo #{1 + 2} baz" }}"#,
            expect![[r##"
                ROOT@0..24
                  TWIG_VAR@0..24
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..21
                      TWIG_LITERAL_STRING@2..21
                        TK_WHITESPACE@2..3 " "
                        TK_DOUBLE_QUOTES@3..4 "\""
                        TWIG_LITERAL_STRING_INNER@4..20
                          TK_WORD@4..7 "foo"
                          TK_WHITESPACE@7..8 " "
                          TWIG_LITERAL_STRING_INTERPOLATION@8..16
                            TK_HASHTAG@8..9 "#"
                            TK_OPEN_CURLY@9..10 "{"
                            TWIG_EXPRESSION@10..15
                              TWIG_BINARY_EXPRESSION@10..15
                                TWIG_EXPRESSION@10..11
                                  TWIG_LITERAL_NUMBER@10..11
                                    TK_NUMBER@10..11 "1"
                                TK_WHITESPACE@11..12 " "
                                TK_PLUS@12..13 "+"
                                TWIG_EXPRESSION@13..15
                                  TWIG_LITERAL_NUMBER@13..15
                                    TK_WHITESPACE@13..14 " "
                                    TK_NUMBER@14..15 "2"
                            TK_CLOSE_CURLY@15..16 "}"
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                        TK_DOUBLE_QUOTES@20..21 "\""
                    TK_WHITESPACE@21..22 " "
                    TK_CLOSE_CURLY_CURLY@22..24 "}}""##]],
        );
    }

    #[test]
    fn parse_twig_string_interpolation_missing_expression() {
        check_parse(
            r#"{{ "foo #{ } baz" }}"#,
            expect![[r##"
                ROOT@0..20
                  TWIG_VAR@0..20
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..17
                      TWIG_LITERAL_STRING@2..17
                        TK_WHITESPACE@2..3 " "
                        TK_DOUBLE_QUOTES@3..4 "\""
                        TWIG_LITERAL_STRING_INNER@4..16
                          TK_WORD@4..7 "foo"
                          TK_WHITESPACE@7..8 " "
                          TWIG_LITERAL_STRING_INTERPOLATION@8..12
                            TK_HASHTAG@8..9 "#"
                            TK_OPEN_CURLY@9..10 "{"
                            TK_WHITESPACE@10..11 " "
                            TK_CLOSE_CURLY@11..12 "}"
                          TK_WHITESPACE@12..13 " "
                          TK_WORD@13..16 "baz"
                        TK_DOUBLE_QUOTES@16..17 "\""
                    TK_WHITESPACE@17..18 " "
                    TK_CLOSE_CURLY_CURLY@18..20 "}}"
                error at 11..12: expected twig expression but found }"##]],
        );
    }

    #[test]
    fn parse_twig_integer_number() {
        check_parse(
            "{{ 42 }}",
            expect![[r#"
                ROOT@0..8
                  TWIG_VAR@0..8
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..5
                      TWIG_LITERAL_NUMBER@2..5
                        TK_WHITESPACE@2..3 " "
                        TK_NUMBER@3..5 "42"
                    TK_WHITESPACE@5..6 " "
                    TK_CLOSE_CURLY_CURLY@6..8 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_floating_point_number() {
        check_parse(
            "{{ 0.3337 }}",
            expect![[r#"
                ROOT@0..12
                  TWIG_VAR@0..12
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..9
                      TWIG_LITERAL_NUMBER@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_NUMBER@3..9 "0.3337"
                    TK_WHITESPACE@9..10 " "
                    TK_CLOSE_CURLY_CURLY@10..12 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_number_array() {
        check_parse(
            "{{ [1, 2, 3] }}",
            expect![[r#"
                ROOT@0..15
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..12
                      TWIG_LITERAL_ARRAY@2..12
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_SQUARE@3..4 "["
                        TWIG_LITERAL_ARRAY_INNER@4..11
                          TWIG_EXPRESSION@4..5
                            TWIG_LITERAL_NUMBER@4..5
                              TK_NUMBER@4..5 "1"
                          TK_COMMA@5..6 ","
                          TWIG_EXPRESSION@6..8
                            TWIG_LITERAL_NUMBER@6..8
                              TK_WHITESPACE@6..7 " "
                              TK_NUMBER@7..8 "2"
                          TK_COMMA@8..9 ","
                          TWIG_EXPRESSION@9..11
                            TWIG_LITERAL_NUMBER@9..11
                              TK_WHITESPACE@9..10 " "
                              TK_NUMBER@10..11 "3"
                        TK_CLOSE_SQUARE@11..12 "]"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_number_array_missing_comma() {
        check_parse(
            "{{ [1, 2 3] }}",
            expect![[r#"
                ROOT@0..14
                  TWIG_VAR@0..14
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..11
                      TWIG_LITERAL_ARRAY@2..11
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_SQUARE@3..4 "["
                        TWIG_LITERAL_ARRAY_INNER@4..10
                          TWIG_EXPRESSION@4..5
                            TWIG_LITERAL_NUMBER@4..5
                              TK_NUMBER@4..5 "1"
                          TK_COMMA@5..6 ","
                          TWIG_EXPRESSION@6..8
                            TWIG_LITERAL_NUMBER@6..8
                              TK_WHITESPACE@6..7 " "
                              TK_NUMBER@7..8 "2"
                          TWIG_EXPRESSION@8..10
                            TWIG_LITERAL_NUMBER@8..10
                              TK_WHITESPACE@8..9 " "
                              TK_NUMBER@9..10 "3"
                        TK_CLOSE_SQUARE@10..11 "]"
                    TK_WHITESPACE@11..12 " "
                    TK_CLOSE_CURLY_CURLY@12..14 "}}"
                error at 9..10: expected , but found number"#]],
        );
    }

    #[test]
    fn parse_twig_string_array() {
        check_parse(
            r#"{{ ["hello", "trailing", "comma",] }}"#,
            expect![[r#"
                ROOT@0..37
                  TWIG_VAR@0..37
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..34
                      TWIG_LITERAL_ARRAY@2..34
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_SQUARE@3..4 "["
                        TWIG_LITERAL_ARRAY_INNER@4..33
                          TWIG_EXPRESSION@4..11
                            TWIG_LITERAL_STRING@4..11
                              TK_DOUBLE_QUOTES@4..5 "\""
                              TWIG_LITERAL_STRING_INNER@5..10
                                TK_WORD@5..10 "hello"
                              TK_DOUBLE_QUOTES@10..11 "\""
                          TK_COMMA@11..12 ","
                          TWIG_EXPRESSION@12..23
                            TWIG_LITERAL_STRING@12..23
                              TK_WHITESPACE@12..13 " "
                              TK_DOUBLE_QUOTES@13..14 "\""
                              TWIG_LITERAL_STRING_INNER@14..22
                                TK_WORD@14..22 "trailing"
                              TK_DOUBLE_QUOTES@22..23 "\""
                          TK_COMMA@23..24 ","
                          TWIG_EXPRESSION@24..32
                            TWIG_LITERAL_STRING@24..32
                              TK_WHITESPACE@24..25 " "
                              TK_DOUBLE_QUOTES@25..26 "\""
                              TWIG_LITERAL_STRING_INNER@26..31
                                TK_WORD@26..31 "comma"
                              TK_DOUBLE_QUOTES@31..32 "\""
                          TK_COMMA@32..33 ","
                        TK_CLOSE_SQUARE@33..34 "]"
                    TK_WHITESPACE@34..35 " "
                    TK_CLOSE_CURLY_CURLY@35..37 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_null() {
        check_parse(
            "{{ null }}",
            expect![[r#"
            ROOT@0..10
              TWIG_VAR@0..10
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..7
                  TWIG_LITERAL_NULL@2..7
                    TK_WHITESPACE@2..3 " "
                    TK_NULL@3..7 "null"
                TK_WHITESPACE@7..8 " "
                TK_CLOSE_CURLY_CURLY@8..10 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_boolean_true() {
        check_parse(
            "{{ true }}",
            expect![[r#"
            ROOT@0..10
              TWIG_VAR@0..10
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..7
                  TWIG_LITERAL_BOOLEAN@2..7
                    TK_WHITESPACE@2..3 " "
                    TK_TRUE@3..7 "true"
                TK_WHITESPACE@7..8 " "
                TK_CLOSE_CURLY_CURLY@8..10 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_boolean_false() {
        check_parse(
            "{{ false }}",
            expect![[r#"
            ROOT@0..11
              TWIG_VAR@0..11
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..8
                  TWIG_LITERAL_BOOLEAN@2..8
                    TK_WHITESPACE@2..3 " "
                    TK_FALSE@3..8 "false"
                TK_WHITESPACE@8..9 " "
                TK_CLOSE_CURLY_CURLY@9..11 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_number_hash() {
        check_parse(
            "{{ { 1: 'hello', 2: 'world' } }}",
            expect![[r#"
                ROOT@0..32
                  TWIG_VAR@0..32
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..29
                      TWIG_LITERAL_HASH@2..29
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..27
                          TWIG_LITERAL_HASH_PAIR@4..15
                            TWIG_LITERAL_HASH_KEY@4..6
                              TWIG_LITERAL_NUMBER@4..6
                                TK_WHITESPACE@4..5 " "
                                TK_NUMBER@5..6 "1"
                            TK_COLON@6..7 ":"
                            TWIG_EXPRESSION@7..15
                              TWIG_LITERAL_STRING@7..15
                                TK_WHITESPACE@7..8 " "
                                TK_SINGLE_QUOTES@8..9 "'"
                                TWIG_LITERAL_STRING_INNER@9..14
                                  TK_WORD@9..14 "hello"
                                TK_SINGLE_QUOTES@14..15 "'"
                          TK_COMMA@15..16 ","
                          TWIG_LITERAL_HASH_PAIR@16..27
                            TWIG_LITERAL_HASH_KEY@16..18
                              TWIG_LITERAL_NUMBER@16..18
                                TK_WHITESPACE@16..17 " "
                                TK_NUMBER@17..18 "2"
                            TK_COLON@18..19 ":"
                            TWIG_EXPRESSION@19..27
                              TWIG_LITERAL_STRING@19..27
                                TK_WHITESPACE@19..20 " "
                                TK_SINGLE_QUOTES@20..21 "'"
                                TWIG_LITERAL_STRING_INNER@21..26
                                  TK_WORD@21..26 "world"
                                TK_SINGLE_QUOTES@26..27 "'"
                        TK_WHITESPACE@27..28 " "
                        TK_CLOSE_CURLY@28..29 "}"
                    TK_WHITESPACE@29..30 " "
                    TK_CLOSE_CURLY_CURLY@30..32 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_string_hash() {
        check_parse(
            "{{ { 'hello': 42, 'world': 33 } }}",
            expect![[r#"
                ROOT@0..34
                  TWIG_VAR@0..34
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..31
                      TWIG_LITERAL_HASH@2..31
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..29
                          TWIG_LITERAL_HASH_PAIR@4..16
                            TWIG_LITERAL_HASH_KEY@4..12
                              TWIG_LITERAL_STRING@4..12
                                TK_WHITESPACE@4..5 " "
                                TK_SINGLE_QUOTES@5..6 "'"
                                TWIG_LITERAL_STRING_INNER@6..11
                                  TK_WORD@6..11 "hello"
                                TK_SINGLE_QUOTES@11..12 "'"
                            TK_COLON@12..13 ":"
                            TWIG_EXPRESSION@13..16
                              TWIG_LITERAL_NUMBER@13..16
                                TK_WHITESPACE@13..14 " "
                                TK_NUMBER@14..16 "42"
                          TK_COMMA@16..17 ","
                          TWIG_LITERAL_HASH_PAIR@17..29
                            TWIG_LITERAL_HASH_KEY@17..25
                              TWIG_LITERAL_STRING@17..25
                                TK_WHITESPACE@17..18 " "
                                TK_SINGLE_QUOTES@18..19 "'"
                                TWIG_LITERAL_STRING_INNER@19..24
                                  TK_WORD@19..24 "world"
                                TK_SINGLE_QUOTES@24..25 "'"
                            TK_COLON@25..26 ":"
                            TWIG_EXPRESSION@26..29
                              TWIG_LITERAL_NUMBER@26..29
                                TK_WHITESPACE@26..27 " "
                                TK_NUMBER@27..29 "33"
                        TK_WHITESPACE@29..30 " "
                        TK_CLOSE_CURLY@30..31 "}"
                    TK_WHITESPACE@31..32 " "
                    TK_CLOSE_CURLY_CURLY@32..34 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_named_hash() {
        check_parse(
            "{{ { hello: 42, world: 33 } }}",
            expect![[r#"
                ROOT@0..30
                  TWIG_VAR@0..30
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..27
                      TWIG_LITERAL_HASH@2..27
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..25
                          TWIG_LITERAL_HASH_PAIR@4..14
                            TWIG_LITERAL_HASH_KEY@4..10
                              TK_WHITESPACE@4..5 " "
                              TK_WORD@5..10 "hello"
                            TK_COLON@10..11 ":"
                            TWIG_EXPRESSION@11..14
                              TWIG_LITERAL_NUMBER@11..14
                                TK_WHITESPACE@11..12 " "
                                TK_NUMBER@12..14 "42"
                          TK_COMMA@14..15 ","
                          TWIG_LITERAL_HASH_PAIR@15..25
                            TWIG_LITERAL_HASH_KEY@15..21
                              TK_WHITESPACE@15..16 " "
                              TK_WORD@16..21 "world"
                            TK_COLON@21..22 ":"
                            TWIG_EXPRESSION@22..25
                              TWIG_LITERAL_NUMBER@22..25
                                TK_WHITESPACE@22..23 " "
                                TK_NUMBER@23..25 "33"
                        TK_WHITESPACE@25..26 " "
                        TK_CLOSE_CURLY@26..27 "}"
                    TK_WHITESPACE@27..28 " "
                    TK_CLOSE_CURLY_CURLY@28..30 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_unquoted_hash_with_only_underscore() {
        check_parse(
            "{{ { valid: 42, _: 99 } }}",
            expect![[r#"
                ROOT@0..26
                  TWIG_VAR@0..26
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..23
                      TWIG_LITERAL_HASH@2..23
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..21
                          TWIG_LITERAL_HASH_PAIR@4..14
                            TWIG_LITERAL_HASH_KEY@4..10
                              TK_WHITESPACE@4..5 " "
                              TK_WORD@5..10 "valid"
                            TK_COLON@10..11 ":"
                            TWIG_EXPRESSION@11..14
                              TWIG_LITERAL_NUMBER@11..14
                                TK_WHITESPACE@11..12 " "
                                TK_NUMBER@12..14 "42"
                          TK_COMMA@14..15 ","
                          TWIG_LITERAL_HASH_PAIR@15..21
                            TWIG_LITERAL_HASH_KEY@15..17
                              TK_WHITESPACE@15..16 " "
                              TK_WORD@16..17 "_"
                            TK_COLON@17..18 ":"
                            TWIG_EXPRESSION@18..21
                              TWIG_LITERAL_NUMBER@18..21
                                TK_WHITESPACE@18..19 " "
                                TK_NUMBER@19..21 "99"
                        TK_WHITESPACE@21..22 " "
                        TK_CLOSE_CURLY@22..23 "}"
                    TK_WHITESPACE@23..24 " "
                    TK_CLOSE_CURLY_CURLY@24..26 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_expression_hash_missing_comma() {
        check_parse(
            "{{ { (15): 42 (60): 33 } }}",
            expect![[r#"
                ROOT@0..27
                  TWIG_VAR@0..27
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..24
                      TWIG_LITERAL_HASH@2..24
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..22
                          TWIG_LITERAL_HASH_PAIR@4..13
                            TWIG_LITERAL_HASH_KEY@4..9
                              TK_WHITESPACE@4..5 " "
                              TK_OPEN_PARENTHESIS@5..6 "("
                              TWIG_EXPRESSION@6..8
                                TWIG_LITERAL_NUMBER@6..8
                                  TK_NUMBER@6..8 "15"
                              TK_CLOSE_PARENTHESIS@8..9 ")"
                            TK_COLON@9..10 ":"
                            TWIG_EXPRESSION@10..13
                              TWIG_LITERAL_NUMBER@10..13
                                TK_WHITESPACE@10..11 " "
                                TK_NUMBER@11..13 "42"
                          TWIG_LITERAL_HASH_PAIR@13..22
                            TWIG_LITERAL_HASH_KEY@13..18
                              TK_WHITESPACE@13..14 " "
                              TK_OPEN_PARENTHESIS@14..15 "("
                              TWIG_EXPRESSION@15..17
                                TWIG_LITERAL_NUMBER@15..17
                                  TK_NUMBER@15..17 "60"
                              TK_CLOSE_PARENTHESIS@17..18 ")"
                            TK_COLON@18..19 ":"
                            TWIG_EXPRESSION@19..22
                              TWIG_LITERAL_NUMBER@19..22
                                TK_WHITESPACE@19..20 " "
                                TK_NUMBER@20..22 "33"
                        TK_WHITESPACE@22..23 " "
                        TK_CLOSE_CURLY@23..24 "}"
                    TK_WHITESPACE@24..25 " "
                    TK_CLOSE_CURLY_CURLY@25..27 "}}"
                error at 14..15: expected , but found ("#]],
        );
    }

    #[test]
    fn parse_twig_expression_hash_missing_whitespace() {
        check_parse(
            "{{ { '%total%':reviews.totalReviews } }}",
            expect![[r#"
            ROOT@0..40
              TWIG_VAR@0..40
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..37
                  TWIG_LITERAL_HASH@2..37
                    TK_WHITESPACE@2..3 " "
                    TK_OPEN_CURLY@3..4 "{"
                    TWIG_LITERAL_HASH_ITEMS@4..35
                      TWIG_LITERAL_HASH_PAIR@4..35
                        TWIG_LITERAL_HASH_KEY@4..14
                          TWIG_LITERAL_STRING@4..14
                            TK_WHITESPACE@4..5 " "
                            TK_SINGLE_QUOTES@5..6 "'"
                            TWIG_LITERAL_STRING_INNER@6..13
                              TK_PERCENT@6..7 "%"
                              TK_WORD@7..12 "total"
                              TK_PERCENT@12..13 "%"
                            TK_SINGLE_QUOTES@13..14 "'"
                        TK_COLON@14..15 ":"
                        TWIG_EXPRESSION@15..35
                          TWIG_ACCESSOR@15..35
                            TWIG_OPERAND@15..22
                              TWIG_LITERAL_NAME@15..22
                                TK_WORD@15..22 "reviews"
                            TK_DOT@22..23 "."
                            TWIG_OPERAND@23..35
                              TWIG_LITERAL_NAME@23..35
                                TK_WORD@23..35 "totalReviews"
                    TK_WHITESPACE@35..36 " "
                    TK_CLOSE_CURLY@36..37 "}"
                TK_WHITESPACE@37..38 " "
                TK_CLOSE_CURLY_CURLY@38..40 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_complex_expression_hash() {
        check_parse(
            "{{ { (foo): 'foo', (1 + 1): 'bar', (foo ~ 'b'): 'baz' } }}",
            expect![[r#"
                ROOT@0..58
                  TWIG_VAR@0..58
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..55
                      TWIG_LITERAL_HASH@2..55
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..53
                          TWIG_LITERAL_HASH_PAIR@4..17
                            TWIG_LITERAL_HASH_KEY@4..10
                              TK_WHITESPACE@4..5 " "
                              TK_OPEN_PARENTHESIS@5..6 "("
                              TWIG_EXPRESSION@6..9
                                TWIG_LITERAL_NAME@6..9
                                  TK_WORD@6..9 "foo"
                              TK_CLOSE_PARENTHESIS@9..10 ")"
                            TK_COLON@10..11 ":"
                            TWIG_EXPRESSION@11..17
                              TWIG_LITERAL_STRING@11..17
                                TK_WHITESPACE@11..12 " "
                                TK_SINGLE_QUOTES@12..13 "'"
                                TWIG_LITERAL_STRING_INNER@13..16
                                  TK_WORD@13..16 "foo"
                                TK_SINGLE_QUOTES@16..17 "'"
                          TK_COMMA@17..18 ","
                          TWIG_LITERAL_HASH_PAIR@18..33
                            TWIG_LITERAL_HASH_KEY@18..26
                              TK_WHITESPACE@18..19 " "
                              TK_OPEN_PARENTHESIS@19..20 "("
                              TWIG_EXPRESSION@20..25
                                TWIG_BINARY_EXPRESSION@20..25
                                  TWIG_EXPRESSION@20..21
                                    TWIG_LITERAL_NUMBER@20..21
                                      TK_NUMBER@20..21 "1"
                                  TK_WHITESPACE@21..22 " "
                                  TK_PLUS@22..23 "+"
                                  TWIG_EXPRESSION@23..25
                                    TWIG_LITERAL_NUMBER@23..25
                                      TK_WHITESPACE@23..24 " "
                                      TK_NUMBER@24..25 "1"
                              TK_CLOSE_PARENTHESIS@25..26 ")"
                            TK_COLON@26..27 ":"
                            TWIG_EXPRESSION@27..33
                              TWIG_LITERAL_STRING@27..33
                                TK_WHITESPACE@27..28 " "
                                TK_SINGLE_QUOTES@28..29 "'"
                                TWIG_LITERAL_STRING_INNER@29..32
                                  TK_WORD@29..32 "bar"
                                TK_SINGLE_QUOTES@32..33 "'"
                          TK_COMMA@33..34 ","
                          TWIG_LITERAL_HASH_PAIR@34..53
                            TWIG_LITERAL_HASH_KEY@34..46
                              TK_WHITESPACE@34..35 " "
                              TK_OPEN_PARENTHESIS@35..36 "("
                              TWIG_EXPRESSION@36..45
                                TWIG_BINARY_EXPRESSION@36..45
                                  TWIG_EXPRESSION@36..39
                                    TWIG_LITERAL_NAME@36..39
                                      TK_WORD@36..39 "foo"
                                  TK_WHITESPACE@39..40 " "
                                  TK_TILDE@40..41 "~"
                                  TWIG_EXPRESSION@41..45
                                    TWIG_LITERAL_STRING@41..45
                                      TK_WHITESPACE@41..42 " "
                                      TK_SINGLE_QUOTES@42..43 "'"
                                      TWIG_LITERAL_STRING_INNER@43..44
                                        TK_WORD@43..44 "b"
                                      TK_SINGLE_QUOTES@44..45 "'"
                              TK_CLOSE_PARENTHESIS@45..46 ")"
                            TK_COLON@46..47 ":"
                            TWIG_EXPRESSION@47..53
                              TWIG_LITERAL_STRING@47..53
                                TK_WHITESPACE@47..48 " "
                                TK_SINGLE_QUOTES@48..49 "'"
                                TWIG_LITERAL_STRING_INNER@49..52
                                  TK_WORD@49..52 "baz"
                                TK_SINGLE_QUOTES@52..53 "'"
                        TK_WHITESPACE@53..54 " "
                        TK_CLOSE_CURLY@54..55 "}"
                    TK_WHITESPACE@55..56 " "
                    TK_CLOSE_CURLY_CURLY@56..58 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_nested_hash() {
        check_parse(
            "{{ { outer: { inner: 'hello' } } }}",
            expect![[r#"
                ROOT@0..35
                  TWIG_VAR@0..35
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..32
                      TWIG_LITERAL_HASH@2..32
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..30
                          TWIG_LITERAL_HASH_PAIR@4..30
                            TWIG_LITERAL_HASH_KEY@4..10
                              TK_WHITESPACE@4..5 " "
                              TK_WORD@5..10 "outer"
                            TK_COLON@10..11 ":"
                            TWIG_EXPRESSION@11..30
                              TWIG_LITERAL_HASH@11..30
                                TK_WHITESPACE@11..12 " "
                                TK_OPEN_CURLY@12..13 "{"
                                TWIG_LITERAL_HASH_ITEMS@13..28
                                  TWIG_LITERAL_HASH_PAIR@13..28
                                    TWIG_LITERAL_HASH_KEY@13..19
                                      TK_WHITESPACE@13..14 " "
                                      TK_WORD@14..19 "inner"
                                    TK_COLON@19..20 ":"
                                    TWIG_EXPRESSION@20..28
                                      TWIG_LITERAL_STRING@20..28
                                        TK_WHITESPACE@20..21 " "
                                        TK_SINGLE_QUOTES@21..22 "'"
                                        TWIG_LITERAL_STRING_INNER@22..27
                                          TK_WORD@22..27 "hello"
                                        TK_SINGLE_QUOTES@27..28 "'"
                                TK_WHITESPACE@28..29 " "
                                TK_CLOSE_CURLY@29..30 "}"
                        TK_WHITESPACE@30..31 " "
                        TK_CLOSE_CURLY@31..32 "}"
                    TK_WHITESPACE@32..33 " "
                    TK_CLOSE_CURLY_CURLY@33..35 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_hash_with_omitted_value() {
        check_parse(
            "{{ { value, is, same, as, key } }}",
            expect![[r#"
                ROOT@0..34
                  TWIG_VAR@0..34
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..31
                      TWIG_LITERAL_HASH@2..31
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_CURLY@3..4 "{"
                        TWIG_LITERAL_HASH_ITEMS@4..29
                          TWIG_LITERAL_HASH_PAIR@4..10
                            TWIG_LITERAL_HASH_KEY@4..10
                              TK_WHITESPACE@4..5 " "
                              TK_WORD@5..10 "value"
                          TK_COMMA@10..11 ","
                          TWIG_LITERAL_HASH_PAIR@11..14
                            TWIG_LITERAL_HASH_KEY@11..14
                              TK_WHITESPACE@11..12 " "
                              TK_WORD@12..14 "is"
                          TK_COMMA@14..15 ","
                          TWIG_LITERAL_HASH_PAIR@15..20
                            TWIG_LITERAL_HASH_KEY@15..20
                              TK_WHITESPACE@15..16 " "
                              TK_WORD@16..20 "same"
                          TK_COMMA@20..21 ","
                          TWIG_LITERAL_HASH_PAIR@21..24
                            TWIG_LITERAL_HASH_KEY@21..24
                              TK_WHITESPACE@21..22 " "
                              TK_WORD@22..24 "as"
                          TK_COMMA@24..25 ","
                          TWIG_LITERAL_HASH_PAIR@25..29
                            TWIG_LITERAL_HASH_KEY@25..29
                              TK_WHITESPACE@25..26 " "
                              TK_WORD@26..29 "key"
                        TK_WHITESPACE@29..30 " "
                        TK_CLOSE_CURLY@30..31 "}"
                    TK_WHITESPACE@31..32 " "
                    TK_CLOSE_CURLY_CURLY@32..34 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_array_with_hash_mixed() {
        check_parse(
            r#"{{ [1, {"foo": "bar"}] }}"#,
            expect![[r#"
                ROOT@0..25
                  TWIG_VAR@0..25
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..22
                      TWIG_LITERAL_ARRAY@2..22
                        TK_WHITESPACE@2..3 " "
                        TK_OPEN_SQUARE@3..4 "["
                        TWIG_LITERAL_ARRAY_INNER@4..21
                          TWIG_EXPRESSION@4..5
                            TWIG_LITERAL_NUMBER@4..5
                              TK_NUMBER@4..5 "1"
                          TK_COMMA@5..6 ","
                          TWIG_EXPRESSION@6..21
                            TWIG_LITERAL_HASH@6..21
                              TK_WHITESPACE@6..7 " "
                              TK_OPEN_CURLY@7..8 "{"
                              TWIG_LITERAL_HASH_ITEMS@8..20
                                TWIG_LITERAL_HASH_PAIR@8..20
                                  TWIG_LITERAL_HASH_KEY@8..13
                                    TWIG_LITERAL_STRING@8..13
                                      TK_DOUBLE_QUOTES@8..9 "\""
                                      TWIG_LITERAL_STRING_INNER@9..12
                                        TK_WORD@9..12 "foo"
                                      TK_DOUBLE_QUOTES@12..13 "\""
                                  TK_COLON@13..14 ":"
                                  TWIG_EXPRESSION@14..20
                                    TWIG_LITERAL_STRING@14..20
                                      TK_WHITESPACE@14..15 " "
                                      TK_DOUBLE_QUOTES@15..16 "\""
                                      TWIG_LITERAL_STRING_INNER@16..19
                                        TK_WORD@16..19 "bar"
                                      TK_DOUBLE_QUOTES@19..20 "\""
                              TK_CLOSE_CURLY@20..21 "}"
                        TK_CLOSE_SQUARE@21..22 "]"
                    TK_WHITESPACE@22..23 " "
                    TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_name() {
        check_parse(
            "{{ my_variable }}",
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_LITERAL_NAME@2..14
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..14 "my_variable"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_token_variable_name() {
        check_parse(
            "{{ and }}",
            expect![[r#"
                ROOT@0..9
                  TWIG_VAR@0..9
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..6
                      TWIG_LITERAL_NAME@2..6
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..6 "and"
                    TK_WHITESPACE@6..7 " "
                    TK_CLOSE_CURLY_CURLY@7..9 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_get_attribute_expression() {
        check_parse(
            r#"{{ product.prices.euro }}"#,
            expect![[r#"
                ROOT@0..25
                  TWIG_VAR@0..25
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..22
                      TWIG_ACCESSOR@2..22
                        TWIG_OPERAND@2..17
                          TWIG_ACCESSOR@2..17
                            TWIG_OPERAND@2..10
                              TWIG_LITERAL_NAME@2..10
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..10 "product"
                            TK_DOT@10..11 "."
                            TWIG_OPERAND@11..17
                              TWIG_LITERAL_NAME@11..17
                                TK_WORD@11..17 "prices"
                        TK_DOT@17..18 "."
                        TWIG_OPERAND@18..22
                          TWIG_LITERAL_NAME@18..22
                            TK_WORD@18..22 "euro"
                    TK_WHITESPACE@22..23 " "
                    TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_with_filters() {
        check_parse(
            r#"{{ product.price|striptags|title }}"#,
            expect![[r#"
                ROOT@0..35
                  TWIG_VAR@0..35
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..32
                      TWIG_FILTER@2..32
                        TWIG_OPERAND@2..26
                          TWIG_FILTER@2..26
                            TWIG_OPERAND@2..16
                              TWIG_ACCESSOR@2..16
                                TWIG_OPERAND@2..10
                                  TWIG_LITERAL_NAME@2..10
                                    TK_WHITESPACE@2..3 " "
                                    TK_WORD@3..10 "product"
                                TK_DOT@10..11 "."
                                TWIG_OPERAND@11..16
                                  TWIG_LITERAL_NAME@11..16
                                    TK_WORD@11..16 "price"
                            TK_SINGLE_PIPE@16..17 "|"
                            TWIG_OPERAND@17..26
                              TWIG_LITERAL_NAME@17..26
                                TK_WORD@17..26 "striptags"
                        TK_SINGLE_PIPE@26..27 "|"
                        TWIG_OPERAND@27..32
                          TWIG_LITERAL_NAME@27..32
                            TK_WORD@27..32 "title"
                    TK_WHITESPACE@32..33 " "
                    TK_CLOSE_CURLY_CURLY@33..35 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_accessor() {
        check_parse(
            r#"{{ product.prices['eur'] }}"#,
            expect![[r#"
                ROOT@0..27
                  TWIG_VAR@0..27
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..24
                      TWIG_INDEX_LOOKUP@2..24
                        TWIG_OPERAND@2..17
                          TWIG_ACCESSOR@2..17
                            TWIG_OPERAND@2..10
                              TWIG_LITERAL_NAME@2..10
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..10 "product"
                            TK_DOT@10..11 "."
                            TWIG_OPERAND@11..17
                              TWIG_LITERAL_NAME@11..17
                                TK_WORD@11..17 "prices"
                        TK_OPEN_SQUARE@17..18 "["
                        TWIG_INDEX@18..23
                          TWIG_EXPRESSION@18..23
                            TWIG_LITERAL_STRING@18..23
                              TK_SINGLE_QUOTES@18..19 "'"
                              TWIG_LITERAL_STRING_INNER@19..22
                                TK_WORD@19..22 "eur"
                              TK_SINGLE_QUOTES@22..23 "'"
                        TK_CLOSE_SQUARE@23..24 "]"
                    TK_WHITESPACE@24..25 " "
                    TK_CLOSE_CURLY_CURLY@25..27 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_nested_array_accessor() {
        check_parse(
            r#"{{ product.prices['eur'][0] }}"#,
            expect![[r#"
                ROOT@0..30
                  TWIG_VAR@0..30
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..27
                      TWIG_INDEX_LOOKUP@2..27
                        TWIG_OPERAND@2..24
                          TWIG_INDEX_LOOKUP@2..24
                            TWIG_OPERAND@2..17
                              TWIG_ACCESSOR@2..17
                                TWIG_OPERAND@2..10
                                  TWIG_LITERAL_NAME@2..10
                                    TK_WHITESPACE@2..3 " "
                                    TK_WORD@3..10 "product"
                                TK_DOT@10..11 "."
                                TWIG_OPERAND@11..17
                                  TWIG_LITERAL_NAME@11..17
                                    TK_WORD@11..17 "prices"
                            TK_OPEN_SQUARE@17..18 "["
                            TWIG_INDEX@18..23
                              TWIG_EXPRESSION@18..23
                                TWIG_LITERAL_STRING@18..23
                                  TK_SINGLE_QUOTES@18..19 "'"
                                  TWIG_LITERAL_STRING_INNER@19..22
                                    TK_WORD@19..22 "eur"
                                  TK_SINGLE_QUOTES@22..23 "'"
                            TK_CLOSE_SQUARE@23..24 "]"
                        TK_OPEN_SQUARE@24..25 "["
                        TWIG_INDEX@25..26
                          TWIG_EXPRESSION@25..26
                            TWIG_LITERAL_NUMBER@25..26
                              TK_NUMBER@25..26 "0"
                        TK_CLOSE_SQUARE@26..27 "]"
                    TK_WHITESPACE@27..28 " "
                    TK_CLOSE_CURLY_CURLY@28..30 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_nested_array_accessor_with_dot() {
        check_parse(
            r#"{{ product.prices['eur'].0 }}"#,
            expect![[r#"
            ROOT@0..29
              TWIG_VAR@0..29
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..26
                  TWIG_INDEX_LOOKUP@2..26
                    TWIG_OPERAND@2..24
                      TWIG_INDEX_LOOKUP@2..24
                        TWIG_OPERAND@2..17
                          TWIG_ACCESSOR@2..17
                            TWIG_OPERAND@2..10
                              TWIG_LITERAL_NAME@2..10
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..10 "product"
                            TK_DOT@10..11 "."
                            TWIG_OPERAND@11..17
                              TWIG_LITERAL_NAME@11..17
                                TK_WORD@11..17 "prices"
                        TK_OPEN_SQUARE@17..18 "["
                        TWIG_INDEX@18..23
                          TWIG_EXPRESSION@18..23
                            TWIG_LITERAL_STRING@18..23
                              TK_SINGLE_QUOTES@18..19 "'"
                              TWIG_LITERAL_STRING_INNER@19..22
                                TK_WORD@19..22 "eur"
                              TK_SINGLE_QUOTES@22..23 "'"
                        TK_CLOSE_SQUARE@23..24 "]"
                    TK_DOT@24..25 "."
                    TWIG_INDEX@25..26
                      TWIG_EXPRESSION@25..26
                        TWIG_LITERAL_NUMBER@25..26
                          TK_NUMBER@25..26 "0"
                TK_WHITESPACE@26..27 " "
                TK_CLOSE_CURLY_CURLY@27..29 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_dot_accessor() {
        check_parse(
            r#"{{ product.tags.0.name }}"#,
            expect![[r#"
            ROOT@0..25
              TWIG_VAR@0..25
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..22
                  TWIG_ACCESSOR@2..22
                    TWIG_OPERAND@2..17
                      TWIG_INDEX_LOOKUP@2..17
                        TWIG_OPERAND@2..15
                          TWIG_ACCESSOR@2..15
                            TWIG_OPERAND@2..10
                              TWIG_LITERAL_NAME@2..10
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..10 "product"
                            TK_DOT@10..11 "."
                            TWIG_OPERAND@11..15
                              TWIG_LITERAL_NAME@11..15
                                TK_WORD@11..15 "tags"
                        TK_DOT@15..16 "."
                        TWIG_INDEX@16..17
                          TWIG_EXPRESSION@16..17
                            TWIG_LITERAL_NUMBER@16..17
                              TK_NUMBER@16..17 "0"
                    TK_DOT@17..18 "."
                    TWIG_OPERAND@18..22
                      TWIG_LITERAL_NAME@18..22
                        TK_WORD@18..22 "name"
                TK_WHITESPACE@22..23 " "
                TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_accessor() {
        check_parse(
            r#"{{ prices[0:10] }}"#,
            expect![[r#"
                ROOT@0..18
                  TWIG_VAR@0..18
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..15
                      TWIG_INDEX_LOOKUP@2..15
                        TWIG_OPERAND@2..9
                          TWIG_LITERAL_NAME@2..9
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..9 "prices"
                        TK_OPEN_SQUARE@9..10 "["
                        TWIG_INDEX_RANGE@10..14
                          TWIG_EXPRESSION@10..11
                            TWIG_LITERAL_NUMBER@10..11
                              TK_NUMBER@10..11 "0"
                          TK_COLON@11..12 ":"
                          TWIG_EXPRESSION@12..14
                            TWIG_LITERAL_NUMBER@12..14
                              TK_NUMBER@12..14 "10"
                        TK_CLOSE_SQUARE@14..15 "]"
                    TK_WHITESPACE@15..16 " "
                    TK_CLOSE_CURLY_CURLY@16..18 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_left_accessor() {
        check_parse(
            r#"{{ prices[10:] }}"#,
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_INDEX_LOOKUP@2..14
                        TWIG_OPERAND@2..9
                          TWIG_LITERAL_NAME@2..9
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..9 "prices"
                        TK_OPEN_SQUARE@9..10 "["
                        TWIG_INDEX_RANGE@10..13
                          TWIG_EXPRESSION@10..12
                            TWIG_LITERAL_NUMBER@10..12
                              TK_NUMBER@10..12 "10"
                          TK_COLON@12..13 ":"
                        TK_CLOSE_SQUARE@13..14 "]"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_right_accessor() {
        check_parse(
            r#"{{ prices[:10] }}"#,
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_INDEX_LOOKUP@2..14
                        TWIG_OPERAND@2..9
                          TWIG_LITERAL_NAME@2..9
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..9 "prices"
                        TK_OPEN_SQUARE@9..10 "["
                        TWIG_INDEX_RANGE@10..13
                          TK_COLON@10..11 ":"
                          TWIG_EXPRESSION@11..13
                            TWIG_LITERAL_NUMBER@11..13
                              TK_NUMBER@11..13 "10"
                        TK_CLOSE_SQUARE@13..14 "]"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_right_accessor_negative() {
        check_parse(
            r#"{{ prices[:-2] }}"#,
            expect![[r#"
            ROOT@0..17
              TWIG_VAR@0..17
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..14
                  TWIG_INDEX_LOOKUP@2..14
                    TWIG_OPERAND@2..9
                      TWIG_LITERAL_NAME@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..9 "prices"
                    TK_OPEN_SQUARE@9..10 "["
                    TWIG_INDEX_RANGE@10..13
                      TK_COLON@10..11 ":"
                      TWIG_EXPRESSION@11..13
                        TWIG_UNARY_EXPRESSION@11..13
                          TK_MINUS@11..12 "-"
                          TWIG_EXPRESSION@12..13
                            TWIG_LITERAL_NUMBER@12..13
                              TK_NUMBER@12..13 "2"
                    TK_CLOSE_SQUARE@13..14 "]"
                TK_WHITESPACE@14..15 " "
                TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_right_accessor_variable() {
        check_parse(
            r#"{{ prices[:upperLimit] }}"#,
            expect![[r#"
            ROOT@0..25
              TWIG_VAR@0..25
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..22
                  TWIG_INDEX_LOOKUP@2..22
                    TWIG_OPERAND@2..9
                      TWIG_LITERAL_NAME@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..9 "prices"
                    TK_OPEN_SQUARE@9..10 "["
                    TWIG_INDEX_RANGE@10..21
                      TK_COLON@10..11 ":"
                      TWIG_EXPRESSION@11..21
                        TWIG_LITERAL_NAME@11..21
                          TK_WORD@11..21 "upperLimit"
                    TK_CLOSE_SQUARE@21..22 "]"
                TK_WHITESPACE@22..23 " "
                TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_range_left_accessor_variable() {
        check_parse(
            r#"{{ prices[upperLimit:] }}"#,
            expect![[r#"
            ROOT@0..25
              TWIG_VAR@0..25
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..22
                  TWIG_INDEX_LOOKUP@2..22
                    TWIG_OPERAND@2..9
                      TWIG_LITERAL_NAME@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..9 "prices"
                    TK_OPEN_SQUARE@9..10 "["
                    TWIG_INDEX_RANGE@10..21
                      TWIG_EXPRESSION@10..20
                        TWIG_LITERAL_NAME@10..20
                          TK_WORD@10..20 "upperLimit"
                      TK_COLON@20..21 ":"
                    TK_CLOSE_SQUARE@21..22 "]"
                TK_WHITESPACE@22..23 " "
                TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_array_index_missing_expression() {
        check_parse(
            r#"{{ prices[] }}"#,
            expect![[r#"
            ROOT@0..14
              TWIG_VAR@0..14
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..11
                  TWIG_INDEX_LOOKUP@2..11
                    TWIG_OPERAND@2..9
                      TWIG_LITERAL_NAME@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..9 "prices"
                    TK_OPEN_SQUARE@9..10 "["
                    TWIG_INDEX@10..10
                    TK_CLOSE_SQUARE@10..11 "]"
                TK_WHITESPACE@11..12 " "
                TK_CLOSE_CURLY_CURLY@12..14 "}}"
            error at 10..11: expected twig expression but found ]"#]],
        );
    }

    #[test]
    fn parse_twig_variable_accessor_indexer_and_filter() {
        check_parse(
            r#"{{ product.prices['eur'][0]|title }}"#,
            expect![[r#"
                ROOT@0..36
                  TWIG_VAR@0..36
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..33
                      TWIG_FILTER@2..33
                        TWIG_OPERAND@2..27
                          TWIG_INDEX_LOOKUP@2..27
                            TWIG_OPERAND@2..24
                              TWIG_INDEX_LOOKUP@2..24
                                TWIG_OPERAND@2..17
                                  TWIG_ACCESSOR@2..17
                                    TWIG_OPERAND@2..10
                                      TWIG_LITERAL_NAME@2..10
                                        TK_WHITESPACE@2..3 " "
                                        TK_WORD@3..10 "product"
                                    TK_DOT@10..11 "."
                                    TWIG_OPERAND@11..17
                                      TWIG_LITERAL_NAME@11..17
                                        TK_WORD@11..17 "prices"
                                TK_OPEN_SQUARE@17..18 "["
                                TWIG_INDEX@18..23
                                  TWIG_EXPRESSION@18..23
                                    TWIG_LITERAL_STRING@18..23
                                      TK_SINGLE_QUOTES@18..19 "'"
                                      TWIG_LITERAL_STRING_INNER@19..22
                                        TK_WORD@19..22 "eur"
                                      TK_SINGLE_QUOTES@22..23 "'"
                                TK_CLOSE_SQUARE@23..24 "]"
                            TK_OPEN_SQUARE@24..25 "["
                            TWIG_INDEX@25..26
                              TWIG_EXPRESSION@25..26
                                TWIG_LITERAL_NUMBER@25..26
                                  TK_NUMBER@25..26 "0"
                            TK_CLOSE_SQUARE@26..27 "]"
                        TK_SINGLE_PIPE@27..28 "|"
                        TWIG_OPERAND@28..33
                          TWIG_LITERAL_NAME@28..33
                            TK_WORD@28..33 "title"
                    TK_WHITESPACE@33..34 " "
                    TK_CLOSE_CURLY_CURLY@34..36 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_function_accessor() {
        check_parse(
            r#"{{ product.prices('eur').gross }}"#,
            expect![[r#"
                ROOT@0..33
                  TWIG_VAR@0..33
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..30
                      TWIG_ACCESSOR@2..30
                        TWIG_OPERAND@2..24
                          TWIG_FUNCTION_CALL@2..24
                            TWIG_OPERAND@2..17
                              TWIG_ACCESSOR@2..17
                                TWIG_OPERAND@2..10
                                  TWIG_LITERAL_NAME@2..10
                                    TK_WHITESPACE@2..3 " "
                                    TK_WORD@3..10 "product"
                                TK_DOT@10..11 "."
                                TWIG_OPERAND@11..17
                                  TWIG_LITERAL_NAME@11..17
                                    TK_WORD@11..17 "prices"
                            TWIG_ARGUMENTS@17..24
                              TK_OPEN_PARENTHESIS@17..18 "("
                              TWIG_EXPRESSION@18..23
                                TWIG_LITERAL_STRING@18..23
                                  TK_SINGLE_QUOTES@18..19 "'"
                                  TWIG_LITERAL_STRING_INNER@19..22
                                    TK_WORD@19..22 "eur"
                                  TK_SINGLE_QUOTES@22..23 "'"
                              TK_CLOSE_PARENTHESIS@23..24 ")"
                        TK_DOT@24..25 "."
                        TWIG_OPERAND@25..30
                          TWIG_LITERAL_NAME@25..30
                            TK_WORD@25..30 "gross"
                    TK_WHITESPACE@30..31 " "
                    TK_CLOSE_CURLY_CURLY@31..33 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_variable_deep_function_accessor() {
        check_parse(
            r#"{{ product.prices.gross('eur').gross }}"#,
            expect![[r#"
                ROOT@0..39
                  TWIG_VAR@0..39
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..36
                      TWIG_ACCESSOR@2..36
                        TWIG_OPERAND@2..30
                          TWIG_FUNCTION_CALL@2..30
                            TWIG_OPERAND@2..23
                              TWIG_ACCESSOR@2..23
                                TWIG_OPERAND@2..17
                                  TWIG_ACCESSOR@2..17
                                    TWIG_OPERAND@2..10
                                      TWIG_LITERAL_NAME@2..10
                                        TK_WHITESPACE@2..3 " "
                                        TK_WORD@3..10 "product"
                                    TK_DOT@10..11 "."
                                    TWIG_OPERAND@11..17
                                      TWIG_LITERAL_NAME@11..17
                                        TK_WORD@11..17 "prices"
                                TK_DOT@17..18 "."
                                TWIG_OPERAND@18..23
                                  TWIG_LITERAL_NAME@18..23
                                    TK_WORD@18..23 "gross"
                            TWIG_ARGUMENTS@23..30
                              TK_OPEN_PARENTHESIS@23..24 "("
                              TWIG_EXPRESSION@24..29
                                TWIG_LITERAL_STRING@24..29
                                  TK_SINGLE_QUOTES@24..25 "'"
                                  TWIG_LITERAL_STRING_INNER@25..28
                                    TK_WORD@25..28 "eur"
                                  TK_SINGLE_QUOTES@28..29 "'"
                              TK_CLOSE_PARENTHESIS@29..30 ")"
                        TK_DOT@30..31 "."
                        TWIG_OPERAND@31..36
                          TWIG_LITERAL_NAME@31..36
                            TK_WORD@31..36 "gross"
                    TK_WHITESPACE@36..37 " "
                    TK_CLOSE_CURLY_CURLY@37..39 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_function() {
        check_parse(
            r#"{{ doIt() }}"#,
            expect![[r#"
                ROOT@0..12
                  TWIG_VAR@0..12
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..9
                      TWIG_FUNCTION_CALL@2..9
                        TWIG_OPERAND@2..7
                          TWIG_LITERAL_NAME@2..7
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..7 "doIt"
                        TWIG_ARGUMENTS@7..9
                          TK_OPEN_PARENTHESIS@7..8 "("
                          TK_CLOSE_PARENTHESIS@8..9 ")"
                    TK_WHITESPACE@9..10 " "
                    TK_CLOSE_CURLY_CURLY@10..12 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_function_arguments() {
        check_parse(
            r#"{{ sum(1, 2) }}"#,
            expect![[r#"
                ROOT@0..15
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..12
                      TWIG_FUNCTION_CALL@2..12
                        TWIG_OPERAND@2..6
                          TWIG_LITERAL_NAME@2..6
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..6 "sum"
                        TWIG_ARGUMENTS@6..12
                          TK_OPEN_PARENTHESIS@6..7 "("
                          TWIG_EXPRESSION@7..8
                            TWIG_LITERAL_NUMBER@7..8
                              TK_NUMBER@7..8 "1"
                          TK_COMMA@8..9 ","
                          TWIG_EXPRESSION@9..11
                            TWIG_LITERAL_NUMBER@9..11
                              TK_WHITESPACE@9..10 " "
                              TK_NUMBER@10..11 "2"
                          TK_CLOSE_PARENTHESIS@11..12 ")"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_function_named_arguments() {
        check_parse(
            r#"{{ sum(a=1, b=2) }}"#,
            expect![[r#"
                ROOT@0..19
                  TWIG_VAR@0..19
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..16
                      TWIG_FUNCTION_CALL@2..16
                        TWIG_OPERAND@2..6
                          TWIG_LITERAL_NAME@2..6
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..6 "sum"
                        TWIG_ARGUMENTS@6..16
                          TK_OPEN_PARENTHESIS@6..7 "("
                          TWIG_NAMED_ARGUMENT@7..10
                            TK_WORD@7..8 "a"
                            TK_EQUAL@8..9 "="
                            TWIG_EXPRESSION@9..10
                              TWIG_LITERAL_NUMBER@9..10
                                TK_NUMBER@9..10 "1"
                          TK_COMMA@10..11 ","
                          TWIG_NAMED_ARGUMENT@11..15
                            TK_WHITESPACE@11..12 " "
                            TK_WORD@12..13 "b"
                            TK_EQUAL@13..14 "="
                            TWIG_EXPRESSION@14..15
                              TWIG_LITERAL_NUMBER@14..15
                                TK_NUMBER@14..15 "2"
                          TK_CLOSE_PARENTHESIS@15..16 ")"
                    TK_WHITESPACE@16..17 " "
                    TK_CLOSE_CURLY_CURLY@17..19 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_function_mixed_named_arguments() {
        check_parse(
            r#"{{ sum(1, b=my_number) }}"#,
            expect![[r#"
                ROOT@0..25
                  TWIG_VAR@0..25
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..22
                      TWIG_FUNCTION_CALL@2..22
                        TWIG_OPERAND@2..6
                          TWIG_LITERAL_NAME@2..6
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..6 "sum"
                        TWIG_ARGUMENTS@6..22
                          TK_OPEN_PARENTHESIS@6..7 "("
                          TWIG_EXPRESSION@7..8
                            TWIG_LITERAL_NUMBER@7..8
                              TK_NUMBER@7..8 "1"
                          TK_COMMA@8..9 ","
                          TWIG_NAMED_ARGUMENT@9..21
                            TK_WHITESPACE@9..10 " "
                            TK_WORD@10..11 "b"
                            TK_EQUAL@11..12 "="
                            TWIG_EXPRESSION@12..21
                              TWIG_LITERAL_NAME@12..21
                                TK_WORD@12..21 "my_number"
                          TK_CLOSE_PARENTHESIS@21..22 ")"
                    TK_WHITESPACE@22..23 " "
                    TK_CLOSE_CURLY_CURLY@23..25 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_function_nested_call() {
        check_parse(
            r#"{{ sum(1, sin(1)) }}"#,
            expect![[r#"
                ROOT@0..20
                  TWIG_VAR@0..20
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..17
                      TWIG_FUNCTION_CALL@2..17
                        TWIG_OPERAND@2..6
                          TWIG_LITERAL_NAME@2..6
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..6 "sum"
                        TWIG_ARGUMENTS@6..17
                          TK_OPEN_PARENTHESIS@6..7 "("
                          TWIG_EXPRESSION@7..8
                            TWIG_LITERAL_NUMBER@7..8
                              TK_NUMBER@7..8 "1"
                          TK_COMMA@8..9 ","
                          TWIG_EXPRESSION@9..16
                            TWIG_FUNCTION_CALL@9..16
                              TWIG_OPERAND@9..13
                                TWIG_LITERAL_NAME@9..13
                                  TK_WHITESPACE@9..10 " "
                                  TK_WORD@10..13 "sin"
                              TWIG_ARGUMENTS@13..16
                                TK_OPEN_PARENTHESIS@13..14 "("
                                TWIG_EXPRESSION@14..15
                                  TWIG_LITERAL_NUMBER@14..15
                                    TK_NUMBER@14..15 "1"
                                TK_CLOSE_PARENTHESIS@15..16 ")"
                          TK_CLOSE_PARENTHESIS@16..17 ")"
                    TK_WHITESPACE@17..18 " "
                    TK_CLOSE_CURLY_CURLY@18..20 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_arrow_function_simple() {
        check_parse(
            r#"{% set my_arrow_function = i => i % 2 %}"#,
            expect![[r#"
                ROOT@0..40
                  TWIG_SET@0..40
                    TWIG_SET_BLOCK@0..40
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..37
                        TWIG_LITERAL_NAME@6..24
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..24 "my_arrow_function"
                        TK_WHITESPACE@24..25 " "
                        TK_EQUAL@25..26 "="
                        TWIG_EXPRESSION@26..37
                          TWIG_ARROW_FUNCTION@26..37
                            TWIG_ARGUMENTS@26..28
                              TWIG_LITERAL_NAME@26..28
                                TK_WHITESPACE@26..27 " "
                                TK_WORD@27..28 "i"
                            TK_WHITESPACE@28..29 " "
                            TK_EQUAL_GREATER_THAN@29..31 "=>"
                            TWIG_EXPRESSION@31..37
                              TWIG_BINARY_EXPRESSION@31..37
                                TWIG_EXPRESSION@31..33
                                  TWIG_LITERAL_NAME@31..33
                                    TK_WHITESPACE@31..32 " "
                                    TK_WORD@32..33 "i"
                                TK_WHITESPACE@33..34 " "
                                TK_PERCENT@34..35 "%"
                                TWIG_EXPRESSION@35..37
                                  TWIG_LITERAL_NUMBER@35..37
                                    TK_WHITESPACE@35..36 " "
                                    TK_NUMBER@36..37 "2"
                      TK_WHITESPACE@37..38 " "
                      TK_PERCENT_CURLY@38..40 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_arrow_function_simple_brackets() {
        check_parse(
            r#"{% set my_arrow_function = (i) => i % 2 %}"#,
            expect![[r#"
                ROOT@0..42
                  TWIG_SET@0..42
                    TWIG_SET_BLOCK@0..42
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..39
                        TWIG_LITERAL_NAME@6..24
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..24 "my_arrow_function"
                        TK_WHITESPACE@24..25 " "
                        TK_EQUAL@25..26 "="
                        TWIG_EXPRESSION@26..39
                          TWIG_ARROW_FUNCTION@26..39
                            TWIG_ARGUMENTS@26..30
                              TK_WHITESPACE@26..27 " "
                              TK_OPEN_PARENTHESIS@27..28 "("
                              TWIG_LITERAL_NAME@28..29
                                TK_WORD@28..29 "i"
                              TK_CLOSE_PARENTHESIS@29..30 ")"
                            TK_WHITESPACE@30..31 " "
                            TK_EQUAL_GREATER_THAN@31..33 "=>"
                            TWIG_EXPRESSION@33..39
                              TWIG_BINARY_EXPRESSION@33..39
                                TWIG_EXPRESSION@33..35
                                  TWIG_LITERAL_NAME@33..35
                                    TK_WHITESPACE@33..34 " "
                                    TK_WORD@34..35 "i"
                                TK_WHITESPACE@35..36 " "
                                TK_PERCENT@36..37 "%"
                                TWIG_EXPRESSION@37..39
                                  TWIG_LITERAL_NUMBER@37..39
                                    TK_WHITESPACE@37..38 " "
                                    TK_NUMBER@38..39 "2"
                      TK_WHITESPACE@39..40 " "
                      TK_PERCENT_CURLY@40..42 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_arrow_function_advanced() {
        check_parse(
            r#"{% set my_arrow_function = (a, b) => a >= b %}"#,
            expect![[r#"
                ROOT@0..46
                  TWIG_SET@0..46
                    TWIG_SET_BLOCK@0..46
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..43
                        TWIG_LITERAL_NAME@6..24
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..24 "my_arrow_function"
                        TK_WHITESPACE@24..25 " "
                        TK_EQUAL@25..26 "="
                        TWIG_EXPRESSION@26..43
                          TWIG_ARROW_FUNCTION@26..43
                            TWIG_ARGUMENTS@26..33
                              TK_WHITESPACE@26..27 " "
                              TK_OPEN_PARENTHESIS@27..28 "("
                              TWIG_LITERAL_NAME@28..29
                                TK_WORD@28..29 "a"
                              TK_COMMA@29..30 ","
                              TWIG_LITERAL_NAME@30..32
                                TK_WHITESPACE@30..31 " "
                                TK_WORD@31..32 "b"
                              TK_CLOSE_PARENTHESIS@32..33 ")"
                            TK_WHITESPACE@33..34 " "
                            TK_EQUAL_GREATER_THAN@34..36 "=>"
                            TWIG_EXPRESSION@36..43
                              TWIG_BINARY_EXPRESSION@36..43
                                TWIG_EXPRESSION@36..38
                                  TWIG_LITERAL_NAME@36..38
                                    TK_WHITESPACE@36..37 " "
                                    TK_WORD@37..38 "a"
                                TK_WHITESPACE@38..39 " "
                                TK_GREATER_THAN_EQUAL@39..41 ">="
                                TWIG_EXPRESSION@41..43
                                  TWIG_LITERAL_NAME@41..43
                                    TK_WHITESPACE@41..42 " "
                                    TK_WORD@42..43 "b"
                      TK_WHITESPACE@43..44 " "
                      TK_PERCENT_CURLY@44..46 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_arrow_function_as_filer_argument() {
        check_parse(
            r#"{% for item in crossSellings|filter(item => item.total > 0 and item.crossSelling.active == true) %} {{ item }} {% endfor %}"#,
            expect![[r#"
                ROOT@0..123
                  TWIG_FOR@0..123
                    TWIG_FOR_BLOCK@0..99
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "item"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TWIG_EXPRESSION@14..96
                        TWIG_FILTER@14..96
                          TWIG_OPERAND@14..28
                            TWIG_LITERAL_NAME@14..28
                              TK_WHITESPACE@14..15 " "
                              TK_WORD@15..28 "crossSellings"
                          TK_SINGLE_PIPE@28..29 "|"
                          TWIG_OPERAND@29..96
                            TWIG_LITERAL_NAME@29..35
                              TK_WORD@29..35 "filter"
                            TWIG_ARGUMENTS@35..96
                              TK_OPEN_PARENTHESIS@35..36 "("
                              TWIG_EXPRESSION@36..95
                                TWIG_ARROW_FUNCTION@36..95
                                  TWIG_ARGUMENTS@36..40
                                    TWIG_LITERAL_NAME@36..40
                                      TK_WORD@36..40 "item"
                                  TK_WHITESPACE@40..41 " "
                                  TK_EQUAL_GREATER_THAN@41..43 "=>"
                                  TWIG_EXPRESSION@43..95
                                    TWIG_BINARY_EXPRESSION@43..95
                                      TWIG_BINARY_EXPRESSION@43..58
                                        TWIG_EXPRESSION@43..54
                                          TWIG_ACCESSOR@43..54
                                            TWIG_OPERAND@43..48
                                              TWIG_LITERAL_NAME@43..48
                                                TK_WHITESPACE@43..44 " "
                                                TK_WORD@44..48 "item"
                                            TK_DOT@48..49 "."
                                            TWIG_OPERAND@49..54
                                              TWIG_LITERAL_NAME@49..54
                                                TK_WORD@49..54 "total"
                                        TK_WHITESPACE@54..55 " "
                                        TK_GREATER_THAN@55..56 ">"
                                        TWIG_EXPRESSION@56..58
                                          TWIG_LITERAL_NUMBER@56..58
                                            TK_WHITESPACE@56..57 " "
                                            TK_NUMBER@57..58 "0"
                                      TK_WHITESPACE@58..59 " "
                                      TK_AND@59..62 "and"
                                      TWIG_EXPRESSION@62..95
                                        TWIG_BINARY_EXPRESSION@62..95
                                          TWIG_EXPRESSION@62..87
                                            TWIG_ACCESSOR@62..87
                                              TWIG_OPERAND@62..80
                                                TWIG_ACCESSOR@62..80
                                                  TWIG_OPERAND@62..67
                                                    TWIG_LITERAL_NAME@62..67
                                                      TK_WHITESPACE@62..63 " "
                                                      TK_WORD@63..67 "item"
                                                  TK_DOT@67..68 "."
                                                  TWIG_OPERAND@68..80
                                                    TWIG_LITERAL_NAME@68..80
                                                      TK_WORD@68..80 "crossSelling"
                                              TK_DOT@80..81 "."
                                              TWIG_OPERAND@81..87
                                                TWIG_LITERAL_NAME@81..87
                                                  TK_WORD@81..87 "active"
                                          TK_WHITESPACE@87..88 " "
                                          TK_DOUBLE_EQUAL@88..90 "=="
                                          TWIG_EXPRESSION@90..95
                                            TWIG_LITERAL_BOOLEAN@90..95
                                              TK_WHITESPACE@90..91 " "
                                              TK_TRUE@91..95 "true"
                              TK_CLOSE_PARENTHESIS@95..96 ")"
                      TK_WHITESPACE@96..97 " "
                      TK_PERCENT_CURLY@97..99 "%}"
                    BODY@99..110
                      TWIG_VAR@99..110
                        TK_WHITESPACE@99..100 " "
                        TK_OPEN_CURLY_CURLY@100..102 "{{"
                        TWIG_EXPRESSION@102..107
                          TWIG_LITERAL_NAME@102..107
                            TK_WHITESPACE@102..103 " "
                            TK_WORD@103..107 "item"
                        TK_WHITESPACE@107..108 " "
                        TK_CLOSE_CURLY_CURLY@108..110 "}}"
                    TWIG_ENDFOR_BLOCK@110..123
                      TK_WHITESPACE@110..111 " "
                      TK_CURLY_PERCENT@111..113 "{%"
                      TK_WHITESPACE@113..114 " "
                      TK_ENDFOR@114..120 "endfor"
                      TK_WHITESPACE@120..121 " "
                      TK_PERCENT_CURLY@121..123 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_filter_arguments() {
        check_parse(
            r#"{{ list|join(', ') }}"#,
            expect![[r#"
                ROOT@0..21
                  TWIG_VAR@0..21
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..18
                      TWIG_FILTER@2..18
                        TWIG_OPERAND@2..7
                          TWIG_LITERAL_NAME@2..7
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..7 "list"
                        TK_SINGLE_PIPE@7..8 "|"
                        TWIG_OPERAND@8..18
                          TWIG_LITERAL_NAME@8..12
                            TK_WORD@8..12 "join"
                          TWIG_ARGUMENTS@12..18
                            TK_OPEN_PARENTHESIS@12..13 "("
                            TWIG_EXPRESSION@13..17
                              TWIG_LITERAL_STRING@13..17
                                TK_SINGLE_QUOTES@13..14 "'"
                                TWIG_LITERAL_STRING_INNER@14..16
                                  TK_COMMA@14..15 ","
                                  TK_WHITESPACE@15..16 " "
                                TK_SINGLE_QUOTES@16..17 "'"
                            TK_CLOSE_PARENTHESIS@17..18 ")"
                    TK_WHITESPACE@18..19 " "
                    TK_CLOSE_CURLY_CURLY@19..21 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_double_filter_arguments() {
        check_parse(
            r#"{{ list|join(', ')|trim }}"#,
            expect![[r#"
                ROOT@0..26
                  TWIG_VAR@0..26
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..23
                      TWIG_FILTER@2..23
                        TWIG_OPERAND@2..18
                          TWIG_FILTER@2..18
                            TWIG_OPERAND@2..7
                              TWIG_LITERAL_NAME@2..7
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..7 "list"
                            TK_SINGLE_PIPE@7..8 "|"
                            TWIG_OPERAND@8..18
                              TWIG_LITERAL_NAME@8..12
                                TK_WORD@8..12 "join"
                              TWIG_ARGUMENTS@12..18
                                TK_OPEN_PARENTHESIS@12..13 "("
                                TWIG_EXPRESSION@13..17
                                  TWIG_LITERAL_STRING@13..17
                                    TK_SINGLE_QUOTES@13..14 "'"
                                    TWIG_LITERAL_STRING_INNER@14..16
                                      TK_COMMA@14..15 ","
                                      TK_WHITESPACE@15..16 " "
                                    TK_SINGLE_QUOTES@16..17 "'"
                                TK_CLOSE_PARENTHESIS@17..18 ")"
                        TK_SINGLE_PIPE@18..19 "|"
                        TWIG_OPERAND@19..23
                          TWIG_LITERAL_NAME@19..23
                            TK_WORD@19..23 "trim"
                    TK_WHITESPACE@23..24 " "
                    TK_CLOSE_CURLY_CURLY@24..26 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_filter_after_string_with_named_argument() {
        check_parse(
            r#"{{ "now"|date('d/m/Y H:i', timezone="Europe/Paris") }}"#,
            expect![[r#"
                ROOT@0..54
                  TWIG_VAR@0..54
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..51
                      TWIG_FILTER@2..51
                        TWIG_OPERAND@2..8
                          TWIG_LITERAL_STRING@2..8
                            TK_WHITESPACE@2..3 " "
                            TK_DOUBLE_QUOTES@3..4 "\""
                            TWIG_LITERAL_STRING_INNER@4..7
                              TK_WORD@4..7 "now"
                            TK_DOUBLE_QUOTES@7..8 "\""
                        TK_SINGLE_PIPE@8..9 "|"
                        TWIG_OPERAND@9..51
                          TWIG_LITERAL_NAME@9..13
                            TK_WORD@9..13 "date"
                          TWIG_ARGUMENTS@13..51
                            TK_OPEN_PARENTHESIS@13..14 "("
                            TWIG_EXPRESSION@14..25
                              TWIG_LITERAL_STRING@14..25
                                TK_SINGLE_QUOTES@14..15 "'"
                                TWIG_LITERAL_STRING_INNER@15..24
                                  TK_WORD@15..16 "d"
                                  TK_FORWARD_SLASH@16..17 "/"
                                  TK_WORD@17..18 "m"
                                  TK_FORWARD_SLASH@18..19 "/"
                                  TK_WORD@19..20 "Y"
                                  TK_WHITESPACE@20..21 " "
                                  TK_WORD@21..22 "H"
                                  TK_COLON@22..23 ":"
                                  TK_WORD@23..24 "i"
                                TK_SINGLE_QUOTES@24..25 "'"
                            TK_COMMA@25..26 ","
                            TWIG_NAMED_ARGUMENT@26..50
                              TK_WHITESPACE@26..27 " "
                              TK_WORD@27..35 "timezone"
                              TK_EQUAL@35..36 "="
                              TWIG_EXPRESSION@36..50
                                TWIG_LITERAL_STRING@36..50
                                  TK_DOUBLE_QUOTES@36..37 "\""
                                  TWIG_LITERAL_STRING_INNER@37..49
                                    TK_WORD@37..43 "Europe"
                                    TK_FORWARD_SLASH@43..44 "/"
                                    TK_WORD@44..49 "Paris"
                                  TK_DOUBLE_QUOTES@49..50 "\""
                            TK_CLOSE_PARENTHESIS@50..51 ")"
                    TK_WHITESPACE@51..52 " "
                    TK_CLOSE_CURLY_CURLY@52..54 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_filter_within_binary_comparison() {
        check_parse(
            r#"{{ users|length > 0 }}"#,
            expect![[r#"
                ROOT@0..22
                  TWIG_VAR@0..22
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..19
                      TWIG_BINARY_EXPRESSION@2..19
                        TWIG_EXPRESSION@2..15
                          TWIG_FILTER@2..15
                            TWIG_OPERAND@2..8
                              TWIG_LITERAL_NAME@2..8
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..8 "users"
                            TK_SINGLE_PIPE@8..9 "|"
                            TWIG_OPERAND@9..15
                              TWIG_LITERAL_NAME@9..15
                                TK_WORD@9..15 "length"
                        TK_WHITESPACE@15..16 " "
                        TK_GREATER_THAN@16..17 ">"
                        TWIG_EXPRESSION@17..19
                          TWIG_LITERAL_NUMBER@17..19
                            TK_WHITESPACE@17..18 " "
                            TK_NUMBER@18..19 "0"
                    TK_WHITESPACE@19..20 " "
                    TK_CLOSE_CURLY_CURLY@20..22 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_include_function_call() {
        check_parse(
            r#"{{ include('sections/articles/sidebar.html') }}"#,
            expect![[r#"
                ROOT@0..47
                  TWIG_VAR@0..47
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..44
                      TWIG_FUNCTION_CALL@2..44
                        TWIG_OPERAND@2..10
                          TWIG_LITERAL_NAME@2..10
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..10 "include"
                        TWIG_ARGUMENTS@10..44
                          TK_OPEN_PARENTHESIS@10..11 "("
                          TWIG_EXPRESSION@11..43
                            TWIG_LITERAL_STRING@11..43
                              TK_SINGLE_QUOTES@11..12 "'"
                              TWIG_LITERAL_STRING_INNER@12..42
                                TK_WORD@12..20 "sections"
                                TK_FORWARD_SLASH@20..21 "/"
                                TK_WORD@21..29 "articles"
                                TK_FORWARD_SLASH@29..30 "/"
                                TK_WORD@30..37 "sidebar"
                                TK_DOT@37..38 "."
                                TK_WORD@38..42 "html"
                              TK_SINGLE_QUOTES@42..43 "'"
                          TK_CLOSE_PARENTHESIS@43..44 ")"
                    TK_WHITESPACE@44..45 " "
                    TK_CLOSE_CURLY_CURLY@45..47 "}}""#]],
        );
    }
}

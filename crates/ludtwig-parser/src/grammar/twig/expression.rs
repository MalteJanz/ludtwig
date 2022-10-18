use crate::grammar::twig::literal::parse_twig_literal;
use crate::parser::event::CompletedMarker;
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_twig_expression(parser: &mut Parser) -> Option<CompletedMarker> {
    parse_twig_expression_binding_power(parser, 0)
}

trait Operator {
    /// left and right binding power used for the Pratt parsing algorithm
    /// None means it is not a suitable binary operator (has two operands)
    /// if the left number is lower => left-associative
    /// if the right number is lower => right-associative
    fn binary_binding_power(&self) -> Option<(u8, u8)>;

    /// Used for the Pratt parsing algorithm
    /// None means it is not a suitable prefix unary operator (has one operand on the right side)
    fn unary_binding_power(&self) -> Option<((), u8)>;
}

impl Operator for SyntaxKind {
    fn binary_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            // left associative
            T!["or"]
            | T!["||"]=> Some((5, 6)), // '||' is not official twig but still parse it
            T!["and"]
            | T!["&&"] => Some((10, 11)), // '&&' is not official twig but still parse it
            T!["b-or"] => Some((14, 15)),
            T!["b-xor"] => Some((16, 17)),
            T!["b-and"] => Some((18, 19)),
            T!["=="]
            | T!["!="]
            | T!["<=>"]
            | T!["<"]
            | T![">"]
            | T![">="]
            | T!["<="]
            | T!["not"] // hack for 'not in' operator 'not' alone is not a binary operator!
            | T!["in"]
            | T!["matches"]
            | T!["starts with"]
            | T!["ends with"]
            | T!["==="] // not official twig but still parse `===` and `!==` to later notify the user by rule
            | T!["!=="] => Some((20, 21)),
            T![".."] => Some((25, 26)),
            T!["+"] | T!["-"] => Some((30, 31)),
            T!["~"] => Some((40, 41)),
            T!["*"] | T!["/"] | T!["//"] | T!["%"] => Some((60, 61)),
            T!["is"] => Some((100, 101)),
            // right associative
            T!["**"] => Some((121, 120)),
            T!["??"] => Some((151, 150)),

            _ => None,
        }
    }

    fn unary_binding_power(&self) -> Option<((), u8)> {
        match self {
            T!["not"] => Some(((), 51)),
            T!["+"] | T!["-"] => Some(((), 201)),
            _ => None,
        }
    }
}

fn parse_twig_expression_binding_power(
    parser: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    let mut lhs = parse_twig_expression_lhs(parser)?;

    // wrap lhs in expression
    let m = parser.precede(lhs);
    lhs = parser.complete(m, SyntaxKind::TWIG_EXPRESSION);

    let mut is_binary = false;
    while let Some((left_binding_power, right_binding_power)) = parser
        .peek_token()
        .and_then(|t| t.kind.binary_binding_power())
    {
        if left_binding_power < minimum_binding_power {
            break;
        }

        // 'not' alone is not a binary expression!
        if parser.at(T!["not"]) && !parser.at_following(&[T!["not"], T!["in"]]) {
            break;
        }

        // Eat the operator’s token.
        let eaten_kind = parser.bump().kind;
        if (eaten_kind == T!["not"] && parser.at(T!["in"]))
            || (eaten_kind == T!["is"] && parser.at(T!["not"]))
        {
            parser.bump(); // eat 'in' / 'not' too
        }
        is_binary = true;

        // recurse
        let m = parser.precede(lhs);
        let parsed_rhs = parse_twig_expression_binding_power(parser, right_binding_power).is_some();
        lhs = parser.complete(m, SyntaxKind::TWIG_BINARY_EXPRESSION);

        if !parsed_rhs {
            break;
        }
    }

    // wrap hole binary expression inside an expression
    if is_binary {
        let m = parser.precede(lhs);
        lhs = parser.complete(m, SyntaxKind::TWIG_EXPRESSION);
    }

    // check for ternary operator (conditional expression) on top level
    if minimum_binding_power == 0 {
        if let Some(m) = parse_conditional_expression(parser, lhs.clone()) {
            lhs = m;
        }
    }

    Some(lhs)
}

fn parse_conditional_expression(
    parser: &mut Parser,
    lhs: CompletedMarker,
) -> Option<CompletedMarker> {
    if !parser.at(T!["?"]) {
        return None;
    }
    let m = parser.precede(lhs);
    parser.bump();

    // truthy expression
    if parse_twig_expression_binding_power(parser, 0).is_none() && !parser.at(T![":"]) {
        parser.add_error(ParseErrorBuilder::new("twig expression or ':'"));
    }

    if parser.at(T![":"]) {
        parser.bump();

        // falsy expression
        if parse_twig_expression_binding_power(parser, 0).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression"));
        }
    }

    let conditional_m = parser.complete(m, SyntaxKind::TWIG_CONDITIONAL_EXPRESSION);
    let outer = parser.precede(conditional_m);
    Some(parser.complete(outer, SyntaxKind::TWIG_EXPRESSION))
}

fn parse_twig_expression_lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["("]) {
        Some(parse_paren_expression(parser))
    } else if parser.at_set(&[T!["-"], T!["+"], T!["not"]]) {
        Some(parse_unary_expression(parser))
    } else {
        parse_twig_literal(parser)
    }
}

fn parse_paren_expression(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["("]));

    let m = parser.start();
    parser.bump();
    parse_twig_expression_binding_power(parser, 0);
    parser.expect(T![")"]);

    parser.complete(m, SyntaxKind::TWIG_PARENTHESES_EXPRESSION)
}

fn parse_unary_expression(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at_set(&[T!["-"], T!["+"], T!["not"]]));

    let m = parser.start();
    // Eat the operator’s token.
    let op_token = parser.bump();
    let ((), right_binding_power) = op_token
        .kind
        .unary_binding_power()
        .expect("'-', '+' and 'not' should have a binding power");

    parse_twig_expression_binding_power(parser, right_binding_power);

    parser.complete(m, SyntaxKind::TWIG_UNARY_EXPRESSION)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;

    #[test]
    fn parse_twig_empty_expression() {
        check_parse(
            "{{ }}",
            expect![[r#"
                ROOT@0..5
                  TWIG_VAR@0..5
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TK_WHITESPACE@2..3 " "
                    TK_CLOSE_CURLY_CURLY@3..5 "}}"
                error at 3..5: expected twig expression but found }}"#]],
        )
    }

    #[test]
    fn parse_twig_simple_number_expression() {
        check_parse(
            "{{ 1 }}",
            expect![[r#"
            ROOT@0..7
              TWIG_VAR@0..7
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..4
                  TWIG_LITERAL_NUMBER@2..4
                    TK_WHITESPACE@2..3 " "
                    TK_NUMBER@3..4 "1"
                TK_WHITESPACE@4..5 " "
                TK_CLOSE_CURLY_CURLY@5..7 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_addition_expression() {
        check_parse(
            "{{ 1 + 2 }}",
            expect![[r#"
            ROOT@0..11
              TWIG_VAR@0..11
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..8
                  TWIG_BINARY_EXPRESSION@2..8
                    TWIG_EXPRESSION@2..4
                      TWIG_LITERAL_NUMBER@2..4
                        TK_WHITESPACE@2..3 " "
                        TK_NUMBER@3..4 "1"
                    TK_WHITESPACE@4..5 " "
                    TK_PLUS@5..6 "+"
                    TWIG_EXPRESSION@6..8
                      TWIG_LITERAL_NUMBER@6..8
                        TK_WHITESPACE@6..7 " "
                        TK_NUMBER@7..8 "2"
                TK_WHITESPACE@8..9 " "
                TK_CLOSE_CURLY_CURLY@9..11 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_math_expression() {
        check_parse(
            "{{ 1 + 2 * 3 }}",
            expect![[r#"
                ROOT@0..15
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..12
                      TWIG_BINARY_EXPRESSION@2..12
                        TWIG_EXPRESSION@2..4
                          TWIG_LITERAL_NUMBER@2..4
                            TK_WHITESPACE@2..3 " "
                            TK_NUMBER@3..4 "1"
                        TK_WHITESPACE@4..5 " "
                        TK_PLUS@5..6 "+"
                        TWIG_EXPRESSION@6..12
                          TWIG_BINARY_EXPRESSION@6..12
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NUMBER@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_NUMBER@7..8 "2"
                            TK_WHITESPACE@8..9 " "
                            TK_STAR@9..10 "*"
                            TWIG_EXPRESSION@10..12
                              TWIG_LITERAL_NUMBER@10..12
                                TK_WHITESPACE@10..11 " "
                                TK_NUMBER@11..12 "3"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_math_paren_expression() {
        check_parse(
            "{{ (1 + 2) * 3 }}",
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_BINARY_EXPRESSION@2..14
                        TWIG_EXPRESSION@2..10
                          TWIG_PARENTHESES_EXPRESSION@2..10
                            TK_WHITESPACE@2..3 " "
                            TK_OPEN_PARENTHESIS@3..4 "("
                            TWIG_EXPRESSION@4..9
                              TWIG_BINARY_EXPRESSION@4..9
                                TWIG_EXPRESSION@4..5
                                  TWIG_LITERAL_NUMBER@4..5
                                    TK_NUMBER@4..5 "1"
                                TK_WHITESPACE@5..6 " "
                                TK_PLUS@6..7 "+"
                                TWIG_EXPRESSION@7..9
                                  TWIG_LITERAL_NUMBER@7..9
                                    TK_WHITESPACE@7..8 " "
                                    TK_NUMBER@8..9 "2"
                            TK_CLOSE_PARENTHESIS@9..10 ")"
                        TK_WHITESPACE@10..11 " "
                        TK_STAR@11..12 "*"
                        TWIG_EXPRESSION@12..14
                          TWIG_LITERAL_NUMBER@12..14
                            TK_WHITESPACE@12..13 " "
                            TK_NUMBER@13..14 "3"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_comparison_expression() {
        check_parse(
            "{{ a >= b + 1 }}",
            expect![[r#"
                ROOT@0..16
                  TWIG_VAR@0..16
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..13
                      TWIG_BINARY_EXPRESSION@2..13
                        TWIG_EXPRESSION@2..4
                          TWIG_LITERAL_NAME@2..4
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..4 "a"
                        TK_WHITESPACE@4..5 " "
                        TK_GREATER_THAN_EQUAL@5..7 ">="
                        TWIG_EXPRESSION@7..13
                          TWIG_BINARY_EXPRESSION@7..13
                            TWIG_EXPRESSION@7..9
                              TWIG_LITERAL_NAME@7..9
                                TK_WHITESPACE@7..8 " "
                                TK_WORD@8..9 "b"
                            TK_WHITESPACE@9..10 " "
                            TK_PLUS@10..11 "+"
                            TWIG_EXPRESSION@11..13
                              TWIG_LITERAL_NUMBER@11..13
                                TK_WHITESPACE@11..12 " "
                                TK_NUMBER@12..13 "1"
                    TK_WHITESPACE@13..14 " "
                    TK_CLOSE_CURLY_CURLY@14..16 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_unary_expression() {
        check_parse(
            "{{ not a }}",
            expect![[r#"
                ROOT@0..11
                  TWIG_VAR@0..11
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..8
                      TWIG_UNARY_EXPRESSION@2..8
                        TK_WHITESPACE@2..3 " "
                        TK_NOT@3..6 "not"
                        TWIG_EXPRESSION@6..8
                          TWIG_LITERAL_NAME@6..8
                            TK_WHITESPACE@6..7 " "
                            TK_WORD@7..8 "a"
                    TK_WHITESPACE@8..9 " "
                    TK_CLOSE_CURLY_CURLY@9..11 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_binary_expression() {
        check_parse(
            "{{ a is odd }}",
            expect![[r#"
                ROOT@0..14
                  TWIG_VAR@0..14
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..11
                      TWIG_BINARY_EXPRESSION@2..11
                        TWIG_EXPRESSION@2..4
                          TWIG_LITERAL_NAME@2..4
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..4 "a"
                        TK_WHITESPACE@4..5 " "
                        TK_IS@5..7 "is"
                        TWIG_EXPRESSION@7..11
                          TWIG_LITERAL_NAME@7..11
                            TK_WHITESPACE@7..8 " "
                            TK_WORD@8..11 "odd"
                    TK_WHITESPACE@11..12 " "
                    TK_CLOSE_CURLY_CURLY@12..14 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_string_concatenation() {
        check_parse(
            "{{ 'hello ' ~ name ~ ' to the world of ' ~ world }}",
            expect![[r#"
                ROOT@0..51
                  TWIG_VAR@0..51
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..48
                      TWIG_BINARY_EXPRESSION@2..48
                        TWIG_BINARY_EXPRESSION@2..40
                          TWIG_BINARY_EXPRESSION@2..18
                            TWIG_EXPRESSION@2..11
                              TWIG_LITERAL_STRING@2..11
                                TK_WHITESPACE@2..3 " "
                                TK_SINGLE_QUOTES@3..4 "'"
                                TWIG_LITERAL_STRING_INNER@4..10
                                  TK_WORD@4..9 "hello"
                                  TK_WHITESPACE@9..10 " "
                                TK_SINGLE_QUOTES@10..11 "'"
                            TK_WHITESPACE@11..12 " "
                            TK_TILDE@12..13 "~"
                            TWIG_EXPRESSION@13..18
                              TWIG_LITERAL_NAME@13..18
                                TK_WHITESPACE@13..14 " "
                                TK_WORD@14..18 "name"
                          TK_WHITESPACE@18..19 " "
                          TK_TILDE@19..20 "~"
                          TWIG_EXPRESSION@20..40
                            TWIG_LITERAL_STRING@20..40
                              TK_WHITESPACE@20..21 " "
                              TK_SINGLE_QUOTES@21..22 "'"
                              TWIG_LITERAL_STRING_INNER@22..39
                                TK_WHITESPACE@22..23 " "
                                TK_WORD@23..25 "to"
                                TK_WHITESPACE@25..26 " "
                                TK_WORD@26..29 "the"
                                TK_WHITESPACE@29..30 " "
                                TK_WORD@30..35 "world"
                                TK_WHITESPACE@35..36 " "
                                TK_WORD@36..38 "of"
                                TK_WHITESPACE@38..39 " "
                              TK_SINGLE_QUOTES@39..40 "'"
                        TK_WHITESPACE@40..41 " "
                        TK_TILDE@41..42 "~"
                        TWIG_EXPRESSION@42..48
                          TWIG_LITERAL_NAME@42..48
                            TK_WHITESPACE@42..43 " "
                            TK_WORD@43..48 "world"
                    TK_WHITESPACE@48..49 " "
                    TK_CLOSE_CURLY_CURLY@49..51 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_binary_and_unary_combined_expression() {
        check_parse(
            "{{ not a is not even }}",
            expect![[r#"
                ROOT@0..23
                  TWIG_VAR@0..23
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..20
                      TWIG_UNARY_EXPRESSION@2..20
                        TK_WHITESPACE@2..3 " "
                        TK_NOT@3..6 "not"
                        TWIG_EXPRESSION@6..20
                          TWIG_BINARY_EXPRESSION@6..20
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "a"
                            TK_WHITESPACE@8..9 " "
                            TK_IS@9..11 "is"
                            TK_WHITESPACE@11..12 " "
                            TK_NOT@12..15 "not"
                            TWIG_EXPRESSION@15..20
                              TWIG_LITERAL_NAME@15..20
                                TK_WHITESPACE@15..16 " "
                                TK_WORD@16..20 "even"
                    TK_WHITESPACE@20..21 " "
                    TK_CLOSE_CURLY_CURLY@21..23 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_another_binary_and_unary_combined_expression() {
        check_parse(
            "{{ not a not in [false] }}",
            expect![[r#"
                ROOT@0..26
                  TWIG_VAR@0..26
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..23
                      TWIG_BINARY_EXPRESSION@2..23
                        TWIG_EXPRESSION@2..8
                          TWIG_UNARY_EXPRESSION@2..8
                            TK_WHITESPACE@2..3 " "
                            TK_NOT@3..6 "not"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "a"
                        TK_WHITESPACE@8..9 " "
                        TK_NOT@9..12 "not"
                        TK_WHITESPACE@12..13 " "
                        TK_IN@13..15 "in"
                        TWIG_EXPRESSION@15..23
                          TWIG_LITERAL_ARRAY@15..23
                            TK_WHITESPACE@15..16 " "
                            TK_OPEN_SQUARE@16..17 "["
                            TWIG_EXPRESSION@17..22
                              TWIG_LITERAL_BOOLEAN@17..22
                                TK_FALSE@17..22 "false"
                            TK_CLOSE_SQUARE@22..23 "]"
                    TK_WHITESPACE@23..24 " "
                    TK_CLOSE_CURLY_CURLY@24..26 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_expression_not_in() {
        check_parse(
            "{{ a not in [1] }}",
            expect![[r#"
                ROOT@0..18
                  TWIG_VAR@0..18
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..15
                      TWIG_BINARY_EXPRESSION@2..15
                        TWIG_EXPRESSION@2..4
                          TWIG_LITERAL_NAME@2..4
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..4 "a"
                        TK_WHITESPACE@4..5 " "
                        TK_NOT@5..8 "not"
                        TK_WHITESPACE@8..9 " "
                        TK_IN@9..11 "in"
                        TWIG_EXPRESSION@11..15
                          TWIG_LITERAL_ARRAY@11..15
                            TK_WHITESPACE@11..12 " "
                            TK_OPEN_SQUARE@12..13 "["
                            TWIG_EXPRESSION@13..14
                              TWIG_LITERAL_NUMBER@13..14
                                TK_NUMBER@13..14 "1"
                            TK_CLOSE_SQUARE@14..15 "]"
                    TK_WHITESPACE@15..16 " "
                    TK_CLOSE_CURLY_CURLY@16..18 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_expression_negative_not_in() {
        check_parse(
            "{{ -n not in [1] }}",
            expect![[r#"
                ROOT@0..19
                  TWIG_VAR@0..19
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..16
                      TWIG_BINARY_EXPRESSION@2..16
                        TWIG_EXPRESSION@2..5
                          TWIG_UNARY_EXPRESSION@2..5
                            TK_WHITESPACE@2..3 " "
                            TK_MINUS@3..4 "-"
                            TWIG_EXPRESSION@4..5
                              TWIG_LITERAL_NAME@4..5
                                TK_WORD@4..5 "n"
                        TK_WHITESPACE@5..6 " "
                        TK_NOT@6..9 "not"
                        TK_WHITESPACE@9..10 " "
                        TK_IN@10..12 "in"
                        TWIG_EXPRESSION@12..16
                          TWIG_LITERAL_ARRAY@12..16
                            TK_WHITESPACE@12..13 " "
                            TK_OPEN_SQUARE@13..14 "["
                            TWIG_EXPRESSION@14..15
                              TWIG_LITERAL_NUMBER@14..15
                                TK_NUMBER@14..15 "1"
                            TK_CLOSE_SQUARE@15..16 "]"
                    TK_WHITESPACE@16..17 " "
                    TK_CLOSE_CURLY_CURLY@17..19 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_expression_boolean_equal_not() {
        check_parse(
            "{{ not a == b }}",
            expect![[r#"
                ROOT@0..16
                  TWIG_VAR@0..16
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..13
                      TWIG_BINARY_EXPRESSION@2..13
                        TWIG_EXPRESSION@2..8
                          TWIG_UNARY_EXPRESSION@2..8
                            TK_WHITESPACE@2..3 " "
                            TK_NOT@3..6 "not"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "a"
                        TK_WHITESPACE@8..9 " "
                        TK_DOUBLE_EQUAL@9..11 "=="
                        TWIG_EXPRESSION@11..13
                          TWIG_LITERAL_NAME@11..13
                            TK_WHITESPACE@11..12 " "
                            TK_WORD@12..13 "b"
                    TK_WHITESPACE@13..14 " "
                    TK_CLOSE_CURLY_CURLY@14..16 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_expression_triple_equal() {
        check_parse(
            "{{ not a === b }}",
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_BINARY_EXPRESSION@2..14
                        TWIG_EXPRESSION@2..8
                          TWIG_UNARY_EXPRESSION@2..8
                            TK_WHITESPACE@2..3 " "
                            TK_NOT@3..6 "not"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "a"
                        TK_WHITESPACE@8..9 " "
                        TK_TRIPLE_EQUAL@9..12 "==="
                        TWIG_EXPRESSION@12..14
                          TWIG_LITERAL_NAME@12..14
                            TK_WHITESPACE@12..13 " "
                            TK_WORD@13..14 "b"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_conditional_expression() {
        check_parse(
            "{{ a > b ? 'Y' : 'N' }}",
            expect![[r#"
                ROOT@0..23
                  TWIG_VAR@0..23
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..20
                      TWIG_CONDITIONAL_EXPRESSION@2..20
                        TWIG_EXPRESSION@2..8
                          TWIG_BINARY_EXPRESSION@2..8
                            TWIG_EXPRESSION@2..4
                              TWIG_LITERAL_NAME@2..4
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..4 "a"
                            TK_WHITESPACE@4..5 " "
                            TK_GREATER_THAN@5..6 ">"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "b"
                        TK_WHITESPACE@8..9 " "
                        TK_QUESTION_MARK@9..10 "?"
                        TWIG_EXPRESSION@10..14
                          TWIG_LITERAL_STRING@10..14
                            TK_WHITESPACE@10..11 " "
                            TK_SINGLE_QUOTES@11..12 "'"
                            TWIG_LITERAL_STRING_INNER@12..13
                              TK_WORD@12..13 "Y"
                            TK_SINGLE_QUOTES@13..14 "'"
                        TK_WHITESPACE@14..15 " "
                        TK_COLON@15..16 ":"
                        TWIG_EXPRESSION@16..20
                          TWIG_LITERAL_STRING@16..20
                            TK_WHITESPACE@16..17 " "
                            TK_SINGLE_QUOTES@17..18 "'"
                            TWIG_LITERAL_STRING_INNER@18..19
                              TK_WORD@18..19 "N"
                            TK_SINGLE_QUOTES@19..20 "'"
                    TK_WHITESPACE@20..21 " "
                    TK_CLOSE_CURLY_CURLY@21..23 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_conditional_expression_without_falsy() {
        // means basically the same as {{ a > b ? 'Y' : '' }}
        check_parse(
            "{{ a > b ? 'Y' }}",
            expect![[r#"
                ROOT@0..17
                  TWIG_VAR@0..17
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..14
                      TWIG_CONDITIONAL_EXPRESSION@2..14
                        TWIG_EXPRESSION@2..8
                          TWIG_BINARY_EXPRESSION@2..8
                            TWIG_EXPRESSION@2..4
                              TWIG_LITERAL_NAME@2..4
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..4 "a"
                            TK_WHITESPACE@4..5 " "
                            TK_GREATER_THAN@5..6 ">"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "b"
                        TK_WHITESPACE@8..9 " "
                        TK_QUESTION_MARK@9..10 "?"
                        TWIG_EXPRESSION@10..14
                          TWIG_LITERAL_STRING@10..14
                            TK_WHITESPACE@10..11 " "
                            TK_SINGLE_QUOTES@11..12 "'"
                            TWIG_LITERAL_STRING_INNER@12..13
                              TK_WORD@12..13 "Y"
                            TK_SINGLE_QUOTES@13..14 "'"
                    TK_WHITESPACE@14..15 " "
                    TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_conditional_expression_without_truthy() {
        // means basically the same as {{ a > b ? a > b : 'N' }}
        check_parse(
            "{{ a > b ?: 'N' }}",
            expect![[r#"
                ROOT@0..18
                  TWIG_VAR@0..18
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..15
                      TWIG_CONDITIONAL_EXPRESSION@2..15
                        TWIG_EXPRESSION@2..8
                          TWIG_BINARY_EXPRESSION@2..8
                            TWIG_EXPRESSION@2..4
                              TWIG_LITERAL_NAME@2..4
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..4 "a"
                            TK_WHITESPACE@4..5 " "
                            TK_GREATER_THAN@5..6 ">"
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "b"
                        TK_WHITESPACE@8..9 " "
                        TK_QUESTION_MARK@9..10 "?"
                        TK_COLON@10..11 ":"
                        TWIG_EXPRESSION@11..15
                          TWIG_LITERAL_STRING@11..15
                            TK_WHITESPACE@11..12 " "
                            TK_SINGLE_QUOTES@12..13 "'"
                            TWIG_LITERAL_STRING_INNER@13..14
                              TK_WORD@13..14 "N"
                            TK_SINGLE_QUOTES@14..15 "'"
                    TK_WHITESPACE@15..16 " "
                    TK_CLOSE_CURLY_CURLY@16..18 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_conditional_expression_nested() {
        check_parse(
            "{{ a ? b ? 'B' : 'N' }}",
            expect![[r#"
                ROOT@0..23
                  TWIG_VAR@0..23
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..20
                      TWIG_CONDITIONAL_EXPRESSION@2..20
                        TWIG_EXPRESSION@2..4
                          TWIG_LITERAL_NAME@2..4
                            TK_WHITESPACE@2..3 " "
                            TK_WORD@3..4 "a"
                        TK_WHITESPACE@4..5 " "
                        TK_QUESTION_MARK@5..6 "?"
                        TWIG_EXPRESSION@6..20
                          TWIG_CONDITIONAL_EXPRESSION@6..20
                            TWIG_EXPRESSION@6..8
                              TWIG_LITERAL_NAME@6..8
                                TK_WHITESPACE@6..7 " "
                                TK_WORD@7..8 "b"
                            TK_WHITESPACE@8..9 " "
                            TK_QUESTION_MARK@9..10 "?"
                            TWIG_EXPRESSION@10..14
                              TWIG_LITERAL_STRING@10..14
                                TK_WHITESPACE@10..11 " "
                                TK_SINGLE_QUOTES@11..12 "'"
                                TWIG_LITERAL_STRING_INNER@12..13
                                  TK_WORD@12..13 "B"
                                TK_SINGLE_QUOTES@13..14 "'"
                            TK_WHITESPACE@14..15 " "
                            TK_COLON@15..16 ":"
                            TWIG_EXPRESSION@16..20
                              TWIG_LITERAL_STRING@16..20
                                TK_WHITESPACE@16..17 " "
                                TK_SINGLE_QUOTES@17..18 "'"
                                TWIG_LITERAL_STRING_INNER@18..19
                                  TK_WORD@18..19 "N"
                                TK_SINGLE_QUOTES@19..20 "'"
                    TK_WHITESPACE@20..21 " "
                    TK_CLOSE_CURLY_CURLY@21..23 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_function_with_is_defined_test() {
        check_parse(
            r#"{{ block("footer", "common_blocks.twig") is defined }}"#,
            expect![[r#"
                ROOT@0..54
                  TWIG_VAR@0..54
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..51
                      TWIG_BINARY_EXPRESSION@2..51
                        TWIG_EXPRESSION@2..40
                          TWIG_FUNCTION_CALL@2..40
                            TWIG_OPERAND@2..8
                              TWIG_LITERAL_NAME@2..8
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..8 "block"
                            TK_OPEN_PARENTHESIS@8..9 "("
                            TWIG_ARGUMENTS@9..39
                              TWIG_EXPRESSION@9..17
                                TWIG_LITERAL_STRING@9..17
                                  TK_DOUBLE_QUOTES@9..10 "\""
                                  TWIG_LITERAL_STRING_INNER@10..16
                                    TK_WORD@10..16 "footer"
                                  TK_DOUBLE_QUOTES@16..17 "\""
                              TK_COMMA@17..18 ","
                              TWIG_EXPRESSION@18..39
                                TWIG_LITERAL_STRING@18..39
                                  TK_WHITESPACE@18..19 " "
                                  TK_DOUBLE_QUOTES@19..20 "\""
                                  TWIG_LITERAL_STRING_INNER@20..38
                                    TK_WORD@20..33 "common_blocks"
                                    TK_DOT@33..34 "."
                                    TK_WORD@34..38 "twig"
                                  TK_DOUBLE_QUOTES@38..39 "\""
                            TK_CLOSE_PARENTHESIS@39..40 ")"
                        TK_WHITESPACE@40..41 " "
                        TK_IS@41..43 "is"
                        TWIG_EXPRESSION@43..51
                          TWIG_LITERAL_NAME@43..51
                            TK_WHITESPACE@43..44 " "
                            TK_WORD@44..51 "defined"
                    TK_WHITESPACE@51..52 " "
                    TK_CLOSE_CURLY_CURLY@52..54 "}}""#]],
        )
    }
}

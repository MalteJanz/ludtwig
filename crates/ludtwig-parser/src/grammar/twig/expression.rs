use crate::grammar::parse_many;
use crate::grammar::twig::literal::{
    parse_postfix_operators, parse_twig_arrow_function, parse_twig_function, parse_twig_literal,
    parse_twig_name,
};
use crate::parser::event::CompletedMarker;
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

use super::Marker;

pub(crate) static TWIG_EXPRESSION_RECOVERY_SET: &[SyntaxKind] = &[
    T!["|"],
    T![")"],
    T!["}"],
    T!["]"],
    T![":"],
    T!["}}"],
    T!["%}"],
    // expressions
    T!["("],
    T!["-"],
    T!["+"],
    T!["not"],
    // literals
    T![number],
    T!["\""],
    T!["'"],
    T!["["],
    T!["null"],
    T!["true"],
    T!["false"],
    T!["{"],
    // operators
    T!["or"],
    T!["||"],
    T!["and"],
    T!["&&"],
    T!["b-or"],
    T!["b-xor"],
    T!["b-and"],
    T!["=="],
    T!["!="],
    T!["<=>"],
    T!["<"],
    T![">"],
    T![">="],
    T!["<="],
    T!["not"],
    T!["in"],
    T!["matches"],
    T!["starts with"],
    T!["ends with"],
    T!["==="],
    T!["!=="],
    T![".."],
    T!["+"],
    T!["-"],
    T!["~"],
    T!["*"],
    T!["/"],
    T!["//"],
    T!["%"],
    T!["is"],
    T!["**"],
    T!["??"],
];

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

    lhs = parse_twig_expression_binding_power_rhs(parser, minimum_binding_power, lhs);
    Some(lhs)
}

pub(crate) fn parse_twig_expression_binding_power_rhs(
    parser: &mut Parser,
    minimum_binding_power: u8,
    mut lhs: CompletedMarker,
) -> CompletedMarker {
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

    lhs
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
        parser.recover(&[T![":"], T!["}}"], T!["%}"]]);
    }

    if parser.at(T![":"]) {
        parser.bump();

        // falsy expression
        if parse_twig_expression_binding_power(parser, 0).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression"));
            parser.recover(&[T!["}}"], T!["%}"]]);
        }
    }

    let conditional_m = parser.complete(m, SyntaxKind::TWIG_CONDITIONAL_EXPRESSION);
    let outer = parser.precede(conditional_m);
    Some(parser.complete(outer, SyntaxKind::TWIG_EXPRESSION))
}

fn parse_twig_expression_lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["("]) {
        let (m, is_paren_expression) = parse_paren_expression(parser);

        if is_paren_expression {
            let node = parser.complete(m, SyntaxKind::TWIG_PARENTHESES_EXPRESSION);
            // including postfix operators
            Some(parse_postfix_operators(parser, node))
        } else {
            Some(parse_paren_arrow_function(parser, m))
        }
    } else if parser.at_set(&[T!["-"], T!["+"], T!["not"]]) {
        Some(parse_unary_expression(parser))
    } else {
        // including postfix operators
        parse_twig_literal(parser).map(|node| parse_postfix_operators(parser, node))
    }
}

fn parse_paren_expression(parser: &mut Parser) -> (Marker, bool) {
    debug_assert!(parser.at(T!["("]));

    let m = parser.start();
    parser.bump();

    // first check for non existent unary expression token to be a arrow function
    if parser
        .peek_token()
        .map_or(false, |t| t.kind.unary_binding_power().is_none())
    {
        // check for a simple name literal
        if let Some(mut lit_m) = parse_twig_name(parser) {
            if parser.at(T![","]) || parser.at_following(&[T![")"], T!["=>"]]) {
                return (m, false); // found arrow function
            }

            // check for optional function call
            if parser.at(T!["("]) {
                lit_m = parse_twig_function(parser, lit_m);
            }

            // including postfix operators
            lit_m = parse_postfix_operators(parser, lit_m);

            // wrap it in expression to continue
            let expr_m = parser.precede(lit_m);
            let lhs = parser.complete(expr_m, SyntaxKind::TWIG_EXPRESSION);
            parse_twig_expression_binding_power_rhs(parser, 0, lhs);
        } else {
            // found a real expression, so it can't be a arrow function at this point anymore
            parse_twig_expression_binding_power(parser, 0);
        }
    } else {
        // found a real expression, so it can't be a arrow function at this point anymore
        parse_twig_expression_binding_power(parser, 0);
    }

    parser.expect(T![")"], TWIG_EXPRESSION_RECOVERY_SET);
    (m, true)
}

fn parse_paren_arrow_function(parser: &mut Parser, m: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T![","]) || parser.at(T![")"]));

    // parse any remaining arguments
    parse_many(
        parser,
        |p| p.at(T![")"]),
        |p| {
            p.expect(T![","], TWIG_EXPRESSION_RECOVERY_SET);
            parse_twig_name(p);
        },
    );

    parser.expect(T![")"], TWIG_EXPRESSION_RECOVERY_SET);
    let last_node = parser.complete(m, SyntaxKind::TWIG_ARGUMENTS);

    parse_twig_arrow_function(parser, last_node)
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
        );
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
        );
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
        );
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
        );
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
        );
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
        );
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
        );
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
        );
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
        );
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
        );
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
                            TWIG_LITERAL_ARRAY_INNER@17..22
                              TWIG_EXPRESSION@17..22
                                TWIG_LITERAL_BOOLEAN@17..22
                                  TK_FALSE@17..22 "false"
                            TK_CLOSE_SQUARE@22..23 "]"
                    TK_WHITESPACE@23..24 " "
                    TK_CLOSE_CURLY_CURLY@24..26 "}}""#]],
        );
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
                            TWIG_LITERAL_ARRAY_INNER@13..14
                              TWIG_EXPRESSION@13..14
                                TWIG_LITERAL_NUMBER@13..14
                                  TK_NUMBER@13..14 "1"
                            TK_CLOSE_SQUARE@14..15 "]"
                    TK_WHITESPACE@15..16 " "
                    TK_CLOSE_CURLY_CURLY@16..18 "}}""#]],
        );
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
                            TWIG_LITERAL_ARRAY_INNER@14..15
                              TWIG_EXPRESSION@14..15
                                TWIG_LITERAL_NUMBER@14..15
                                  TK_NUMBER@14..15 "1"
                            TK_CLOSE_SQUARE@15..16 "]"
                    TK_WHITESPACE@16..17 " "
                    TK_CLOSE_CURLY_CURLY@17..19 "}}""#]],
        );
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
        );
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
        );
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
        );
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
        );
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
        );
    }

    #[test]
    fn parse_twig_conditional_expression_missing_falsy_expression() {
        check_parse(
            "{{ a > b ? 'N' : }}",
            expect![[r#"
            ROOT@0..19
              TWIG_VAR@0..19
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..16
                  TWIG_CONDITIONAL_EXPRESSION@2..16
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
                          TK_WORD@12..13 "N"
                        TK_SINGLE_QUOTES@13..14 "'"
                    TK_WHITESPACE@14..15 " "
                    TK_COLON@15..16 ":"
                TK_WHITESPACE@16..17 " "
                TK_CLOSE_CURLY_CURLY@17..19 "}}"
            error at 17..19: expected twig expression but found }}"#]],
        );
    }

    #[test]
    fn parse_twig_conditional_expression_missing_truthy_expression() {
        check_parse(
            "{{ a > b ? }}",
            expect![[r#"
            ROOT@0..13
              TWIG_VAR@0..13
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..10
                  TWIG_CONDITIONAL_EXPRESSION@2..10
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
                TK_WHITESPACE@10..11 " "
                TK_CLOSE_CURLY_CURLY@11..13 "}}"
            error at 11..13: expected twig expression or ':' but found }}"#]],
        );
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
        );
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
                            TWIG_ARGUMENTS@8..40
                              TK_OPEN_PARENTHESIS@8..9 "("
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
        );
    }

    #[test]
    fn parse_twig_expression_is_even() {
        check_parse(
            r#"{{ var is even }}"#,
            expect![[r#"
            ROOT@0..17
              TWIG_VAR@0..17
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..14
                  TWIG_BINARY_EXPRESSION@2..14
                    TWIG_EXPRESSION@2..6
                      TWIG_LITERAL_NAME@2..6
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..6 "var"
                    TK_WHITESPACE@6..7 " "
                    TK_IS@7..9 "is"
                    TWIG_EXPRESSION@9..14
                      TWIG_LITERAL_NAME@9..14
                        TK_WHITESPACE@9..10 " "
                        TK_WORD@10..14 "even"
                TK_WHITESPACE@14..15 " "
                TK_CLOSE_CURLY_CURLY@15..17 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_expression_is_same_as() {
        check_parse(
            r#"{{ foo.attribute is same as(false) }}"#,
            expect![[r#"
                ROOT@0..37
                  TWIG_VAR@0..37
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..34
                      TWIG_BINARY_EXPRESSION@2..34
                        TWIG_EXPRESSION@2..16
                          TWIG_ACCESSOR@2..16
                            TWIG_OPERAND@2..6
                              TWIG_LITERAL_NAME@2..6
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..6 "foo"
                            TK_DOT@6..7 "."
                            TWIG_OPERAND@7..16
                              TWIG_LITERAL_NAME@7..16
                                TK_WORD@7..16 "attribute"
                        TK_WHITESPACE@16..17 " "
                        TK_IS@17..19 "is"
                        TWIG_EXPRESSION@19..34
                          TWIG_FUNCTION_CALL@19..34
                            TWIG_OPERAND@19..27
                              TWIG_LITERAL_NAME@19..27
                                TK_WHITESPACE@19..20 " "
                                TK_WORD@20..27 "same as"
                            TWIG_ARGUMENTS@27..34
                              TK_OPEN_PARENTHESIS@27..28 "("
                              TWIG_EXPRESSION@28..33
                                TWIG_LITERAL_BOOLEAN@28..33
                                  TK_FALSE@28..33 "false"
                              TK_CLOSE_PARENTHESIS@33..34 ")"
                    TK_WHITESPACE@34..35 " "
                    TK_CLOSE_CURLY_CURLY@35..37 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_expression_is_divisible_by() {
        check_parse(
            r#"{{ foo.attribute is divisible by(false) }}"#,
            expect![[r#"
                ROOT@0..42
                  TWIG_VAR@0..42
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..39
                      TWIG_BINARY_EXPRESSION@2..39
                        TWIG_EXPRESSION@2..16
                          TWIG_ACCESSOR@2..16
                            TWIG_OPERAND@2..6
                              TWIG_LITERAL_NAME@2..6
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..6 "foo"
                            TK_DOT@6..7 "."
                            TWIG_OPERAND@7..16
                              TWIG_LITERAL_NAME@7..16
                                TK_WORD@7..16 "attribute"
                        TK_WHITESPACE@16..17 " "
                        TK_IS@17..19 "is"
                        TWIG_EXPRESSION@19..39
                          TWIG_FUNCTION_CALL@19..39
                            TWIG_OPERAND@19..32
                              TWIG_LITERAL_NAME@19..32
                                TK_WHITESPACE@19..20 " "
                                TK_WORD@20..32 "divisible by"
                            TWIG_ARGUMENTS@32..39
                              TK_OPEN_PARENTHESIS@32..33 "("
                              TWIG_EXPRESSION@33..38
                                TWIG_LITERAL_BOOLEAN@33..38
                                  TK_FALSE@33..38 "false"
                              TK_CLOSE_PARENTHESIS@38..39 ")"
                    TK_WHITESPACE@39..40 " "
                    TK_CLOSE_CURLY_CURLY@40..42 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_unary_filter() {
        // The abs filter should apply to '5' and is then negated
        check_parse(
            r#"{{ -5|abs }}"#,
            expect![[r#"
                ROOT@0..12
                  TWIG_VAR@0..12
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..9
                      TWIG_UNARY_EXPRESSION@2..9
                        TK_WHITESPACE@2..3 " "
                        TK_MINUS@3..4 "-"
                        TWIG_EXPRESSION@4..9
                          TWIG_FILTER@4..9
                            TWIG_OPERAND@4..5
                              TWIG_LITERAL_NUMBER@4..5
                                TK_NUMBER@4..5 "5"
                            TK_SINGLE_PIPE@5..6 "|"
                            TWIG_OPERAND@6..9
                              TWIG_LITERAL_NAME@6..9
                                TK_WORD@6..9 "abs"
                    TK_WHITESPACE@9..10 " "
                    TK_CLOSE_CURLY_CURLY@10..12 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_unary_parentheses_filter() {
        // The abs filter should apply to '-5'
        check_parse(
            r#"{{ (-5)|abs }}"#,
            expect![[r#"
                ROOT@0..14
                  TWIG_VAR@0..14
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..11
                      TWIG_FILTER@2..11
                        TWIG_OPERAND@2..7
                          TWIG_PARENTHESES_EXPRESSION@2..7
                            TK_WHITESPACE@2..3 " "
                            TK_OPEN_PARENTHESIS@3..4 "("
                            TWIG_EXPRESSION@4..6
                              TWIG_UNARY_EXPRESSION@4..6
                                TK_MINUS@4..5 "-"
                                TWIG_EXPRESSION@5..6
                                  TWIG_LITERAL_NUMBER@5..6
                                    TK_NUMBER@5..6 "5"
                            TK_CLOSE_PARENTHESIS@6..7 ")"
                        TK_SINGLE_PIPE@7..8 "|"
                        TWIG_OPERAND@8..11
                          TWIG_LITERAL_NAME@8..11
                            TK_WORD@8..11 "abs"
                    TK_WHITESPACE@11..12 " "
                    TK_CLOSE_CURLY_CURLY@12..14 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_parenthesis_expression_filter() {
        check_parse(
            r#"{{ ('a' ~ 'b')|trim }}"#,
            expect![[r#"
                ROOT@0..22
                  TWIG_VAR@0..22
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..19
                      TWIG_FILTER@2..19
                        TWIG_OPERAND@2..14
                          TWIG_PARENTHESES_EXPRESSION@2..14
                            TK_WHITESPACE@2..3 " "
                            TK_OPEN_PARENTHESIS@3..4 "("
                            TWIG_EXPRESSION@4..13
                              TWIG_BINARY_EXPRESSION@4..13
                                TWIG_EXPRESSION@4..7
                                  TWIG_LITERAL_STRING@4..7
                                    TK_SINGLE_QUOTES@4..5 "'"
                                    TWIG_LITERAL_STRING_INNER@5..6
                                      TK_WORD@5..6 "a"
                                    TK_SINGLE_QUOTES@6..7 "'"
                                TK_WHITESPACE@7..8 " "
                                TK_TILDE@8..9 "~"
                                TWIG_EXPRESSION@9..13
                                  TWIG_LITERAL_STRING@9..13
                                    TK_WHITESPACE@9..10 " "
                                    TK_SINGLE_QUOTES@10..11 "'"
                                    TWIG_LITERAL_STRING_INNER@11..12
                                      TK_WORD@11..12 "b"
                                    TK_SINGLE_QUOTES@12..13 "'"
                            TK_CLOSE_PARENTHESIS@13..14 ")"
                        TK_SINGLE_PIPE@14..15 "|"
                        TWIG_OPERAND@15..19
                          TWIG_LITERAL_NAME@15..19
                            TK_WORD@15..19 "trim"
                    TK_WHITESPACE@19..20 " "
                    TK_CLOSE_CURLY_CURLY@20..22 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_parenthesis_expression_multiple_filters() {
        check_parse(
            r#"{{ ('a' ~ 'b')|trim|escape }}"#,
            expect![[r#"
                ROOT@0..29
                  TWIG_VAR@0..29
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..26
                      TWIG_FILTER@2..26
                        TWIG_OPERAND@2..19
                          TWIG_FILTER@2..19
                            TWIG_OPERAND@2..14
                              TWIG_PARENTHESES_EXPRESSION@2..14
                                TK_WHITESPACE@2..3 " "
                                TK_OPEN_PARENTHESIS@3..4 "("
                                TWIG_EXPRESSION@4..13
                                  TWIG_BINARY_EXPRESSION@4..13
                                    TWIG_EXPRESSION@4..7
                                      TWIG_LITERAL_STRING@4..7
                                        TK_SINGLE_QUOTES@4..5 "'"
                                        TWIG_LITERAL_STRING_INNER@5..6
                                          TK_WORD@5..6 "a"
                                        TK_SINGLE_QUOTES@6..7 "'"
                                    TK_WHITESPACE@7..8 " "
                                    TK_TILDE@8..9 "~"
                                    TWIG_EXPRESSION@9..13
                                      TWIG_LITERAL_STRING@9..13
                                        TK_WHITESPACE@9..10 " "
                                        TK_SINGLE_QUOTES@10..11 "'"
                                        TWIG_LITERAL_STRING_INNER@11..12
                                          TK_WORD@11..12 "b"
                                        TK_SINGLE_QUOTES@12..13 "'"
                                TK_CLOSE_PARENTHESIS@13..14 ")"
                            TK_SINGLE_PIPE@14..15 "|"
                            TWIG_OPERAND@15..19
                              TWIG_LITERAL_NAME@15..19
                                TK_WORD@15..19 "trim"
                        TK_SINGLE_PIPE@19..20 "|"
                        TWIG_OPERAND@20..26
                          TWIG_LITERAL_NAME@20..26
                            TK_WORD@20..26 "escape"
                    TK_WHITESPACE@26..27 " "
                    TK_CLOSE_CURLY_CURLY@27..29 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_filter_accessor_plus_one() {
        check_parse(
            r#"{{ thumbnails|first.width + 1 }}"#,
            expect![[r#"
            ROOT@0..32
              TWIG_VAR@0..32
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_EXPRESSION@2..29
                  TWIG_BINARY_EXPRESSION@2..29
                    TWIG_EXPRESSION@2..25
                      TWIG_ACCESSOR@2..25
                        TWIG_OPERAND@2..19
                          TWIG_FILTER@2..19
                            TWIG_OPERAND@2..13
                              TWIG_LITERAL_NAME@2..13
                                TK_WHITESPACE@2..3 " "
                                TK_WORD@3..13 "thumbnails"
                            TK_SINGLE_PIPE@13..14 "|"
                            TWIG_OPERAND@14..19
                              TWIG_LITERAL_NAME@14..19
                                TK_WORD@14..19 "first"
                        TK_DOT@19..20 "."
                        TWIG_OPERAND@20..25
                          TWIG_LITERAL_NAME@20..25
                            TK_WORD@20..25 "width"
                    TK_WHITESPACE@25..26 " "
                    TK_PLUS@26..27 "+"
                    TWIG_EXPRESSION@27..29
                      TWIG_LITERAL_NUMBER@27..29
                        TK_WHITESPACE@27..28 " "
                        TK_NUMBER@28..29 "1"
                TK_WHITESPACE@29..30 " "
                TK_CLOSE_CURLY_CURLY@30..32 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_array_declartion_and_index() {
        check_parse(
            r#"{{ [0, 1][0] }}"#,
            expect![[r#"
                ROOT@0..15
                  TWIG_VAR@0..15
                    TK_OPEN_CURLY_CURLY@0..2 "{{"
                    TWIG_EXPRESSION@2..12
                      TWIG_INDEX_LOOKUP@2..12
                        TWIG_OPERAND@2..9
                          TWIG_LITERAL_ARRAY@2..9
                            TK_WHITESPACE@2..3 " "
                            TK_OPEN_SQUARE@3..4 "["
                            TWIG_LITERAL_ARRAY_INNER@4..8
                              TWIG_EXPRESSION@4..5
                                TWIG_LITERAL_NUMBER@4..5
                                  TK_NUMBER@4..5 "0"
                              TK_COMMA@5..6 ","
                              TWIG_EXPRESSION@6..8
                                TWIG_LITERAL_NUMBER@6..8
                                  TK_WHITESPACE@6..7 " "
                                  TK_NUMBER@7..8 "1"
                            TK_CLOSE_SQUARE@8..9 "]"
                        TK_OPEN_SQUARE@9..10 "["
                        TWIG_INDEX@10..11
                          TWIG_EXPRESSION@10..11
                            TWIG_LITERAL_NUMBER@10..11
                              TK_NUMBER@10..11 "0"
                        TK_CLOSE_SQUARE@11..12 "]"
                    TK_WHITESPACE@12..13 " "
                    TK_CLOSE_CURLY_CURLY@13..15 "}}""#]],
        );
    }

    #[test]
    fn parse_twig_paren_expression_with_ternary() {
        check_parse(
            r#"{% set value = (shippingMethod.media.translated.alt ?: shippingMethod.translated.name) %}"#,
            expect![[r#"
                ROOT@0..89
                  TWIG_SET@0..89
                    TWIG_SET_BLOCK@0..89
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..86
                        TWIG_LITERAL_NAME@6..12
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..12 "value"
                        TK_WHITESPACE@12..13 " "
                        TK_EQUAL@13..14 "="
                        TWIG_EXPRESSION@14..86
                          TWIG_PARENTHESES_EXPRESSION@14..86
                            TK_WHITESPACE@14..15 " "
                            TK_OPEN_PARENTHESIS@15..16 "("
                            TWIG_EXPRESSION@16..85
                              TWIG_CONDITIONAL_EXPRESSION@16..85
                                TWIG_EXPRESSION@16..51
                                  TWIG_ACCESSOR@16..51
                                    TWIG_OPERAND@16..47
                                      TWIG_ACCESSOR@16..47
                                        TWIG_OPERAND@16..36
                                          TWIG_ACCESSOR@16..36
                                            TWIG_OPERAND@16..30
                                              TWIG_LITERAL_NAME@16..30
                                                TK_WORD@16..30 "shippingMethod"
                                            TK_DOT@30..31 "."
                                            TWIG_OPERAND@31..36
                                              TWIG_LITERAL_NAME@31..36
                                                TK_WORD@31..36 "media"
                                        TK_DOT@36..37 "."
                                        TWIG_OPERAND@37..47
                                          TWIG_LITERAL_NAME@37..47
                                            TK_WORD@37..47 "translated"
                                    TK_DOT@47..48 "."
                                    TWIG_OPERAND@48..51
                                      TWIG_LITERAL_NAME@48..51
                                        TK_WORD@48..51 "alt"
                                TK_WHITESPACE@51..52 " "
                                TK_QUESTION_MARK@52..53 "?"
                                TK_COLON@53..54 ":"
                                TWIG_EXPRESSION@54..85
                                  TWIG_ACCESSOR@54..85
                                    TWIG_OPERAND@54..80
                                      TWIG_ACCESSOR@54..80
                                        TWIG_OPERAND@54..69
                                          TWIG_LITERAL_NAME@54..69
                                            TK_WHITESPACE@54..55 " "
                                            TK_WORD@55..69 "shippingMethod"
                                        TK_DOT@69..70 "."
                                        TWIG_OPERAND@70..80
                                          TWIG_LITERAL_NAME@70..80
                                            TK_WORD@70..80 "translated"
                                    TK_DOT@80..81 "."
                                    TWIG_OPERAND@81..85
                                      TWIG_LITERAL_NAME@81..85
                                        TK_WORD@81..85 "name"
                            TK_CLOSE_PARENTHESIS@85..86 ")"
                      TK_WHITESPACE@86..87 " "
                      TK_PERCENT_CURLY@87..89 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_paren_expression_with_post_operator_and_addition() {
        check_parse(
            r#"{% if (product.price + 1 > 1) %} okey {% endif %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_IF@0..49
                    TWIG_IF_BLOCK@0..32
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_IF@3..5 "if"
                      TWIG_EXPRESSION@5..29
                        TWIG_PARENTHESES_EXPRESSION@5..29
                          TK_WHITESPACE@5..6 " "
                          TK_OPEN_PARENTHESIS@6..7 "("
                          TWIG_EXPRESSION@7..28
                            TWIG_BINARY_EXPRESSION@7..28
                              TWIG_BINARY_EXPRESSION@7..24
                                TWIG_EXPRESSION@7..20
                                  TWIG_ACCESSOR@7..20
                                    TWIG_OPERAND@7..14
                                      TWIG_LITERAL_NAME@7..14
                                        TK_WORD@7..14 "product"
                                    TK_DOT@14..15 "."
                                    TWIG_OPERAND@15..20
                                      TWIG_LITERAL_NAME@15..20
                                        TK_WORD@15..20 "price"
                                TK_WHITESPACE@20..21 " "
                                TK_PLUS@21..22 "+"
                                TWIG_EXPRESSION@22..24
                                  TWIG_LITERAL_NUMBER@22..24
                                    TK_WHITESPACE@22..23 " "
                                    TK_NUMBER@23..24 "1"
                              TK_WHITESPACE@24..25 " "
                              TK_GREATER_THAN@25..26 ">"
                              TWIG_EXPRESSION@26..28
                                TWIG_LITERAL_NUMBER@26..28
                                  TK_WHITESPACE@26..27 " "
                                  TK_NUMBER@27..28 "1"
                          TK_CLOSE_PARENTHESIS@28..29 ")"
                      TK_WHITESPACE@29..30 " "
                      TK_PERCENT_CURLY@30..32 "%}"
                    BODY@32..37
                      HTML_TEXT@32..37
                        TK_WHITESPACE@32..33 " "
                        TK_WORD@33..37 "okey"
                    TWIG_ENDIF_BLOCK@37..49
                      TK_WHITESPACE@37..38 " "
                      TK_CURLY_PERCENT@38..40 "{%"
                      TK_WHITESPACE@40..41 " "
                      TK_ENDIF@41..46 "endif"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_paren_expression_complex() {
        check_parse(
            r#"{% set isDiscount = (not lineItem.good and lineItem.price.totalPrice <= 0) or lineItem.type == DISCOUNT_LINE_ITEM_TYPE %}"#,
            expect![[r#"
                ROOT@0..121
                  TWIG_SET@0..121
                    TWIG_SET_BLOCK@0..121
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..118
                        TWIG_LITERAL_NAME@6..17
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..17 "isDiscount"
                        TK_WHITESPACE@17..18 " "
                        TK_EQUAL@18..19 "="
                        TWIG_EXPRESSION@19..118
                          TWIG_BINARY_EXPRESSION@19..118
                            TWIG_EXPRESSION@19..74
                              TWIG_PARENTHESES_EXPRESSION@19..74
                                TK_WHITESPACE@19..20 " "
                                TK_OPEN_PARENTHESIS@20..21 "("
                                TWIG_EXPRESSION@21..73
                                  TWIG_BINARY_EXPRESSION@21..73
                                    TWIG_EXPRESSION@21..38
                                      TWIG_UNARY_EXPRESSION@21..38
                                        TK_NOT@21..24 "not"
                                        TWIG_EXPRESSION@24..38
                                          TWIG_ACCESSOR@24..38
                                            TWIG_OPERAND@24..33
                                              TWIG_LITERAL_NAME@24..33
                                                TK_WHITESPACE@24..25 " "
                                                TK_WORD@25..33 "lineItem"
                                            TK_DOT@33..34 "."
                                            TWIG_OPERAND@34..38
                                              TWIG_LITERAL_NAME@34..38
                                                TK_WORD@34..38 "good"
                                    TK_WHITESPACE@38..39 " "
                                    TK_AND@39..42 "and"
                                    TWIG_EXPRESSION@42..73
                                      TWIG_BINARY_EXPRESSION@42..73
                                        TWIG_EXPRESSION@42..68
                                          TWIG_ACCESSOR@42..68
                                            TWIG_OPERAND@42..57
                                              TWIG_ACCESSOR@42..57
                                                TWIG_OPERAND@42..51
                                                  TWIG_LITERAL_NAME@42..51
                                                    TK_WHITESPACE@42..43 " "
                                                    TK_WORD@43..51 "lineItem"
                                                TK_DOT@51..52 "."
                                                TWIG_OPERAND@52..57
                                                  TWIG_LITERAL_NAME@52..57
                                                    TK_WORD@52..57 "price"
                                            TK_DOT@57..58 "."
                                            TWIG_OPERAND@58..68
                                              TWIG_LITERAL_NAME@58..68
                                                TK_WORD@58..68 "totalPrice"
                                        TK_WHITESPACE@68..69 " "
                                        TK_LESS_THAN_EQUAL@69..71 "<="
                                        TWIG_EXPRESSION@71..73
                                          TWIG_LITERAL_NUMBER@71..73
                                            TK_WHITESPACE@71..72 " "
                                            TK_NUMBER@72..73 "0"
                                TK_CLOSE_PARENTHESIS@73..74 ")"
                            TK_WHITESPACE@74..75 " "
                            TK_OR@75..77 "or"
                            TWIG_EXPRESSION@77..118
                              TWIG_BINARY_EXPRESSION@77..118
                                TWIG_EXPRESSION@77..91
                                  TWIG_ACCESSOR@77..91
                                    TWIG_OPERAND@77..86
                                      TWIG_LITERAL_NAME@77..86
                                        TK_WHITESPACE@77..78 " "
                                        TK_WORD@78..86 "lineItem"
                                    TK_DOT@86..87 "."
                                    TWIG_OPERAND@87..91
                                      TWIG_LITERAL_NAME@87..91
                                        TK_WORD@87..91 "type"
                                TK_WHITESPACE@91..92 " "
                                TK_DOUBLE_EQUAL@92..94 "=="
                                TWIG_EXPRESSION@94..118
                                  TWIG_LITERAL_NAME@94..118
                                    TK_WHITESPACE@94..95 " "
                                    TK_WORD@95..118 "DISCOUNT_LINE_ITEM_TYPE"
                      TK_WHITESPACE@118..119 " "
                      TK_PERCENT_CURLY@119..121 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_paren_expression_nested_in_binary() {
        check_parse(
            r#"{% set sizes = (theme_config('breakpoint.sm') - 1) ~ 'px' %}"#,
            expect![[r#"
                ROOT@0..60
                  TWIG_SET@0..60
                    TWIG_SET_BLOCK@0..60
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..57
                        TWIG_LITERAL_NAME@6..12
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..12 "sizes"
                        TK_WHITESPACE@12..13 " "
                        TK_EQUAL@13..14 "="
                        TWIG_EXPRESSION@14..57
                          TWIG_BINARY_EXPRESSION@14..57
                            TWIG_EXPRESSION@14..50
                              TWIG_PARENTHESES_EXPRESSION@14..50
                                TK_WHITESPACE@14..15 " "
                                TK_OPEN_PARENTHESIS@15..16 "("
                                TWIG_EXPRESSION@16..49
                                  TWIG_BINARY_EXPRESSION@16..49
                                    TWIG_EXPRESSION@16..45
                                      TWIG_FUNCTION_CALL@16..45
                                        TWIG_OPERAND@16..28
                                          TWIG_LITERAL_NAME@16..28
                                            TK_WORD@16..28 "theme_config"
                                        TWIG_ARGUMENTS@28..45
                                          TK_OPEN_PARENTHESIS@28..29 "("
                                          TWIG_EXPRESSION@29..44
                                            TWIG_LITERAL_STRING@29..44
                                              TK_SINGLE_QUOTES@29..30 "'"
                                              TWIG_LITERAL_STRING_INNER@30..43
                                                TK_WORD@30..40 "breakpoint"
                                                TK_DOT@40..41 "."
                                                TK_WORD@41..43 "sm"
                                              TK_SINGLE_QUOTES@43..44 "'"
                                          TK_CLOSE_PARENTHESIS@44..45 ")"
                                    TK_WHITESPACE@45..46 " "
                                    TK_MINUS@46..47 "-"
                                    TWIG_EXPRESSION@47..49
                                      TWIG_LITERAL_NUMBER@47..49
                                        TK_WHITESPACE@47..48 " "
                                        TK_NUMBER@48..49 "1"
                                TK_CLOSE_PARENTHESIS@49..50 ")"
                            TK_WHITESPACE@50..51 " "
                            TK_TILDE@51..52 "~"
                            TWIG_EXPRESSION@52..57
                              TWIG_LITERAL_STRING@52..57
                                TK_WHITESPACE@52..53 " "
                                TK_SINGLE_QUOTES@53..54 "'"
                                TWIG_LITERAL_STRING_INNER@54..56
                                  TK_WORD@54..56 "px"
                                TK_SINGLE_QUOTES@56..57 "'"
                      TK_WHITESPACE@57..58 " "
                      TK_PERCENT_CURLY@58..60 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_paren_expression_inside_hash() {
        check_parse(
            r#"{% set sizes = {
                xs: (theme_config('breakpoint.sm') - 1) ~ 'px',
                sm: (theme_config('breakpoint.md') - 1) ~'px',
                md: ((theme_config('breakpoint.lg') - 1) / columns)|round(0, 'ceil') ~'px',
                lg: ((theme_config('breakpoint.xl') - 1) / columns)|round(0, 'ceil') ~'px'
            } %}"#,
            expect![[r#"
                ROOT@0..343
                  TWIG_SET@0..343
                    TWIG_SET_BLOCK@0..343
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..340
                        TWIG_LITERAL_NAME@6..12
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..12 "sizes"
                        TK_WHITESPACE@12..13 " "
                        TK_EQUAL@13..14 "="
                        TWIG_EXPRESSION@14..340
                          TWIG_LITERAL_HASH@14..340
                            TK_WHITESPACE@14..15 " "
                            TK_OPEN_CURLY@15..16 "{"
                            TWIG_LITERAL_HASH_ITEMS@16..326
                              TWIG_LITERAL_HASH_PAIR@16..79
                                TWIG_LITERAL_HASH_KEY@16..35
                                  TK_LINE_BREAK@16..17 "\n"
                                  TK_WHITESPACE@17..33 "                "
                                  TK_WORD@33..35 "xs"
                                TK_COLON@35..36 ":"
                                TWIG_EXPRESSION@36..79
                                  TWIG_BINARY_EXPRESSION@36..79
                                    TWIG_EXPRESSION@36..72
                                      TWIG_PARENTHESES_EXPRESSION@36..72
                                        TK_WHITESPACE@36..37 " "
                                        TK_OPEN_PARENTHESIS@37..38 "("
                                        TWIG_EXPRESSION@38..71
                                          TWIG_BINARY_EXPRESSION@38..71
                                            TWIG_EXPRESSION@38..67
                                              TWIG_FUNCTION_CALL@38..67
                                                TWIG_OPERAND@38..50
                                                  TWIG_LITERAL_NAME@38..50
                                                    TK_WORD@38..50 "theme_config"
                                                TWIG_ARGUMENTS@50..67
                                                  TK_OPEN_PARENTHESIS@50..51 "("
                                                  TWIG_EXPRESSION@51..66
                                                    TWIG_LITERAL_STRING@51..66
                                                      TK_SINGLE_QUOTES@51..52 "'"
                                                      TWIG_LITERAL_STRING_INNER@52..65
                                                        TK_WORD@52..62 "breakpoint"
                                                        TK_DOT@62..63 "."
                                                        TK_WORD@63..65 "sm"
                                                      TK_SINGLE_QUOTES@65..66 "'"
                                                  TK_CLOSE_PARENTHESIS@66..67 ")"
                                            TK_WHITESPACE@67..68 " "
                                            TK_MINUS@68..69 "-"
                                            TWIG_EXPRESSION@69..71
                                              TWIG_LITERAL_NUMBER@69..71
                                                TK_WHITESPACE@69..70 " "
                                                TK_NUMBER@70..71 "1"
                                        TK_CLOSE_PARENTHESIS@71..72 ")"
                                    TK_WHITESPACE@72..73 " "
                                    TK_TILDE@73..74 "~"
                                    TWIG_EXPRESSION@74..79
                                      TWIG_LITERAL_STRING@74..79
                                        TK_WHITESPACE@74..75 " "
                                        TK_SINGLE_QUOTES@75..76 "'"
                                        TWIG_LITERAL_STRING_INNER@76..78
                                          TK_WORD@76..78 "px"
                                        TK_SINGLE_QUOTES@78..79 "'"
                              TK_COMMA@79..80 ","
                              TWIG_LITERAL_HASH_PAIR@80..142
                                TWIG_LITERAL_HASH_KEY@80..99
                                  TK_LINE_BREAK@80..81 "\n"
                                  TK_WHITESPACE@81..97 "                "
                                  TK_WORD@97..99 "sm"
                                TK_COLON@99..100 ":"
                                TWIG_EXPRESSION@100..142
                                  TWIG_BINARY_EXPRESSION@100..142
                                    TWIG_EXPRESSION@100..136
                                      TWIG_PARENTHESES_EXPRESSION@100..136
                                        TK_WHITESPACE@100..101 " "
                                        TK_OPEN_PARENTHESIS@101..102 "("
                                        TWIG_EXPRESSION@102..135
                                          TWIG_BINARY_EXPRESSION@102..135
                                            TWIG_EXPRESSION@102..131
                                              TWIG_FUNCTION_CALL@102..131
                                                TWIG_OPERAND@102..114
                                                  TWIG_LITERAL_NAME@102..114
                                                    TK_WORD@102..114 "theme_config"
                                                TWIG_ARGUMENTS@114..131
                                                  TK_OPEN_PARENTHESIS@114..115 "("
                                                  TWIG_EXPRESSION@115..130
                                                    TWIG_LITERAL_STRING@115..130
                                                      TK_SINGLE_QUOTES@115..116 "'"
                                                      TWIG_LITERAL_STRING_INNER@116..129
                                                        TK_WORD@116..126 "breakpoint"
                                                        TK_DOT@126..127 "."
                                                        TK_WORD@127..129 "md"
                                                      TK_SINGLE_QUOTES@129..130 "'"
                                                  TK_CLOSE_PARENTHESIS@130..131 ")"
                                            TK_WHITESPACE@131..132 " "
                                            TK_MINUS@132..133 "-"
                                            TWIG_EXPRESSION@133..135
                                              TWIG_LITERAL_NUMBER@133..135
                                                TK_WHITESPACE@133..134 " "
                                                TK_NUMBER@134..135 "1"
                                        TK_CLOSE_PARENTHESIS@135..136 ")"
                                    TK_WHITESPACE@136..137 " "
                                    TK_TILDE@137..138 "~"
                                    TWIG_EXPRESSION@138..142
                                      TWIG_LITERAL_STRING@138..142
                                        TK_SINGLE_QUOTES@138..139 "'"
                                        TWIG_LITERAL_STRING_INNER@139..141
                                          TK_WORD@139..141 "px"
                                        TK_SINGLE_QUOTES@141..142 "'"
                              TK_COMMA@142..143 ","
                              TWIG_LITERAL_HASH_PAIR@143..234
                                TWIG_LITERAL_HASH_KEY@143..162
                                  TK_LINE_BREAK@143..144 "\n"
                                  TK_WHITESPACE@144..160 "                "
                                  TK_WORD@160..162 "md"
                                TK_COLON@162..163 ":"
                                TWIG_EXPRESSION@163..234
                                  TWIG_BINARY_EXPRESSION@163..234
                                    TWIG_EXPRESSION@163..228
                                      TWIG_FILTER@163..228
                                        TWIG_OPERAND@163..211
                                          TWIG_PARENTHESES_EXPRESSION@163..211
                                            TK_WHITESPACE@163..164 " "
                                            TK_OPEN_PARENTHESIS@164..165 "("
                                            TWIG_EXPRESSION@165..210
                                              TWIG_BINARY_EXPRESSION@165..210
                                                TWIG_EXPRESSION@165..200
                                                  TWIG_PARENTHESES_EXPRESSION@165..200
                                                    TK_OPEN_PARENTHESIS@165..166 "("
                                                    TWIG_EXPRESSION@166..199
                                                      TWIG_BINARY_EXPRESSION@166..199
                                                        TWIG_EXPRESSION@166..195
                                                          TWIG_FUNCTION_CALL@166..195
                                                            TWIG_OPERAND@166..178
                                                              TWIG_LITERAL_NAME@166..178
                                                                TK_WORD@166..178 "theme_config"
                                                            TWIG_ARGUMENTS@178..195
                                                              TK_OPEN_PARENTHESIS@178..179 "("
                                                              TWIG_EXPRESSION@179..194
                                                                TWIG_LITERAL_STRING@179..194
                                                                  TK_SINGLE_QUOTES@179..180 "'"
                                                                  TWIG_LITERAL_STRING_INNER@180..193
                                                                    TK_WORD@180..190 "breakpoint"
                                                                    TK_DOT@190..191 "."
                                                                    TK_WORD@191..193 "lg"
                                                                  TK_SINGLE_QUOTES@193..194 "'"
                                                              TK_CLOSE_PARENTHESIS@194..195 ")"
                                                        TK_WHITESPACE@195..196 " "
                                                        TK_MINUS@196..197 "-"
                                                        TWIG_EXPRESSION@197..199
                                                          TWIG_LITERAL_NUMBER@197..199
                                                            TK_WHITESPACE@197..198 " "
                                                            TK_NUMBER@198..199 "1"
                                                    TK_CLOSE_PARENTHESIS@199..200 ")"
                                                TK_WHITESPACE@200..201 " "
                                                TK_FORWARD_SLASH@201..202 "/"
                                                TWIG_EXPRESSION@202..210
                                                  TWIG_LITERAL_NAME@202..210
                                                    TK_WHITESPACE@202..203 " "
                                                    TK_WORD@203..210 "columns"
                                            TK_CLOSE_PARENTHESIS@210..211 ")"
                                        TK_SINGLE_PIPE@211..212 "|"
                                        TWIG_OPERAND@212..228
                                          TWIG_LITERAL_NAME@212..217
                                            TK_WORD@212..217 "round"
                                          TWIG_ARGUMENTS@217..228
                                            TK_OPEN_PARENTHESIS@217..218 "("
                                            TWIG_EXPRESSION@218..219
                                              TWIG_LITERAL_NUMBER@218..219
                                                TK_NUMBER@218..219 "0"
                                            TK_COMMA@219..220 ","
                                            TWIG_EXPRESSION@220..227
                                              TWIG_LITERAL_STRING@220..227
                                                TK_WHITESPACE@220..221 " "
                                                TK_SINGLE_QUOTES@221..222 "'"
                                                TWIG_LITERAL_STRING_INNER@222..226
                                                  TK_WORD@222..226 "ceil"
                                                TK_SINGLE_QUOTES@226..227 "'"
                                            TK_CLOSE_PARENTHESIS@227..228 ")"
                                    TK_WHITESPACE@228..229 " "
                                    TK_TILDE@229..230 "~"
                                    TWIG_EXPRESSION@230..234
                                      TWIG_LITERAL_STRING@230..234
                                        TK_SINGLE_QUOTES@230..231 "'"
                                        TWIG_LITERAL_STRING_INNER@231..233
                                          TK_WORD@231..233 "px"
                                        TK_SINGLE_QUOTES@233..234 "'"
                              TK_COMMA@234..235 ","
                              TWIG_LITERAL_HASH_PAIR@235..326
                                TWIG_LITERAL_HASH_KEY@235..254
                                  TK_LINE_BREAK@235..236 "\n"
                                  TK_WHITESPACE@236..252 "                "
                                  TK_WORD@252..254 "lg"
                                TK_COLON@254..255 ":"
                                TWIG_EXPRESSION@255..326
                                  TWIG_BINARY_EXPRESSION@255..326
                                    TWIG_EXPRESSION@255..320
                                      TWIG_FILTER@255..320
                                        TWIG_OPERAND@255..303
                                          TWIG_PARENTHESES_EXPRESSION@255..303
                                            TK_WHITESPACE@255..256 " "
                                            TK_OPEN_PARENTHESIS@256..257 "("
                                            TWIG_EXPRESSION@257..302
                                              TWIG_BINARY_EXPRESSION@257..302
                                                TWIG_EXPRESSION@257..292
                                                  TWIG_PARENTHESES_EXPRESSION@257..292
                                                    TK_OPEN_PARENTHESIS@257..258 "("
                                                    TWIG_EXPRESSION@258..291
                                                      TWIG_BINARY_EXPRESSION@258..291
                                                        TWIG_EXPRESSION@258..287
                                                          TWIG_FUNCTION_CALL@258..287
                                                            TWIG_OPERAND@258..270
                                                              TWIG_LITERAL_NAME@258..270
                                                                TK_WORD@258..270 "theme_config"
                                                            TWIG_ARGUMENTS@270..287
                                                              TK_OPEN_PARENTHESIS@270..271 "("
                                                              TWIG_EXPRESSION@271..286
                                                                TWIG_LITERAL_STRING@271..286
                                                                  TK_SINGLE_QUOTES@271..272 "'"
                                                                  TWIG_LITERAL_STRING_INNER@272..285
                                                                    TK_WORD@272..282 "breakpoint"
                                                                    TK_DOT@282..283 "."
                                                                    TK_WORD@283..285 "xl"
                                                                  TK_SINGLE_QUOTES@285..286 "'"
                                                              TK_CLOSE_PARENTHESIS@286..287 ")"
                                                        TK_WHITESPACE@287..288 " "
                                                        TK_MINUS@288..289 "-"
                                                        TWIG_EXPRESSION@289..291
                                                          TWIG_LITERAL_NUMBER@289..291
                                                            TK_WHITESPACE@289..290 " "
                                                            TK_NUMBER@290..291 "1"
                                                    TK_CLOSE_PARENTHESIS@291..292 ")"
                                                TK_WHITESPACE@292..293 " "
                                                TK_FORWARD_SLASH@293..294 "/"
                                                TWIG_EXPRESSION@294..302
                                                  TWIG_LITERAL_NAME@294..302
                                                    TK_WHITESPACE@294..295 " "
                                                    TK_WORD@295..302 "columns"
                                            TK_CLOSE_PARENTHESIS@302..303 ")"
                                        TK_SINGLE_PIPE@303..304 "|"
                                        TWIG_OPERAND@304..320
                                          TWIG_LITERAL_NAME@304..309
                                            TK_WORD@304..309 "round"
                                          TWIG_ARGUMENTS@309..320
                                            TK_OPEN_PARENTHESIS@309..310 "("
                                            TWIG_EXPRESSION@310..311
                                              TWIG_LITERAL_NUMBER@310..311
                                                TK_NUMBER@310..311 "0"
                                            TK_COMMA@311..312 ","
                                            TWIG_EXPRESSION@312..319
                                              TWIG_LITERAL_STRING@312..319
                                                TK_WHITESPACE@312..313 " "
                                                TK_SINGLE_QUOTES@313..314 "'"
                                                TWIG_LITERAL_STRING_INNER@314..318
                                                  TK_WORD@314..318 "ceil"
                                                TK_SINGLE_QUOTES@318..319 "'"
                                            TK_CLOSE_PARENTHESIS@319..320 ")"
                                    TK_WHITESPACE@320..321 " "
                                    TK_TILDE@321..322 "~"
                                    TWIG_EXPRESSION@322..326
                                      TWIG_LITERAL_STRING@322..326
                                        TK_SINGLE_QUOTES@322..323 "'"
                                        TWIG_LITERAL_STRING_INNER@323..325
                                          TK_WORD@323..325 "px"
                                        TK_SINGLE_QUOTES@325..326 "'"
                            TK_LINE_BREAK@326..327 "\n"
                            TK_WHITESPACE@327..339 "            "
                            TK_CLOSE_CURLY@339..340 "}"
                      TK_WHITESPACE@340..341 " "
                      TK_PERCENT_CURLY@341..343 "%}""#]],
        );
    }
}

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
    /// None means it is not a suitable unary operator (has one operand)
    fn unary_binding_power(&self) -> Option<((), u8)>;
}

impl Operator for SyntaxKind {
    fn binary_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            // left associative
            T!["or"] => Some((5, 6)),
            T!["and"] => Some((10, 11)),
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
            | T!["not in"]
            | T!["in"]
            | T!["matches"]
            | T!["starts with"]
            | T!["ends with"] => Some((20, 21)),
            T![".."] => Some((25, 26)),
            T!["+"] | T!["-"] => Some((30, 31)),
            T!["*"] | T!["/"] | T!["%"] | T!["//"] => Some((60, 61)),
            T!["is"] | T!["is not"] => Some((100, 101)),
            // right associative
            T!["**"] => Some((121, 120)),
            T!["??"] => Some((151, 150)),

            _ => None,
        }
    }

    fn unary_binding_power(&self) -> Option<((), u8)> {
        match self {
            T!["not"] => Some(((), 200)),
            T!["+"] | T!["-"] => Some(((), 220)),
            _ => None,
        }
    }
}

fn parse_twig_expression_binding_power(
    parser: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    let mut lhs = parse_twig_expression_lhs(parser)?;

    while let Some((left_binding_power, right_binding_power)) = parser
        .peek_token()
        .and_then(|t| t.kind.binary_binding_power())
    {
        if left_binding_power < minimum_binding_power {
            break;
        }

        // Eat the operator’s token.
        parser.bump();

        // recurse
        let m = parser.precede(lhs);
        let parsed_rhs = parse_twig_expression_binding_power(parser, right_binding_power).is_some();
        lhs = parser.complete(m, SyntaxKind::TWIG_BINARY_EXPRESSION);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn parse_twig_expression_lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["("]) {
        Some(parse_paren_expression(parser))
    } else if parser.at_set(&[T!["-"], T!["+"], T!["not"]]) {
        Some(parse_unary_expression(parser))
    } else if let Some(literal) = parse_twig_literal(parser) {
        Some(literal)
    } else {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        None
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
    fn parse_twig_simple_math_expression() {
        check_parse(
            "{{ 1 + 2 * 3 }}",
            expect![[r#"
            ROOT@0..15
              TWIG_VAR@0..15
                TK_OPEN_CURLY_CURLY@0..2 "{{"
                TWIG_INFIX_EXPRESSION@2..12
                  TWIG_LITERAL_NUMBER@2..4
                    TK_WHITESPACE@2..3 " "
                    TK_NUMBER@3..4 "1"
                  TK_WHITESPACE@4..5 " "
                  TK_PLUS@5..6 "+"
                  TWIG_INFIX_EXPRESSION@6..12
                    TWIG_LITERAL_NUMBER@6..8
                      TK_WHITESPACE@6..7 " "
                      TK_NUMBER@7..8 "2"
                    TK_WHITESPACE@8..9 " "
                    TK_STAR@9..10 "*"
                    TWIG_LITERAL_NUMBER@10..12
                      TK_WHITESPACE@10..11 " "
                      TK_NUMBER@11..12 "3"
                TK_WHITESPACE@12..13 " "
                TK_CLOSE_CURLY_CURLY@13..15 "}}""#]],
        )
    }

    #[test]
    fn parse_twig_simple_comparison_expression() {
        check_parse("{{ a >= b + 1 }}", expect![[r#""#]])
    }

    #[test]
    fn parse_twig_simple_unary_expression() {
        check_parse("{{ not a }}", expect![[r#""#]])
    }
}

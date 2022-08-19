use super::IResult;
use crate::ast::*;
use crate::parser::general::Input;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace0};
use nom::combinator::map;
use nom::multi::many_till;
use nom::sequence::{preceded, terminated};

/// parses any {{ ... }} block. It could be a twig output expression or something like a vue print expression.
pub(crate) fn expression_block(input: Input) -> IResult<SyntaxNode> {
    preceded(
        terminated(tag("{{"), multispace0),
        map(
            many_till(anychar, preceded(multispace0, tag("}}"))),
            |(v, _)| {
                SyntaxNode::OutputExpression(OutputExpression {
                    content: v.into_iter().collect(),
                })
            },
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    #[test]
    fn test_some_vue_variable_print() {
        let res = expression_block("{{ $tc('swag-migration.index.confirmAbortDialog.hint') }}");

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::OutputExpression(OutputExpression {
                    content: "$tc('swag-migration.index.confirmAbortDialog.hint')".to_string()
                })
            ))
        )
    }

    #[test]
    fn test_some_vue_variable_print_with_complex_logic() {
        let res = expression_block(
            "{{   if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }       }}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                SyntaxNode::OutputExpression(OutputExpression {
                    content: "if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }".to_string()
                })
            ))
        )
    }

    #[test]
    fn test_some_vue_print_with_no_whitespace_and_more_content() {
        let res = expression_block(
            "{{currentOrder.transactions.last().paymentMethod.translated.name}}{{ counter }}",
        );

        assert_eq!(
            res,
            Ok((
                "{{ counter }}",
                SyntaxNode::OutputExpression(OutputExpression {
                    content: "currentOrder.transactions.last().paymentMethod.translated.name"
                        .to_string()
                })
            ))
        )
    }
}

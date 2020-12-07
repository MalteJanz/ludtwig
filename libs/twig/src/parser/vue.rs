use super::IResult;
use crate::ast::*;
use crate::parser::general::Input;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace0};
use nom::combinator::map;
use nom::multi::many_till;
use nom::sequence::{preceded, terminated};

pub(crate) fn vue_block(input: Input) -> IResult<HtmlNode> {
    preceded(
        terminated(tag("{{"), multispace0),
        map(
            many_till(anychar, preceded(multispace0, tag("}}"))),
            |(v, _)| {
                HtmlNode::VueBlock(VueBlock {
                    content: v.into_iter().collect(),
                })
            },
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_some_vue_variable_print() {
        let res = vue_block("{{ $tc('swag-migration.index.confirmAbortDialog.hint') }}");

        assert_eq!(
            res,
            Ok((
                "",
                HtmlNode::VueBlock(VueBlock {
                    content: "$tc('swag-migration.index.confirmAbortDialog.hint')".to_string()
                })
            ))
        )
    }

    #[test]
    fn test_some_vue_variable_print_with_complex_logic() {
        let res = vue_block(
            "{{   if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }       }}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                HtmlNode::VueBlock(VueBlock {
                    content: "if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }".to_string()
                })
            ))
        )
    }

    #[test]
    fn test_some_vue_print_with_no_whitespace_and_more_content() {
        let res = vue_block(
            "{{currentOrder.transactions.last().paymentMethod.translated.name}}{{ counter }}",
        );

        assert_eq!(
            res,
            Ok((
                "{{ counter }}",
                HtmlNode::VueBlock(VueBlock {
                    content: "currentOrder.transactions.last().paymentMethod.translated.name"
                        .to_string()
                })
            ))
        )
    }
}

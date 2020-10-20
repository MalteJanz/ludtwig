use super::IResult;
use crate::ast::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::character::complete::multispace0;
use nom::combinator::map;
use nom::sequence::{delimited, preceded};

pub(crate) fn vue_block(input: &str) -> IResult<HtmlNode> {
    delimited(
        tag("{{"),
        preceded(
            multispace0,
            map(
                alt((take_until(" }}"), take_until("}}"))),
                |content: &str| {
                    HtmlNode::VueBlock(VueBlock {
                        content: content.to_owned(),
                    })
                },
            ),
        ),
        alt((tag(" }}"), tag("}}"))),
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
            "{{   if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }    }}",
        );

        assert_eq!(
            res,
            Ok((
                "",
                HtmlNode::VueBlock(VueBlock {
                    content: "if a { $tc('swag-migration.index.confirmAbortDialog.hint' ) } else {  $tc('nothing' ); }   ".to_string()
                })
            ))
        )
    }
}

use crate::ast::*;
use crate::parser::html::{html_complete_tag, html_plain_text};
use crate::parser::twig::twig_complete_block;
use crate::parser::vue::vue_block;
use crate::TwigParseError;
use nom::branch::alt;
use nom::multi::many1;
use std::collections::HashMap;

pub(crate) type IResult<'a, O> = nom::IResult<&'a str, O, TwigParseError>;

pub(crate) fn document_node(input: &str) -> IResult<HtmlNode> {
    alt((
        twig_complete_block,
        html_complete_tag,
        vue_block,
        html_plain_text,
    ))(input)
}

pub(crate) fn document_node_all(input: &str) -> IResult<HtmlNode> {
    let (remaining, children) = many1(document_node)(&input)?;

    Ok((
        remaining,
        HtmlNode::Tag(HtmlTag {
            name: "ROOT",
            self_closed: false,
            arguments: HashMap::new(),
            children,
        }),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_some_vue_template() {
        let res = document_node(
            "<sw-button-group
                v-if=\"startButtonVisible\"
                :splitButton=\"true\">

            <sw-button variant=\"primary\"
                       :disabled=\"startButtonDisabled\"
                       @click=\"onStartButtonClick\">
                {{ $tc('swag-migration.index.confirmAbortDialog.hint') }}
            </sw-button>

            <sw-context-button :disabled=\"isLoading\">
                <template slot=\"button\">

                    <sw-button square
                               variant=\"primary\"
                               :disabled=\"isLoading\">
                        <sw-icon name=\"small-arrow-medium-down\" size=\"16\"></sw-icon>
                    </sw-button>
                </template>

                <sw-context-menu-item @click=\"onSaveButtonClick\"
                                      :disabled=\"isLoading\">
                    {{ $tc('swag-migration.index.confirmAbortDialog.hint') }}
                </sw-context-menu-item>
            </sw-context-button>

        </sw-button-group>",
        );

        assert!(res.is_ok());
    }
}

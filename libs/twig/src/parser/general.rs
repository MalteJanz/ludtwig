use crate::ast::*;
use crate::error::{DynamicParseError, TwigParsingErrorInformation};
use crate::parser::html::{html_complete_tag, html_plain_text};
use crate::parser::twig::twig_complete_block;
use crate::parser::vue::vue_block;
use nom::branch::alt;
use nom::lib::std::collections::BTreeMap;
use nom::multi::many1;

pub(crate) type IResult<'a, O> = nom::IResult<&'a str, O, TwigParsingErrorInformation<&'a str>>;

/// create a new error from an input position, a DYNAMIC string and an existing error.
/// This is used mainly in the [dynamic_context] combinator, to add user friendly information
/// to errors when backtracking through a parse tree
pub(crate) fn dynamic_context<I: Clone, E: DynamicParseError<I>, F, O>(
    context: String,
    f: F,
) -> impl Fn(I) -> nom::IResult<I, O, E>
where
    F: Fn(I) -> nom::IResult<I, O, E>,
{
    move |i: I| match f(i.clone()) {
        Ok(o) => Ok(o),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(E::add_dynamic_context(
            i,
            context.clone(),
            e,
        ))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(E::add_dynamic_context(
            i,
            context.clone(),
            e,
        ))),
    }
}

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
            attributes: BTreeMap::new(),
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

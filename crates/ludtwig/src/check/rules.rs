mod html_attribute_name_kebab_case;
mod indentation;
mod line_ending;
mod twig_block_name_snake_case;

use crate::check::rule::Rule;
use crate::check::rules::html_attribute_name_kebab_case::RuleHtmlAttributeNameKebabCase;
use crate::check::rules::indentation::RuleIndentation;
use crate::check::rules::line_ending::RuleLineEnding;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;

pub static RULES: &[&(dyn Rule + Sync)] = &[
    &RuleTwigBlockNameSnakeCase,
    &RuleHtmlAttributeNameKebabCase,
    &RuleLineEnding,
    &RuleIndentation,
];

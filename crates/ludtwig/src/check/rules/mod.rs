mod html_attribute_name_kebab_case;
mod twig_block_name_snake_case;

use crate::check::rule::Rule;
use crate::check::rules::html_attribute_name_kebab_case::HtmlAttributeNameKebabCase;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;

pub static RULES: &[&(dyn Rule + Sync)] =
    &[&RuleTwigBlockNameSnakeCase, &HtmlAttributeNameKebabCase];

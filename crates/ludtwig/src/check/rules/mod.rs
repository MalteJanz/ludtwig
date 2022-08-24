use crate::check::rule::Rule;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;

mod twig_block_name_snake_case;

pub fn get_rules() -> Vec<Box<dyn Rule>> {
    vec![Box::new(RuleTwigBlockNameSnakeCase)]
}

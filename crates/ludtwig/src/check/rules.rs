mod html_attribute_name_kebab_case;
mod indentation;
mod line_ending;
mod twig_block_name_snake_case;

use crate::check::rule::{Rule, Severity};
use crate::check::rules::html_attribute_name_kebab_case::RuleHtmlAttributeNameKebabCase;
use crate::check::rules::indentation::RuleIndentation;
use crate::check::rules::line_ending::RuleLineEnding;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;
use crate::{CliContext, ProcessingEvent};
use std::io;
use std::io::Write;

/// List of all rule trait objects, also add them to the `active-rules` in `ludtwig-config.toml`!
pub static RULES: &[&(dyn Rule + Sync)] = &[
    &RuleTwigBlockNameSnakeCase,
    &RuleHtmlAttributeNameKebabCase,
    &RuleLineEnding,
    &RuleIndentation,
];

pub fn get_active_rules(
    config_active_rules: &[String],
    cli_context: &CliContext,
) -> Vec<&'static (dyn Rule + Sync)> {
    // gather active rules
    let config_active_rules: Vec<&str> = config_active_rules.iter().map(String::as_ref).collect();
    let active_rules: Vec<&(dyn Rule + Sync)> = RULES
        .iter()
        .filter_map(|r| {
            if config_active_rules.contains(&r.name()) {
                Some(*r)
            } else {
                None
            }
        })
        .collect();
    // validate that every rule in the config is there
    for config_rule in &config_active_rules {
        let mut found = false;
        for rule in &active_rules {
            if &rule.name() == config_rule {
                found = true;
                break;
            }
        }

        if !found {
            io::stderr()
                .write_all(format!("Can't find active rule {}\n", config_rule).as_bytes())
                .unwrap();
            cli_context.send_processing_output(ProcessingEvent::Report(Severity::Error));
        }
    }
    active_rules
}

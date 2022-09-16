use std::io;
use std::io::Write;

use crate::check::rule::{Rule, Severity};
use crate::check::rules::html_attribute_name_kebab_case::RuleHtmlAttributeNameKebabCase;
use crate::check::rules::indentation::RuleIndentation;
use crate::check::rules::line_ending::RuleLineEnding;
use crate::check::rules::twig_block_line_breaks::RuleTwigBlockLineBreaks;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;
use crate::check::rules::whitespace_between_line_breaks::RuleWhitespaceBetweenLineBreaks;
use crate::{CliContext, ProcessingEvent};

mod html_attribute_name_kebab_case;
mod indentation;
mod line_ending;
mod twig_block_line_breaks;
mod twig_block_name_snake_case;
mod whitespace_between_line_breaks;

/// List of all rule trait objects, also add them to the `active-rules` in `ludtwig-config.toml`!
pub static RULES: &[&(dyn Rule + Sync)] = &[
    &RuleWhitespaceBetweenLineBreaks,
    &RuleLineEnding,
    &RuleIndentation,
    &RuleTwigBlockLineBreaks,
    &RuleTwigBlockNameSnakeCase,
    &RuleHtmlAttributeNameKebabCase,
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

#[cfg(test)]
pub mod test {
    use std::path::PathBuf;
    use std::sync::mpsc::Receiver;
    use std::sync::{mpsc, Arc};

    use codespan_reporting::term::termcolor::Buffer;

    use ludtwig_parser::parse;
    use ludtwig_parser::syntax::untyped::SyntaxNode;

    use crate::check::produce_diagnostics;
    use crate::check::rule::{Rule, RuleContext};
    use crate::check::rules::RULES;
    use crate::check::run_rules;
    use crate::process::{iteratively_apply_suggestions, FileContext};
    use crate::{CliContext, Config, ProcessingEvent};

    fn debug_rule(
        rule_name: &str,
        source_code: &str,
    ) -> (
        FileContext,
        RuleContext,
        Vec<&'static (dyn Rule + Sync)>,
        Receiver<ProcessingEvent>,
    ) {
        let rule = *RULES.iter().find(|r| r.name() == rule_name).unwrap();
        let (tx, rx) = mpsc::sync_channel(256);
        let parse = parse(source_code);

        let file_context = FileContext {
            cli_context: Arc::new(CliContext {
                output_tx: tx,
                fix: false,
                inspect: false,
                output_path: None,
                config: Config::new(crate::config::DEFAULT_CONFIG_PATH).unwrap(),
            }),
            file_path: PathBuf::from("./debug-rule.html.twig"),
            tree_root: SyntaxNode::new_root(parse.green_node),
            source_code: source_code.to_owned(),
            parse_errors: parse.errors,
        };

        let active_rules = vec![rule];

        let rule_result_context = run_rules(&active_rules, &file_context);

        (file_context, rule_result_context, active_rules, rx)
    }

    pub fn test_rule(rule_name: &str, source_code: &str, expected_report: expect_test::Expect) {
        let (file_context, rule_result_context, _, rx) = debug_rule(rule_name, source_code);
        let mut buffer = Buffer::no_color();
        produce_diagnostics(&file_context, rule_result_context, &mut buffer);
        expected_report.assert_eq(&String::from_utf8_lossy(buffer.as_slice()));
        drop(rx);
    }

    pub fn test_rule_fix(
        rule_name: &str,
        source_code: &str,
        expected_source_code: expect_test::Expect,
    ) {
        let (file_context, rule_result_context, active_rules, rx) =
            debug_rule(rule_name, source_code);
        let (file_context, _, dirty, iteration) =
            iteratively_apply_suggestions(&active_rules, file_context, rule_result_context);

        expected_source_code.assert_eq(&file_context.source_code);
        assert!(dirty);
        assert_eq!(
            iteration, 1,
            "fixing a single rule should happen in one iteration!"
        );
        drop(rx);
    }
}

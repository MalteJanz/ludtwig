use crate::check::rule::Rule;
use crate::check::rules::html_attribute_name_kebab_case::RuleHtmlAttributeNameKebabCase;
use crate::check::rules::indentation::RuleIndentation;
use crate::check::rules::line_ending::RuleLineEnding;
use crate::check::rules::ludtwig_ignore_file_not_on_top::RuleLudtwigIgnoreFileNotOnTop;
use crate::check::rules::twig_block_line_breaks::RuleTwigBlockLineBreaks;
use crate::check::rules::twig_block_name_snake_case::RuleTwigBlockNameSnakeCase;
use crate::check::rules::twig_logic_and::RuleTwigLogicAnd;
use crate::check::rules::twig_logic_or::RuleTwigLogicOr;
use crate::check::rules::twig_string_quotation::RuleTwigStringQuotation;
use crate::check::rules::unknown_token::RuleUnknownToken;
use crate::check::rules::whitespace_between_line_breaks::RuleWhitespaceBetweenLineBreaks;
use crate::error::ConfigurationError;
use crate::Config;
use ludtwig_parser::syntax::typed::{AstNode, LudtwigDirectiveFileIgnore};
use ludtwig_parser::syntax::untyped::SyntaxNode;

mod html_attribute_name_kebab_case;
mod indentation;
mod line_ending;
mod ludtwig_ignore_file_not_on_top;
mod twig_block_line_breaks;
mod twig_block_name_snake_case;
mod twig_logic_and;
mod twig_logic_or;
mod twig_string_quotation;
mod unknown_token;
mod whitespace_between_line_breaks;

/// List of all rule trait objects, also add them to the `active-rules` in `ludtwig-config.toml`!
pub static RULE_DEFINITIONS: &[&'static dyn Rule] = &[
    &RuleLudtwigIgnoreFileNotOnTop,
    &RuleUnknownToken,
    &RuleWhitespaceBetweenLineBreaks,
    &RuleLineEnding,
    &RuleIndentation,
    &RuleTwigBlockLineBreaks,
    &RuleTwigBlockNameSnakeCase,
    &RuleHtmlAttributeNameKebabCase,
    &RuleTwigLogicAnd,
    &RuleTwigLogicOr,
    &RuleTwigStringQuotation,
];

/// Get active rule definitions based on config
pub fn get_config_active_rule_definitions(
    config: &Config,
) -> Result<Vec<&'static dyn Rule>, ConfigurationError> {
    // gather active rules
    let config_active_rules: Vec<&str> = config
        .general
        .active_rules
        .iter()
        .map(String::as_ref)
        .collect();
    let active_rules: Vec<&'static dyn Rule> = RULE_DEFINITIONS
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
            return Err(ConfigurationError::RuleNotFound {
                name: config_rule.to_string(),
            });
        }
    }

    Ok(active_rules)
}

/// filter down config active rule definitions for a specific file
/// after looking inside it for ludtwig-ignore-file directives
pub fn get_file_active_rule_definitions(
    root: &SyntaxNode,
    definitions: &[&'static dyn Rule],
) -> Vec<&'static dyn Rule> {
    let mut disabled_rules: Vec<String> = vec![];

    for directive in root
        .children()
        // .take(3) // Todo: maybe only look at x first children of root?
        .filter_map(LudtwigDirectiveFileIgnore::cast)
    {
        let mut rules = directive.get_rules();
        if rules.is_empty() {
            // no rule to disable specified, so ignore all rules on this file (run nothing)
            return vec![];
        } else {
            disabled_rules.append(&mut rules);
        }
    }

    definitions
        .iter()
        .filter(|d| !disabled_rules.iter().any(|rule_name| rule_name == d.name()))
        .cloned()
        .collect()
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
    use crate::check::rule::RuleContext;
    use crate::check::rules::RULE_DEFINITIONS;
    use crate::check::run_rules;
    use crate::process::{iteratively_apply_suggestions, FileContext};
    use crate::{CliContext, CliSharedData, Config, ProcessingEvent};

    fn debug_rule(
        rule_name: &str,
        source_code: &str,
    ) -> (FileContext, RuleContext, Receiver<ProcessingEvent>) {
        let config = Config::new(crate::config::DEFAULT_CONFIG_PATH).unwrap();

        let rule = RULE_DEFINITIONS
            .iter()
            .find(|r| r.name() == rule_name)
            .unwrap();
        let (tx, rx) = mpsc::channel();
        let parse = parse(source_code);

        let file_context = FileContext {
            cli_context: CliContext {
                output_tx: tx,
                data: Arc::new(CliSharedData {
                    fix: false,
                    inspect: false,
                    config,
                    rule_definitions: vec![*rule],
                }),
            },
            file_path: PathBuf::from("./debug-rule.html.twig"),
            tree_root: SyntaxNode::new_root(parse.green_node),
            source_code: source_code.to_owned(),
            parse_errors: parse.errors,
            file_rule_definitions: vec![*rule],
        };

        let rule_result_context = run_rules(&file_context);

        (file_context, rule_result_context, rx)
    }

    pub fn test_rule(rule_name: &str, source_code: &str, expected_report: expect_test::Expect) {
        let (file_context, rule_result_context, rx) = debug_rule(rule_name, source_code);
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
        let (file_context, rule_result_context, rx) = debug_rule(rule_name, source_code);
        let (file_context, _, dirty, iteration) =
            iteratively_apply_suggestions(file_context, rule_result_context).unwrap();

        expected_source_code.assert_eq(&file_context.source_code);
        assert!(dirty);
        assert_eq!(
            iteration, 1,
            "fixing a single rule should happen in one iteration!"
        );
        drop(rx);
    }

    pub fn test_rule_does_not_fix(
        rule_name: &str,
        source_code: &str,
        expected_source_code: expect_test::Expect,
    ) {
        let (file_context, rule_result_context, rx) = debug_rule(rule_name, source_code);
        let (file_context, _, dirty, iteration) =
            iteratively_apply_suggestions(file_context, rule_result_context).unwrap();

        expected_source_code.assert_eq(&file_context.source_code);
        assert!(!dirty);
        assert_eq!(iteration, 0, "No fixing should have no extra iterations!");
        drop(rx);
    }
}

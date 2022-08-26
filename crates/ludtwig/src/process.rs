use crate::check::rule::{CheckSuggestion, RuleContext, Severity};
use crate::check::{get_cli_outputs_from_rule_results, get_rule_context_suggestions, run_rules};
use crate::output::{CliOutput, CliOutputMessage};
use crate::CliContext;
use ludtwig_parser::syntax::typed::{AstNode, Root};
use ludtwig_parser::syntax::untyped::SyntaxNode;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

/// The context for a single file.
#[derive(Debug)]
pub struct FileContext {
    pub cli_context: Arc<CliContext>,

    /// The file path that is associated with this context
    pub file_path: PathBuf,

    /// The parsed [SyntaxNode] AST for this file / context.
    pub tree_root: SyntaxNode,

    pub source_code: String,
}

impl FileContext {
    /// Helper function to send some [Output] to the user for this specific file.
    pub fn send_output(&self, output: CliOutput) {
        self.cli_context.send_output(CliOutputMessage {
            file: self.file_path.clone(),
            output,
        });
    }
}

/// Process a single file with it's filepath.
pub fn process_file(path: PathBuf, cli_context: Arc<CliContext>) {
    // notify the output about this file (to increase the processed file counter)
    cli_context.send_output(CliOutputMessage {
        file: path.clone(),
        output: CliOutput {
            severity: Severity::Info,
            message: "".to_string(),
        },
    });

    let file_content = match fs::read_to_string(&path) {
        Ok(f) => f,
        Err(_) => {
            cli_context.send_output(CliOutputMessage {
                file: path,
                output: CliOutput {
                    severity: Severity::Error,
                    message: "Can't read file".to_string(),
                },
            });

            return;
        }
    };

    run_analysis(path, file_content, cli_context);
}

fn run_analysis(path: PathBuf, original_file_content: String, cli_context: Arc<CliContext>) {
    let tree_root = ludtwig_parser::parse(&original_file_content);
    let apply_suggestions = cli_context.fix;
    let file_context = FileContext {
        cli_context,
        file_path: path,
        source_code: tree_root.text().to_string(),
        tree_root,
    };

    // run all the rules
    let rule_result_context = run_rules(&file_context);

    // apply suggestions if needed
    let (file_context, rule_result_context) = if apply_suggestions {
        iteratively_apply_suggestions(file_context, rule_result_context)
    } else {
        (file_context, rule_result_context)
    };

    // send cli outputs over to output thread
    let cli_outputs = get_cli_outputs_from_rule_results(&file_context, rule_result_context);
    for out in cli_outputs {
        file_context.send_output(out);
    }
}

fn iteratively_apply_suggestions(
    file_context: FileContext,
    rule_result_context: RuleContext,
) -> (FileContext, RuleContext) {
    let mut current_results = (file_context, rule_result_context);

    // try at maximum 10 parsing iterations
    for _ in 0..10 {
        // get all the suggestions
        let mut suggestions = get_rule_context_suggestions(&current_results.1);
        println!("found suggestions: {:#?}", suggestions);

        if suggestions.is_empty() {
            return current_results;
        }

        // sort by syntax range
        suggestions
            .sort_by(|(_, sug_a), (_, sug_b)| sug_a.syntax_range.ordering(sug_b.syntax_range));

        // filter out overlapping suggestions
        let mut overlapping_rules = vec![];
        suggestions.iter().zip(suggestions.iter().skip(1)).for_each(
            |((rule_a, sug_a), (rule_b, sug_b))| {
                if sug_a.syntax_range.ordering(sug_b.syntax_range).is_eq() {
                    overlapping_rules.push(*rule_b);
                }
            },
        );
        let suggestions = suggestions
            .into_iter()
            .filter_map(|(rule, suggestion)| {
                if overlapping_rules.contains(&rule) {
                    return None;
                }

                Some(suggestion)
            })
            .collect();
        println!("apply suggestions: {:#?}", suggestions);

        // transform source code according to non overlapping suggestions
        let source_code = apply_suggestions_to_text(suggestions, current_results.0.source_code);

        // Parse the new source code again
        let tree_root = ludtwig_parser::parse(&source_code);
        let source_code = tree_root.text().to_string();

        let file_context = FileContext {
            source_code,
            tree_root,
            ..current_results.0
        };

        // Run all rules again
        let rule_result_context = run_rules(&file_context);
        current_results = (file_context, rule_result_context);
    }

    current_results
}

fn apply_suggestions_to_text(
    suggestions: Vec<&CheckSuggestion>,
    mut source_code: String,
) -> String {
    suggestions.into_iter().rev().for_each(|suggestion| {
        let start: usize = suggestion.syntax_range.start().into();
        let end: usize = suggestion.syntax_range.end().into();

        source_code.replace_range(start..end, &suggestion.replace_with);
    });

    source_code
}

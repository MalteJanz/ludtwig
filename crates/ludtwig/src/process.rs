use std::collections::HashSet;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::sync::Arc;

use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice};

use ludtwig_parser::syntax::untyped::SyntaxNode;
use ludtwig_parser::ParseError;

use crate::check::rule::{CheckSuggestion, Rule, RuleContext, Severity};
use crate::check::rules::get_active_rules;
use crate::check::{get_rule_context_suggestions, produce_diagnostics, run_rules};
use crate::output::ProcessingEvent;
use crate::CliContext;

/// The context for a single file.
#[derive(Debug)]
pub struct FileContext {
    pub cli_context: Arc<CliContext>,

    /// The file path that is associated with this context
    pub file_path: PathBuf,

    /// The parsed [SyntaxNode] AST for this file / context.
    pub tree_root: SyntaxNode,

    pub source_code: String,

    pub parse_errors: Vec<ParseError>,
}

impl FileContext {
    pub fn send_processing_output(&self, event: ProcessingEvent) {
        self.cli_context.send_processing_output(event);
    }
}

/// Process a single file with it's filepath.
pub fn process_file(path: PathBuf, cli_context: Arc<CliContext>) {
    // notify the output about this file (to increase the processed file counter)
    cli_context.send_processing_output(ProcessingEvent::FileProcessed);

    let file_content = match fs::read_to_string(&path) {
        Ok(f) => f,
        Err(_) => {
            io::stderr()
                .write_all(format!("Can't read file {:?}", path).as_bytes())
                .unwrap();
            cli_context.send_processing_output(ProcessingEvent::Report(Severity::Error));

            return;
        }
    };

    run_analysis(path, file_content, cli_context);
}

fn run_analysis(path: PathBuf, original_file_content: String, cli_context: Arc<CliContext>) {
    let parse = ludtwig_parser::parse(&original_file_content);
    // TODO: also use the parse errors!
    let root = SyntaxNode::new_root(parse.green_node);

    let apply_suggestions = cli_context.fix;
    let file_context = FileContext {
        cli_context,
        file_path: path,
        source_code: root.text().to_string(),
        tree_root: root,
        parse_errors: parse.errors,
    };

    // get active rules
    let active_rules = get_active_rules(
        &file_context.cli_context.config.general.active_rules,
        &file_context.cli_context,
    );

    // run all the rules
    let rule_result_context = run_rules(&active_rules, &file_context);

    // apply suggestions if needed
    let (file_context, rule_result_context) = if apply_suggestions {
        let (file_context, rule_result_context, dirty) =
            iteratively_apply_suggestions(&active_rules, file_context, rule_result_context);
        if dirty {
            // Todo: better error handling -> maybe other files can be written
            fs::write(&file_context.file_path, &file_context.source_code)
                .expect("Can't write back into file");
            println!("fixed {:?}", &file_context.file_path);
        }

        (file_context, rule_result_context)
    } else {
        (file_context, rule_result_context)
    };

    // send processing events for rule check results + parser errors and output them to the terminal
    let writer = BufferWriter::stderr(ColorChoice::Always);
    let mut buffer = writer.buffer();
    produce_diagnostics(&file_context, rule_result_context, &mut buffer);
    writer.print(&buffer).unwrap();
}

pub fn iteratively_apply_suggestions(
    active_rules: &Vec<&(dyn Rule + Sync)>,
    file_context: FileContext,
    rule_result_context: RuleContext,
) -> (FileContext, RuleContext, bool) {
    let mut current_results = (file_context, rule_result_context, false);

    // try at maximum 10 parsing iterations
    for _ in 0..10 {
        // get all the suggestions
        let mut suggestions = get_rule_context_suggestions(&current_results.1);
        if suggestions.is_empty() {
            break;
        }

        // sort by syntax range
        suggestions
            .sort_by(|(_, sug_a), (_, sug_b)| sug_a.syntax_range.ordering(sug_b.syntax_range));

        // filter out overlapping suggestions
        let mut overlapping_rules = HashSet::new();
        suggestions.iter().zip(suggestions.iter().skip(1)).for_each(
            |((rule_a, sug_a), (rule_b, sug_b))| {
                if sug_a.syntax_range.ordering(sug_b.syntax_range).is_eq() {
                    if rule_a == rule_b {
                        // TODO: better error handling -> all other file threads can continue?
                        panic!("Suggestion collision inside the same rule, check rule {} or write bug report", rule_a);
                    }

                    overlapping_rules.insert(*rule_b);
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

        // transform source code according to non overlapping suggestions
        current_results.2 = true; // set dirty flag
        let source_code = apply_suggestions_to_text(suggestions, current_results.0.source_code);

        // Parse the new source code again
        let new_parse = ludtwig_parser::parse(&source_code);
        let tree_root = SyntaxNode::new_root(new_parse.green_node);
        let source_code = tree_root.text().to_string();

        let file_context = FileContext {
            source_code,
            tree_root,
            parse_errors: new_parse.errors,
            ..current_results.0
        };

        // Run all rules again
        let rule_result_context = run_rules(active_rules, &file_context);
        current_results = (file_context, rule_result_context, current_results.2);
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

pub mod rule;
mod rules;

use crate::check::rule::{CheckSuggestion, RuleContext, Severity};
use crate::check::rules::get_active_rules;
use crate::process::FileContext;
use crate::ProcessingEvent;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice};
use ludtwig_parser::syntax::typed;
use ludtwig_parser::syntax::typed::AstNode;
use ludtwig_parser::syntax::untyped::{SyntaxElement, WalkEvent};
use std::borrow::Borrow;
use std::sync::Arc;

pub fn run_rules(file_context: &FileContext) -> RuleContext {
    let mut ctx = RuleContext {
        check_results: vec![],
        cli_context: Arc::clone(&file_context.cli_context),
    };

    let active_rules = get_active_rules(&ctx.config().general.active_rules, &ctx.cli_context);

    // run root node checks once for each rule
    for rule in &active_rules {
        rule.check_root(file_context.tree_root.clone(), &mut ctx);
    }

    // iterate through syntax tree
    let mut preorder = file_context.tree_root.preorder_with_tokens();
    while let Some(walk_event) = preorder.next() {
        if let WalkEvent::Enter(element) = walk_event {
            match element {
                SyntaxElement::Node(n) => {
                    if typed::Error::can_cast(n.kind()) {
                        preorder.skip_subtree();
                        continue; // Skip error nodes in rules for now
                                  // TODO: maybe also pass errors to specific rules to generate CLI output?
                    }

                    // run node checks for every rule
                    for rule in &active_rules {
                        rule.check_node(n.clone(), &mut ctx);
                    }
                }
                SyntaxElement::Token(t) => {
                    // run token checks for every rule
                    for rule in &active_rules {
                        rule.check_token(t.clone(), &mut ctx);
                    }
                }
            }
        }
    }

    ctx
}

pub fn get_rule_context_suggestions(rule_ctx: &RuleContext) -> Vec<(&str, &CheckSuggestion)> {
    rule_ctx
        .check_results
        .iter()
        .flat_map(|res| {
            let rule_name = res.rule_name.borrow();
            res.suggestions.iter().map(move |sug| (rule_name, sug))
        })
        .collect()
}

pub fn produce_diagnostics(file_context: &FileContext, result_rule_ctx: RuleContext) {
    // diagnostic output setup
    let mut files = SimpleFiles::new();
    let file_id = files.add(
        file_context.file_path.to_str().unwrap(),
        &file_context.source_code,
    );
    let writer = BufferWriter::stderr(ColorChoice::Always);
    let mut buffer = writer.buffer();
    let config = term::Config {
        // styles: Styles::with_blue(term::termcolor::Color::Cyan),
        ..Default::default()
    };

    // run through the parser errors
    for result in &file_context.parse_errors {
        // notify output about this
        file_context.send_processing_output(ProcessingEvent::Report(Severity::Error));
        let label =
            Label::primary(file_id, result.range).with_message(result.expected_message().unwrap());
        let diagnostic = Diagnostic::error()
            .with_code("SyntaxError")
            .with_message("The parser encountered a syntax error")
            .with_labels(vec![label]);

        term::emit(&mut buffer, &config, &files, &diagnostic).unwrap();
    }

    // run through the rule check results
    for result in result_rule_ctx.check_results {
        let diagnostic = match result.severity {
            Severity::Error => Diagnostic::error(),
            Severity::Warning => Diagnostic::warning(),
            Severity::Info => Diagnostic::note(),
        };

        // notify output about this
        file_context.send_processing_output(ProcessingEvent::Report(result.severity));

        let mut labels = vec![];
        if let Some(primary) = result.primary {
            labels
                .push(Label::primary(file_id, primary.syntax_range).with_message(primary.message));
        }

        for secondary in result.secondary {
            labels.push(
                Label::secondary(file_id, secondary.syntax_range).with_message(secondary.message),
            )
        }

        for suggestion in result.suggestions {
            labels.push(
                Label::secondary(file_id, suggestion.syntax_range).with_message(format!(
                    "{}: {}",
                    suggestion.message, suggestion.replace_with
                )),
            )
        }

        let diagnostic = diagnostic
            .with_code(result.rule_name)
            .with_message(result.message)
            .with_labels(labels);

        term::emit(&mut buffer, &config, &files, &diagnostic).unwrap();
    }

    // write the diagnostics to console
    writer.print(&buffer).unwrap();
}

pub mod rule;
mod rules;

use crate::check::rule::RuleContext;
use crate::check::rules::RULES;
use crate::output::CliOutput;
use crate::process::FileContext;
use ansi_term::Color;
use ludtwig_parser::syntax::typed;
use ludtwig_parser::syntax::typed::AstNode;
use ludtwig_parser::syntax::untyped::{SyntaxElement, WalkEvent};
use std::fmt::Write;

pub fn run_rules(file_context: &FileContext) -> RuleContext {
    let mut ctx = RuleContext {
        check_results: vec![],
    };

    let all_rules = RULES;

    // run root node checks once for each rule
    for rule in all_rules {
        rule.check_root(file_context.tree_root.syntax().to_owned(), &mut ctx);
    }

    // iterate through syntax tree
    let mut preorder = file_context.tree_root.syntax().preorder_with_tokens();
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
                    for rule in all_rules {
                        rule.check_node(n.clone(), &mut ctx);
                    }
                }
                SyntaxElement::Token(t) => {
                    // run token checks for every rule
                    for rule in all_rules {
                        rule.check_token(t.clone(), &mut ctx);
                    }
                }
            }
        }
    }

    ctx
}

pub fn get_cli_outputs_from_rule_results(
    file_context: &FileContext,
    result_rule_ctx: RuleContext,
) -> Vec<CliOutput> {
    let mut outputs = vec![];

    for result in result_rule_ctx.check_results {
        let mut s = String::new();
        let _ = write!(
            s,
            "{}",
            Color::Red.paint(format!("Error[{}]", result.rule_name))
        );
        let _ = writeln!(s, ": {}", result.message);
        let _ = writeln!(s, "    {:?}", file_context.file_path.as_os_str());

        if let Some(note) = result.primary {
            let _ = writeln!(s, "          {:?}", note.syntax_range);
            let _ = writeln!(
                s,
                "          {} <- {}",
                &file_context.source_code
                    [usize::from(note.syntax_range.start())..usize::from(note.syntax_range.end())],
                note.message
            );
        }

        // TODO: line and column numbers with view into file
        // TODO: There is a nice crate for all of this called codespan-reporting

        outputs.push(CliOutput {
            severity: result.severity,
            message: s,
        });
    }

    outputs
}

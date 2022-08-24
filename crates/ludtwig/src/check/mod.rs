use crate::check::rule::{CheckResult, RuleContext};
use crate::check::rules::get_rules;
use crate::output::CliOutput;
use crate::process::FileContext;
use ansi_term::Color;
use ludtwig_parser::syntax::typed::AstNode;
use ludtwig_parser::syntax::untyped::{SyntaxElement, WalkEvent};
use std::fmt::Write;
use std::path::PathBuf;

pub mod rule;
mod rules;

pub fn run_rules(file_context: &FileContext) -> RuleContext {
    let mut ctx = RuleContext {
        check_results: vec![],
    };

    let all_rules = get_rules();

    // run root node checks once for each rule
    for rule in &all_rules {
        rule.check_root(file_context.tree_root.syntax().to_owned(), &mut ctx);
    }

    // iterate through syntax tree
    for walk_event in file_context.tree_root.syntax().preorder_with_tokens() {
        if let WalkEvent::Enter(element) = walk_event {
            match element {
                SyntaxElement::Node(n) => {
                    // run node checks for every rule
                    for rule in &all_rules {
                        rule.check_node(n.clone(), &mut ctx);
                    }
                }
                SyntaxElement::Token(t) => {
                    // run token checks for every rule
                    for rule in &all_rules {
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
                "          {}",
                &file_context.source_code
                    [usize::from(note.syntax_range.start())..usize::from(note.syntax_range.end())]
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

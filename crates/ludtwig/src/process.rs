use crate::check::rule::Severity;
use crate::check::{get_cli_outputs_from_rule_results, run_rules};
use crate::output::{CliOutput, CliOutputMessage};
use crate::CliContext;
use ludtwig_parser::syntax::typed::Root;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

/// The context for a single file.
#[derive(Debug)]
pub struct FileContext {
    pub cli_context: Arc<CliContext>,

    /// The file path that is associated with this context
    pub file_path: PathBuf,

    // The parsed [SyntaxNode] AST for this file / context.
    pub tree_root: Root,

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

    let tree_root = ludtwig_parser::parse(&file_content);

    let file_context = Arc::new(FileContext {
        cli_context,
        file_path: path,
        tree_root,
        source_code: file_content,
    });

    work_with_file_context(file_context);
}

/// Do all the analyzing and writing work concurrently on the [FileContext].
fn work_with_file_context(file_context: Arc<FileContext>) {
    let rule_result_context = run_rules(&file_context);
    let cli_outputs = get_cli_outputs_from_rule_results(&file_context, rule_result_context);

    // send cli outputs over to output thread
    for out in cli_outputs {
        file_context.send_output(out);
    }
}

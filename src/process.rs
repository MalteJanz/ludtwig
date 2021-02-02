use crate::analyzer::analyze;
use crate::output::{Output, OutputMessage};
use crate::writer::write_tree;
use crate::CliContext;
use async_std::fs;
use async_std::path::PathBuf;
use async_std::sync::Arc;
use async_std::task;
use ludtwig_parser::ast::SyntaxNode;

/// The context for a single file.
#[derive(Debug)]
pub struct FileContext {
    pub cli_context: Arc<CliContext>,

    /// The file path that is associated with this context
    pub file_path: PathBuf,

    /// The parsed [SyntaxNode] AST for this file / context.
    pub tree: SyntaxNode,
}

impl FileContext {
    /// Helper function to send some [Output] to the user for this specific file.
    pub async fn send_output(&self, output: Output) {
        self.cli_context
            .send_output(OutputMessage {
                file: self.file_path.clone(),
                output,
            })
            .await;
    }
}

/// Process a single file with it's filepath.
pub async fn process_file(path: PathBuf, cli_context: Arc<CliContext>) {
    // notify the output about this file (to increase the processed file counter)
    cli_context
        .send_output(OutputMessage {
            file: path.clone(),
            output: Output::None,
        })
        .await;

    let file_content = match fs::read_to_string(&path).await {
        Ok(f) => f,
        Err(_) => {
            cli_context
                .send_output(OutputMessage {
                    file: path.clone(),
                    output: Output::Error("Can't read file".into()),
                })
                .await;

            return;
        }
    };

    let tree = match ludtwig_parser::parse(&file_content) {
        Ok(t) => t,
        Err(e) => {
            cli_context
                .send_output(OutputMessage {
                    file: path.clone(),
                    output: Output::Error(e.pretty_helpful_error_string(&file_content)),
                })
                .await;

            return;
        }
    };

    let file_context = Arc::new(FileContext {
        cli_context,
        file_path: path,
        tree,
    });

    work_with_file_context(file_context).await;
}

/// Do all the analyzing and writing work concurrently on the [FileContext].
async fn work_with_file_context(file_context: Arc<FileContext>) {
    let mut futures = vec![];

    if !file_context.cli_context.no_analysis {
        let clone = Arc::clone(&file_context);
        futures.push(task::spawn(async move {
            analyze(clone).await;
        }));
    }

    if !file_context.cli_context.no_writing {
        let clone = Arc::clone(&file_context);
        futures.push(task::spawn(async move {
            write_tree(clone).await;
        }));
    }

    drop(file_context);

    for fut in futures {
        fut.await;
    }
}

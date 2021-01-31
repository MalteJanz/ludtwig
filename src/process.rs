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

    /// The file path that is associated with this context and the parsed [SyntaxNode] AST.
    pub file_path: Arc<PathBuf>,

    pub tree: SyntaxNode,
}

impl FileContext {
    /// Helper function to send some [Output] to the user for this specific file.
    pub async fn send_output(&self, output: Output) {
        self.cli_context
            .send_output(OutputMessage {
                file: Arc::clone(&self.file_path),
                output,
            })
            .await;
    }
}

/// Process a single file with it's filepath.
pub async fn process_file(path: PathBuf, cli_context: Arc<CliContext>) {
    let path = Arc::new(path);

    let file_content = match fs::read_to_string(&*path).await {
        Ok(f) => f,
        Err(_) => {
            cli_context
                .send_output(OutputMessage {
                    file: Arc::clone(&path),
                    output: Output::Error("Can't read file".into()),
                })
                .await;

            return;
        }
    };

    let tree = task::spawn(async move {
        let tree = match ludtwig_parser::parse(&file_content) {
            Ok(r) => r,
            Err(e) => {
                return Err(e.pretty_helpful_error_string(&file_content));
            }
        };

        Ok(tree)
    })
    .await;

    let tree = match tree {
        Ok(t) => {
            cli_context
                .send_output(OutputMessage {
                    file: Arc::clone(&path),
                    output: Output::None,
                })
                .await;

            t
        }
        Err(e) => {
            cli_context
                .send_output(OutputMessage {
                    file: Arc::clone(&path),
                    output: Output::Error(e),
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

    let mut futs = vec![];

    if !file_context.cli_context.no_analysis {
        let clone = Arc::clone(&file_context);
        futs.push(task::spawn(async move {
            analyze(clone).await;
        }));
    }

    if !file_context.cli_context.no_writing {
        futs.push(task::spawn(async move {
            write_tree(file_context).await;
        }));
    }

    for fut in futs {
        fut.await;
    }
}

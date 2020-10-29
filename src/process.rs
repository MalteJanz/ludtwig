use crate::analyzer::analyze;
use crate::output::OutputMessage;
use crate::output::OutputType;
use crate::writer::write_tree;
use crate::CliContext;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use twig::ast::HtmlNode;

#[derive(Debug)]
pub struct FileContext {
    pub cli_context: Arc<CliContext>,
    pub file_path: PathBuf,
    pub tree: HtmlNode,
}

pub async fn process_file(path: PathBuf, cli_context: Arc<CliContext>) {
    let file_content = match fs::read_to_string(&path).await {
        Ok(f) => f,
        Err(_) => {
            cli_context
                .output_tx
                .send(OutputMessage {
                    file: path.into(),
                    message: "Can't read file".to_string(),
                    output_type: OutputType::Error,
                })
                .await
                .unwrap();
            return;
        }
    };

    let tree = tokio::task::spawn_blocking(move || {
        let tree = match twig::parse(&file_content) {
            Ok(r) => r,
            Err(e) => {
                return Err(e.pretty_helpful_error_string(&file_content));
            }
        };

        Ok(tree)
    })
    .await
    .unwrap();

    let tree = match tree {
        Ok(t) => t,
        Err(e) => {
            cli_context
                .output_tx
                .send(OutputMessage {
                    file: path.into(),
                    message: e,
                    output_type: OutputType::Error,
                })
                .await
                .unwrap();

            return;
        }
    };

    let file_context = Arc::new(FileContext {
        cli_context,
        file_path: path,
        tree,
    });

    let mut futs = vec![];

    if file_context.cli_context.no_analysis == false {
        let clone = Arc::clone(&file_context);
        futs.push(tokio::spawn(async move {
            analyze(clone).await;
        }));
    }

    if file_context.cli_context.no_writing == false {
        futs.push(tokio::spawn(async move {
            write_tree(file_context).await;
        }));
    }

    for fut in futs {
        fut.await.unwrap();
    }
}

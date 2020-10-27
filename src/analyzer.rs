use crate::output::OutputMessage;
use crate::output::OutputType;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::mpsc;
use twig::ast::HtmlNode;

pub async fn analyze<'a>(path: PathBuf, tree: Arc<HtmlNode>, tx: mpsc::Sender<OutputMessage>) {
    let clone = Arc::clone(&tree);
    tokio::spawn(async move {
        analyze_blocks(&path, &clone, tx, None).await;
    })
    .await
    .unwrap();

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    path: &'a PathBuf,
    node: &'a HtmlNode,
    tx: mpsc::Sender<OutputMessage>,
    parent_block_name: Option<&'a str>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            HtmlNode::Root(root) => {
                for child in root {
                    analyze_blocks(path, child, tx.clone(), parent_block_name).await;
                }
            }
            HtmlNode::Tag(tag) => {
                for child in &tag.children {
                    analyze_blocks(path, child, tx.clone(), parent_block_name).await;
                }
            }
            HtmlNode::TwigBlock(twig) => {
                if let Some(parent) = parent_block_name {
                    if !twig.name.contains(parent) {
                        tx.send(OutputMessage {
                            file: path.into(),
                            message: format!(
                                "Twig block name '{}' does not contain parent twig block name '{}'",
                                twig.name, parent
                            ),
                            output_type: OutputType::Warning,
                        })
                        .await
                        .unwrap();
                    }
                }

                for child in &twig.children {
                    analyze_blocks(path, child, tx.clone(), Some(&twig.name)).await;
                }
            }
            _ => {}
        }
    })
}

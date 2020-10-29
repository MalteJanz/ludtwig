use crate::output::OutputMessage;
use crate::output::OutputType;
use crate::process::FileContext;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use twig::ast::HtmlNode;

pub async fn analyze<'a>(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);
    tokio::spawn(async move {
        analyze_blocks(&file_context.tree, clone, None).await;
    })
    .await
    .unwrap();

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    node: &'a HtmlNode,
    file_context: Arc<FileContext>,
    parent_block_name: Option<&'a str>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            HtmlNode::Root(root) => {
                for child in root {
                    analyze_blocks(child, Arc::clone(&file_context), parent_block_name).await;
                }
            }
            HtmlNode::Tag(tag) => {
                for child in &tag.children {
                    analyze_blocks(child, Arc::clone(&file_context), parent_block_name).await;
                }
            }
            HtmlNode::TwigBlock(twig) => {
                if let Some(parent) = parent_block_name {
                    if !twig.name.contains(parent) {
                        file_context
                            .cli_context
                            .output_tx
                            .send(OutputMessage {
                                file: file_context.file_path.clone(),
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
                    analyze_blocks(child, Arc::clone(&file_context), Some(&twig.name)).await;
                }
            }
            _ => {}
        }
    })
}

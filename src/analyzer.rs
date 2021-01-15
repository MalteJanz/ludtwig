use crate::output::Output;
use crate::process::FileContext;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use twig::ast::{SyntaxNode, TwigStructure};

/// Entry function for doing the analyzing.
/// It spawns all analyzer functions concurrently.
pub async fn analyze<'a>(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);
    tokio::spawn(async move {
        analyze_blocks(&file_context.tree, &clone, None).await;
    })
    .await
    .unwrap();

    // run more analyzers in parallel...
}

/// Example analyzer:
/// Checks if the twig block name contains the block name of it's parent twig block.
/// This should reduce typing errors that humans will produce.
fn analyze_blocks<'a>(
    node: &'a SyntaxNode,
    file_context: &'a FileContext,
    parent_block_name: Option<&'a str>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            SyntaxNode::Root(root) => {
                for child in root {
                    analyze_blocks(child, file_context, parent_block_name).await;
                }
            }
            SyntaxNode::Tag(tag) => {
                for child in &tag.children {
                    analyze_blocks(child, file_context, parent_block_name).await;
                }
            }
            SyntaxNode::TwigStructure(TwigStructure::TwigBlock(twig)) => {
                if let Some(parent) = parent_block_name {
                    if !twig.name.contains(parent) {
                        file_context
                            .send_output(Output::Warning(format!(
                                "Twig child with block name '{}' does not contain parent twig block name '{}'",
                                twig.name, parent
                            )))
                            .await;
                    }
                }

                for child in &twig.children {
                    analyze_blocks(child, file_context, Some(&twig.name)).await;
                }
            }
            _ => {}
        }
    })
}

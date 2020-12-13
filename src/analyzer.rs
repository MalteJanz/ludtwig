use crate::output::Output;
use crate::process::FileContext;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use twig::ast::HtmlNode;

pub async fn analyze<'a>(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);
    tokio::spawn(async move {
        analyze_blocks(&file_context.tree, &clone, None).await;
    })
    .await
    .unwrap();

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    node: &'a HtmlNode,
    file_context: &'a FileContext,
    parent_block_name: Option<&'a str>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            HtmlNode::Root(root) => {
                for child in root {
                    analyze_blocks(child, file_context, parent_block_name).await;
                }
            }
            HtmlNode::Tag(tag) => {
                for child in &tag.children {
                    analyze_blocks(child, file_context, parent_block_name).await;
                }
            }
            HtmlNode::TwigBlock(twig) => {
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

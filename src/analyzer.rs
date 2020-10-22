use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use twig::ast::HtmlNode;

pub async fn analyze<'a>(tree: Arc<HtmlNode>) {
    let clone = Arc::clone(&tree);
    tokio::spawn(async move {
        analyze_blocks(&clone, None).await;
    })
    .await
    .unwrap();

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    node: &'a HtmlNode,
    parent_block_name: Option<&'a str>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            HtmlNode::Root(root) => {
                for child in root {
                    analyze_blocks(child, parent_block_name).await;
                }
            }
            HtmlNode::Tag(tag) => {
                for child in &tag.children {
                    analyze_blocks(child, parent_block_name).await;
                }
            }
            HtmlNode::TwigBlock(twig) => {
                //println!("compare {:?} - {:?}", parent_block_name, &twig.name);
                if let Some(parent) = parent_block_name {
                    if !twig.name.contains(parent) {
                        println!("found error because twig block name '{}' does not contain parent twig block name '{}'", twig.name, parent);
                    }
                }

                for child in &twig.children {
                    analyze_blocks(child, Some(&twig.name)).await;
                }
            }
            _ => {}
        }
    })
}

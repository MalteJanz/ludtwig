use crate::output::Output;
use crate::process::FileContext;
use ludtwig_parser::ast::{SyntaxNode, TwigBlock, TwigStructure};
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

/// Entry function for doing the analyzing.
/// It spawns all analyzer functions concurrently.
pub async fn analyze<'a>(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);

    tokio::spawn(async move {
        let mut block_names = HashSet::new();
        analyze_blocks(&file_context.tree, &clone, &mut block_names).await;
    })
    .await
    .unwrap();

    /*
    tokio::spawn(async move {
        analyze_blocks(&file_context.tree, &clone, None).await;
    })
    .await
    .unwrap();
    */

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    node: &'a SyntaxNode,
    file_context: &'a FileContext,
    block_names: &'a mut HashSet<String>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        match node {
            SyntaxNode::Root(root) => {
                for child in root {
                    analyze_blocks(child, file_context, block_names).await;
                }
            }
            SyntaxNode::Tag(tag) => {
                // TODO: blocks in html tag attributes are not visited yet. Iteration logic should be refactored.
                for child in &tag.children {
                    analyze_blocks(child, file_context, block_names).await;
                }
            }
            SyntaxNode::TwigStructure(twig_structure) => match twig_structure {
                TwigStructure::TwigBlock(twig) => {
                    check_twig_block_name_for_duplicate(twig, file_context, block_names).await;

                    for child in &twig.children {
                        analyze_blocks(child, file_context, block_names).await;
                    }
                }
                TwigStructure::TwigFor(twig) => {
                    for child in &twig.children {
                        analyze_blocks(child, file_context, block_names).await;
                    }
                }
                TwigStructure::TwigIf(twig) => {
                    for arm in &twig.if_arms {
                        for child in &arm.children {
                            analyze_blocks(child, file_context, block_names).await;
                        }
                    }
                }
                TwigStructure::TwigApply(twig) => {
                    for child in &twig.children {
                        analyze_blocks(child, file_context, block_names).await;
                    }
                }
                TwigStructure::TwigSetCapture(twig) => {
                    for child in &twig.children {
                        analyze_blocks(child, file_context, block_names).await;
                    }
                }
            },
            _ => {}
        }
    })
}

async fn check_twig_block_name_for_duplicate(
    block: &TwigBlock<SyntaxNode>,
    file_context: &FileContext,
    block_names: &mut HashSet<String>,
) {
    if block_names.contains(&block.name) {
        file_context
            .send_output(Output::Warning(format!(
                "Duplicate twig block name found: '{}'",
                &block.name
            )))
            .await;
    } else {
        block_names.insert(block.name.clone());
    }
}

/*
// Example analyzer:
// Checks if the twig block name contains the block name of it's parent twig block.
// This should reduce typing errors that humans will produce.
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
*/

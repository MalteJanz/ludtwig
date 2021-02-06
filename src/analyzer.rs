use crate::output::Output;
use crate::process::FileContext;
use async_std::sync::Arc;
use async_std::task;
use ludtwig_parser::ast::{SyntaxNode, TagAttribute, TwigBlock, TwigStructure};
use std::collections::HashSet;

/// Entry function for doing the analyzing.
/// It spawns all analyzer functions concurrently.
pub async fn analyze(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);

    task::spawn(async move {
        let mut block_names = HashSet::new();
        analyze_blocks(&file_context.tree, &clone, &mut block_names).await;
    })
    .await;

    /*
    task::spawn(async move {
        analyze_blocks(&file_context.tree, &clone, None).await;
    })
    .await;
    */

    // run more analyzers in parallel...
}

async fn analyze_blocks<'a>(
    tree: &'a SyntaxNode,
    file_context: &'a FileContext,
    block_names: &'a mut HashSet<String>,
) {
    for node in tree.iter() {
        if let SyntaxNode::TwigStructure(TwigStructure::TwigBlock(block)) = node {
            check_twig_block_name_for_duplicate(block, file_context, block_names).await;
            continue;
        }

        for attr in node.attribute_iter() {
            // also check attributes for twig blocks.
            if let TagAttribute::TwigStructure(TwigStructure::TwigBlock(block)) = attr {
                check_twig_block_name_for_duplicate(block, file_context, block_names).await;
            }
        }
    }
}

async fn check_twig_block_name_for_duplicate<C>(
    block: &TwigBlock<C>,
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

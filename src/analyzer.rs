use crate::output::Output;
use crate::process::FileContext;
use ludtwig_parser::ast::{SyntaxNode, TagAttribute, TwigBlock, TwigStructure};
use std::collections::HashSet;
use std::sync::Arc;

/// Entry function for doing the analyzing.
pub fn analyze(file_context: Arc<FileContext>) {
    let clone = Arc::clone(&file_context);

    let mut block_names = HashSet::new();
    analyze_blocks(&file_context.tree, &clone, &mut block_names);

    // run more analyzers in parallel...
}

fn analyze_blocks<'a>(
    tree: &'a SyntaxNode,
    file_context: &'a FileContext,
    block_names: &'a mut HashSet<String>,
) {
    for node in tree.iter() {
        if let SyntaxNode::TwigStructure(TwigStructure::TwigBlock(block)) = node {
            check_twig_block_name_for_duplicate(block, file_context, block_names);
            continue;
        }

        for attr in node.attribute_iter() {
            // also check attributes for twig blocks.
            if let TagAttribute::TwigStructure(TwigStructure::TwigBlock(block)) = attr {
                check_twig_block_name_for_duplicate(block, file_context, block_names);
            }
        }
    }
}

fn check_twig_block_name_for_duplicate<C>(
    block: &TwigBlock<C>,
    file_context: &FileContext,
    block_names: &mut HashSet<String>,
) {
    if block_names.contains(&block.name) {
        file_context.send_output(Output::Warning(format!(
            "Duplicate twig block name found: '{}'",
            &block.name
        )));
    } else {
        block_names.insert(block.name.clone());
    }
}

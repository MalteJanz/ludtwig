use crate::writer::write_tree;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use tokio::fs;
use twig::ast::HtmlNode;

pub async fn process_file<P>(path: P)
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    println!("\nFile: {}", path.to_string_lossy());
    let file_content_result = fs::read_to_string(path).await;
    assert!(
        file_content_result.is_ok(),
        "Can't read file '{}'",
        path.to_string_lossy()
    );
    let file_content = file_content_result.unwrap();

    let tree = tokio::task::spawn_blocking(move || {
        let tree = match twig::parse(&file_content) {
            Ok(r) => r,
            Err(e) => {
                panic!("{}", e.pretty_helpful_error_string(&file_content));
            }
        };

        tree
    })
    .await
    .unwrap();

    let mut raw_path = path.parent().unwrap_or(Path::new(""));

    let stem = path.file_stem().unwrap_or(OsStr::new(""));

    let mut filename = stem.to_os_string();
    filename.push(".formatted.");

    if let Some(extension) = path.extension() {
        filename.push(extension);
    }

    let file_path = raw_path.join(filename);
    write_tree(file_path, &tree).await;

    //print_twig_block_hierarchy(&tree, 0);
}

pub fn print_twig_block_hierarchy(node: &HtmlNode, spaces: i32) {
    match node {
        HtmlNode::TwigBlock(block) => {
            for _ in 0..spaces {
                print!(" ")
            }
            println!("{}", block.name);

            for child in &block.children {
                print_twig_block_hierarchy(&child, spaces + 4);
            }
        }
        HtmlNode::Tag(tag) => {
            for child in &tag.children {
                print_twig_block_hierarchy(&child, spaces);
            }
        }
        _ => {}
    }
}

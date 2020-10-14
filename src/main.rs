use std::fs;
use twig::ast::*;

fn main() {
    let file_content =
        fs::read_to_string("example_files/another-whitespace-sensitive-example.html.twig")
            .expect("Can't read file 'example.html'");
    let result = match twig::parse(&file_content) {
        Ok(r) => r,
        Err(e) => {
            panic!("Parsing error: {}", e);
        }
    };

    print_twig_block_hierarchy(&result, 0);
}

fn print_twig_block_hierarchy(node: &HtmlNode, spaces: i32) {
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

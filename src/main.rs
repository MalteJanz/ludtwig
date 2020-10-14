use crate::parser::HtmlNode;
use nom::error::VerboseError;
use std::fs;

mod parser;

fn main() {
    let file_content =
        fs::read_to_string("complex.html.twig").expect("Can't read file 'example.html'");
    let result = parser::parse(&file_content);

    match result {
        Ok(result) => {
            println!("{:#?}", result);
            print_twig_block_hierarchy(&result.1, 0)
        }
        Err(nom::Err::Error(e)) => {
            println!("Raw error:\n{:#?}", &e);
            println!(
                "Parsing error:\n{}",
                nom::error::convert_error(&file_content, e)
            );
        }
        Err(nom::Err::Failure(e)) => {
            println!("Raw error:\n{:#?}", &e);
            println!(
                "Parsing error:\n{}",
                nom::error::convert_error(&file_content, e)
            );
        }
        _ => println!("Unkown error"),
    }
}

fn print_twig_block_hierarchy(node: &HtmlNode, spaces: i32) {
    match node {
        HtmlNode::TwigBlock(block) => {
            for _ in 0..spaces {
                print!(" ")
            }
            print!("{}\n", block.name);

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

use crate::parser::HtmlNode;
use nom::error::VerboseError;
use std::fs;

mod parser;

fn main() {
    let file_content = fs::read_to_string("example.html").expect("Can't read file 'example.html'");
    let result = parser::parse(&file_content);

    match result {
        Ok(result) => {
            println!("{:#?}", result);
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

use std::fs;
mod parser;

fn main() {
    let file_content = fs::read_to_string("simple.html").expect("Can't read file 'example.html'");
    let result = parser::parse(&file_content).unwrap();

    println!("{:#?}", result);
}

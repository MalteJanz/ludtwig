use crate::lexer::lex;
use crate::syntax::untyped::{build_example_tree, SyntaxNode};

mod lexer;
pub mod syntax;

pub fn parse(input_text: &str) -> SyntaxNode {
    let lex_result = lex(input_text);
    // TODO: add real parsing call here
    build_example_tree()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_not_panic_on_simple_parse_call() {
        parse("asdf");
    }
}

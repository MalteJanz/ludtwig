use crate::syntax::untyped::{build_example_tree, SyntaxNode};

pub mod lexer;
pub mod syntax;

pub fn parse(input_text: &str) -> SyntaxNode {
    build_example_tree()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_not_panic_on_parse_call() {
        parse("asdf");
    }
}

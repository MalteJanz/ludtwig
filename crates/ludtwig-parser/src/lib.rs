use crate::syntax::typed::Root;
use crate::syntax::untyped::build_example_tree;
use rowan::ast::AstNode;

pub mod syntax;

pub fn parse(input_text: &str) -> Root {
    Root::cast(build_example_tree()).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_not_panic_on_parse_call() {
        parse("asdf");
    }
}

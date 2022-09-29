extern crate core;

pub use parser::parse;
pub use parser::Parse;
pub use parser::ParseError;

use crate::lexer::lex;

mod grammar;
mod lexer;
mod parser;
pub mod syntax;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::typed::AstNode;
    use crate::syntax::typed::HtmlTag;
    use crate::syntax::untyped::SyntaxNode;
    use rowan::ast::support;

    #[test]
    fn it_should_not_panic_on_simple_parse_call() {
        parse("asdf");
    }

    #[test]
    fn it_should_not_panic_on_prev_sibling_call() {
        let parse = parse("<div>a<hr/></div>");
        let root = SyntaxNode::new_root(parse.green_node);
        // println!("{}", debug_tree(&root));

        let prev = root.prev_sibling();
        // println!("prev sibling: {:?}", prev);
        assert!(prev.is_none());

        let child: HtmlTag = support::child(&root).unwrap();
        let prev = child.syntax().prev_sibling();
        // println!("{:?} prev sibling: {:?}", child, prev);
        assert!(prev.is_none());

        let child: HtmlTag = support::child(child.body().unwrap().syntax()).unwrap();
        let prev = child.syntax().prev_sibling();
        // println!("{:?} prev sibling: {:?}", child, prev);
        assert!(prev.is_some());
    }
}

#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::doc_markdown)]
//! # Parser
//! This is a handwritten top-down parser for the [twig templating language](https://twig.symfony.com/) combined with HTML.
//! It's mostly developed together with the linter / formatter [ludtwig](https://github.com/MalteJanz/ludtwig).
//!
//! Parsing both Twig and HTML together into a single hierarchical syntax tree gives some benefits,
//! valid syntax trees follow some desirable properties:
//! - non-self-closing HTML tags always need a corresponding closing tag
//! - opening and closing HTML tag must exist inside the same Twig block
//! - Twig syntax is only allowed in specific places instead of everywhere
//!
//! which in turn make these templates and the HTML generated in the end less error-prone.
//!
//! # Syntax trees
//! The parser constructs a syntax tree based on the library [rowan](https://github.com/rust-analyzer/rowan).
//! A conceptual overview can be found here [Syntax in rust-analyzer](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md).
//! Basically the syntax tree consists of three layers:
//! - GreenNodes
//! - SyntaxNodes (aka RedNodes)
//! - AstNodes (defined in this crate)
//!
//! # Examples
//!
//! ## Parsing into a syntax tree
//! ```
//! use ludtwig_parser::syntax::untyped::{debug_tree, SyntaxNode};
//!
//! let parse = ludtwig_parser::parse("{{ 42 }}");
//!
//! // parsing might result in errors
//! assert!(parse.errors.is_empty());
//!
//! // in any case it will always produce a usable syntax tree
//! // and provide a green node of the root.
//!
//! // for getting a SyntaxNode of the root you can use this shortcut
//! // or construct the root by hand from the GreenNode (see SyntaxNode::new_root)
//! let (tree_root, errors) = parse.split();
//!
//! // you can now iterate or search through the syntax tree
//! // or debug print it with
//! println!("{}", debug_tree(&tree_root));
//! # assert_eq!(debug_tree(&tree_root), r##"ROOT@0..8
//! #   TWIG_VAR@0..8
//! #     TK_OPEN_CURLY_CURLY@0..2 "{{"
//! #     TWIG_EXPRESSION@2..5
//! #       TWIG_LITERAL_NUMBER@2..5
//! #         TK_WHITESPACE@2..3 " "
//! #         TK_NUMBER@3..5 "42"
//! #     TK_WHITESPACE@5..6 " "
//! #     TK_CLOSE_CURLY_CURLY@6..8 "}}""##);
//! ```
//!
//! ## Iterate in Preorder
//! ```
//! # let parse = ludtwig_parser::parse("{{ 42 }}");
//! # let (tree_root, errors) = parse.split();
//!
//! use ludtwig_parser::syntax::untyped::WalkEvent;
//! use ludtwig_parser::syntax::typed::AstNode;
//! use ludtwig_parser::syntax::typed::TwigVar;
//!
//! let mut preorder = tree_root.preorder();
//! while let Some(walk_event) = preorder.next() {
//!     match walk_event {
//!        WalkEvent::Enter(syntax_node) => {
//!            if let Some(twig_var) = TwigVar::cast(syntax_node) {
//!                // do something with ast node here
//!            }
//!        }
//!        WalkEvent::Leave(syntax_node) => {}
//!     }
//! }
//! ```
//!
//! ## Utilities for retrieving a specific AstNode or Token
//! As of now there might be missing utility method implementations on AstNode's.
//! You can use these instead to retrieve any AstNode / Token you want (under a given AstNode).
//! ```
//! # let parse = ludtwig_parser::parse("{{ 42 }}");
//! # let (tree_root, errors) = parse.split();
//!
//! use ludtwig_parser::syntax::typed::{AstNode, support, TwigVar};
//! use ludtwig_parser::syntax::untyped::SyntaxToken;
//! use ludtwig_parser::T;
//!
//! // finding a specific AstNode in a (sub) tree (first occurrence)
//! // Note: only looks through the direct children of the given SyntaxNode
//! let twig_var: TwigVar = support::child(&tree_root).unwrap();
//!
//! // you can freely get the underlaying SyntaxNode of an AstNode with
//! let twig_var_syntax_node = twig_var.syntax();
//!
//! // finding a specific Token in a (sub) tree (first occurrence)
//! // Note: only looks through the direct children of the given SyntaxNode
//! let twig_var_opening_braces: SyntaxToken = support::token(twig_var_syntax_node, T!["{{"]).unwrap();
//!
//! // finding specific AstNode's in a (sub) tree (all occurrence)
//! // returns an iterator
//! // Note: only looks through the direct children of the given SyntaxNode
//! let twig_vars = support::children::<TwigVar>(&tree_root);
//! # assert_eq!(twig_vars.count(), 1);
//! ```
//!

pub use parser::parse;
pub use parser::Parse;
pub use parser::ParseError;

use crate::lexer::lex;

mod grammar;
mod lexer;
mod parser;
pub mod syntax;

pub use grammar::TWIG_NAME_REGEX;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::typed::AstNode;
    use crate::syntax::typed::HtmlTag;
    use crate::syntax::untyped::SyntaxNode;
    use rowan::ast::support;

    #[test]
    fn it_should_not_panic_on_simple_parse_call() {
        let _ = parse("asdf");
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

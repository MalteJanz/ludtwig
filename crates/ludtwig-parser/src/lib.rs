extern crate core;

use crate::lexer::lex;

mod grammar;
mod lexer;
mod parser;
pub mod syntax;

pub use parser::parse;
pub use parser::Parse;
pub use parser::ParseError;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_not_panic_on_simple_parse_call() {
        parse("asdf");
    }
}

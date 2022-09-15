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

    #[test]
    fn it_should_not_panic_on_simple_parse_call() {
        parse("asdf");
    }
}

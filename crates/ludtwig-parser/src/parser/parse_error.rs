use crate::syntax::untyped::{SyntaxKind, TextRange};
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParseError {
    pub expected: Vec<SyntaxKind>,
    pub found: Option<SyntaxKind>,
    pub range: TextRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected {}, but found {}",
            u32::from(self.range.start()),
            u32::from(self.range.start()),
            self.expected[0],
            self.found.unwrap(),
        )
    }
}

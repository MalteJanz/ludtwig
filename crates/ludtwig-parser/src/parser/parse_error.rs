use crate::syntax::untyped::{SyntaxKind, TextRange};
use std::fmt;
use std::fmt::Formatter;
use std::fmt::Write;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParseError {
    pub expected: Vec<SyntaxKind>,
    pub found: Option<SyntaxKind>,
    pub range: TextRange,
}

impl ParseError {
    pub fn expected_message(&self) -> Result<String, fmt::Error> {
        let mut s = String::from("expected ");
        let num_expected = self.expected.len();
        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if idx == 0 {
                write!(s, "{}", expected_kind)?;
            } else if idx == num_expected - 1 {
                write!(s, " or {}", expected_kind)?;
            } else {
                write!(s, ", {}", expected_kind)?;
            }
        }

        if let Some(found) = self.found {
            write!(s, ", but found {}", found)?;
        }

        Ok(s)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: ",
            u32::from(self.range.start()),
            u32::from(self.range.start()),
        )?;

        write!(f, "{}", self.expected_message()?)?;

        Ok(())
    }
}

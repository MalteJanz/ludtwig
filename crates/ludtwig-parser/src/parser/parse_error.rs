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
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.start()),
        )?;

        let num_expected = self.expected.len();
        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if idx == 0 {
                write!(f, "{}", expected_kind)?;
            } else if idx == num_expected - 1 {
                write!(f, " or {}", expected_kind)?;
            } else {
                write!(f, ", {}", expected_kind)?;
            }
        }

        if let Some(found) = self.found {
            write!(f, ", but found {}", found)?;
        }

        Ok(())
    }
}

use std::fmt;
use std::fmt::Formatter;

use crate::syntax::untyped::{SyntaxKind, TextRange};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParseErrorBuilder {
    pub(super) range: Option<TextRange>,
    pub(super) found: Option<SyntaxKind>,
    pub(super) expected: String,
}

impl ParseErrorBuilder {
    /// expected should only describe what is missing.
    /// It is later passed into "expected <expectedString> but found ..."
    pub fn new<S>(expected: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            range: None,
            found: None,
            expected: expected.into(),
        }
    }

    #[allow(unused)]
    pub fn range(mut self, range: TextRange) -> Self {
        self.range = Some(range);
        self
    }

    #[allow(unused)]
    pub fn found(mut self, found: SyntaxKind) -> Self {
        self.found = Some(found);
        self
    }

    pub(super) fn build(self) -> ParseError {
        ParseError {
            range: self.range.unwrap(),
            found: self.found,
            expected: self.expected,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParseError {
    pub range: TextRange,
    pub found: Option<SyntaxKind>,
    pub expected: String,
}

impl ParseError {
    pub fn expected_message(&self) -> String {
        match self.found {
            Some(found) => format!("expected {} but found {}", self.expected, found),
            None => format!("expected {} but reached end of file", self.expected),
        }
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

        write!(f, "{}", self.expected_message())
    }
}

use crate::lexer::Token;
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

    pub(crate) fn at_token(mut self, token: &Token) -> Self {
        self.range = Some(token.range);
        self.found = Some(token.kind);
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
    #[must_use]
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
            u32::from(self.range.end()),
        )?;

        write!(f, "{}", self.expected_message())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::T;
    use rowan::TextSize;

    #[test]
    fn parse_error_display() {
        let range = TextRange::new(TextSize::from(3), TextSize::from(5));
        let parse_error = ParseError {
            range,
            found: Some(T!["{%"]),
            expected: "word".to_string(),
        };

        assert_eq!(
            format!("{}", parse_error),
            "error at 3..5: expected word but found {%"
        );
    }
}

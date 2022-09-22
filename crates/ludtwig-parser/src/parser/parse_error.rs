use std::fmt;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

use crate::syntax::untyped::{SyntaxKind, TextRange};

/// Describes single syntax kind with optional matching content
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expectation {
    pub kind: SyntaxKind,
    pub content: Option<String>,
}

impl Expectation {
    pub fn new(kind: SyntaxKind, content: Option<String>) -> Self {
        Self { kind, content }
    }
}

impl From<SyntaxKind> for Expectation {
    fn from(kind: SyntaxKind) -> Self {
        Self {
            kind,
            content: None,
        }
    }
}

impl Display for Expectation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(content) = &self.content {
            write!(f, "'{}'", content)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

/// Describes a chain of expectations (for example first '{%' then 'endblock')
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExpectationChain {
    pub chain: Vec<Expectation>,
}

impl ExpectationChain {
    pub fn new(chain: Vec<Expectation>) -> Self {
        Self { chain }
    }
}

impl From<Expectation> for ExpectationChain {
    fn from(expectation: Expectation) -> Self {
        Self {
            chain: vec![expectation],
        }
    }
}

impl Display for ExpectationChain {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (idx, item) in self.chain.iter().enumerate() {
            if idx == 0 {
                write!(f, "{}", item)?;
            } else {
                write!(f, " {}", item)?;
            }
        }

        Ok(())
    }
}

/// Describes a full parser error, which contains all the ExpectationChains which could have prevented this error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParseError {
    pub expected: Vec<ExpectationChain>,
    pub found: Option<SyntaxKind>,
    pub range: TextRange,
}

impl ParseError {
    pub fn expected_message(&self) -> Result<String, fmt::Error> {
        // TODO: idea: merge overlapping expectations into one chain (caused by multiple parser.expect(...) calls)

        let mut s = String::from("expected\n");
        let num_expected = self.expected.len();
        for (idx, chain) in self.expected.iter().enumerate() {
            if idx == 0 {
                write!(s, "{}", chain)?;
            } else if idx == num_expected - 1 {
                write!(s, "\nor\n{}", chain)?;
            } else {
                write!(s, "\n{}", chain)?;
            }
        }

        if let Some(found) = self.found {
            write!(s, "\nbut found {}", found)?;
        } else {
            write!(s, "\nbut reached end of file")?;
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

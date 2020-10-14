use nom::error::ErrorKind;
use nom::lib::std::fmt::Formatter;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum TwigParseError {
    Unparseable,
    MissingClosing,
}
// TODO: implement errors with more information so it can be reported back to the user!

impl Error for TwigParseError {}

impl Display for TwigParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TwigParseError::Unparseable => write!(f, "Unparseable input"),
            TwigParseError::MissingClosing => write!(f, "Missing closing tag / block"),
        }
    }
}

impl<I> nom::error::ParseError<I> for TwigParseError {
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self {
        TwigParseError::Unparseable
    }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

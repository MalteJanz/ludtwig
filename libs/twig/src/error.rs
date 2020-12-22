use nom::error::{ContextError, ErrorKind};
use nom::lib::std::fmt::Formatter;
use std::borrow::Cow;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct TwigParsingErrorInformation<I> {
    pub leftover: I,
    pub context: Option<Cow<'static, str>>,
    pub(crate) kind: ErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum TwigParseError<I> {
    ParsingError(TwigParsingErrorInformation<I>),
    ParsingFailure(TwigParsingErrorInformation<I>),
}

impl<I: std::fmt::Debug + std::fmt::Display> Error for TwigParseError<I> {}

impl<I: std::fmt::Debug + std::fmt::Display> Display for TwigParseError<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TwigParseError::ParsingError(info) => write!(
                f,
                "parsing error because: ({}, {:?}, {:?})",
                info.leftover, info.kind, info.context
            ),
            TwigParseError::ParsingFailure(info) => write!(
                f,
                "Unrecoverable parsing failure because: ({}, {:?}, {:?})",
                info.leftover, info.kind, info.context
            ),
        }
    }
}

impl<I: std::fmt::Debug + std::fmt::Display> nom::error::ParseError<I>
    for TwigParsingErrorInformation<I>
{
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self {
        //println!("[FROM_ERROR_KIND] {:?}: {:?}", _kind, _input);

        TwigParsingErrorInformation {
            leftover: _input,
            kind: _kind,
            context: None,
        }
    }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        //println!("[APPEND] {:?}: {:?}", _kind, _input);
        other
    }

    fn from_char(input: I, _: char) -> Self {
        //println!("[FROM_CHAR] {:?}", input);
        TwigParsingErrorInformation {
            leftover: input,
            kind: ErrorKind::Not,
            context: None,
        }
    }
}

impl<I: std::fmt::Debug + std::fmt::Display> ContextError<I> for TwigParsingErrorInformation<I> {
    fn add_context(_input: I, _ctx: &'static str, mut other: Self) -> Self {
        //println!("[ADD_CONTEXT] {} {:?} {:?}", _ctx, _input, other);
        other.context = Some(_ctx.into());

        other
    }
}

/// allows the error to have a dynamic (heap allocated) string as it's context.
pub(crate) trait DynamicParseError<I> {
    fn add_dynamic_context(input: I, ctx: String, other: Self) -> Self;
}

impl<I: std::fmt::Debug + std::fmt::Display> DynamicParseError<I>
    for TwigParsingErrorInformation<I>
{
    fn add_dynamic_context(_input: I, ctx: String, mut other: Self) -> Self {
        //println!("[ADD_DYNAMIC_CONTEXT] {:?} {:?} {:?}", _ctx, _input, other);
        other.context = Some(Cow::Owned(ctx));

        other
    }
}

impl<I> From<nom::Err<TwigParsingErrorInformation<I>>> for TwigParseError<I> {
    fn from(e: nom::Err<TwigParsingErrorInformation<I>>) -> Self {
        match e {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(i) => TwigParseError::ParsingError(i),
            nom::Err::Failure(i) => TwigParseError::ParsingFailure(i),
        }
    }
}

// error reporting logic
impl TwigParseError<&str> {
    /// Use the parsing error and the raw input to generate a human friendly error message.
    /// This message contains line and column information, the line itself and other information from the error.
    pub fn pretty_helpful_error_string(&self, input: &str) -> String {
        let mut output = String::with_capacity(256);

        let info = match self {
            TwigParseError::ParsingError(i) => i,
            TwigParseError::ParsingFailure(i) => i,
        };

        let (line, column, last_line) = get_line_and_column_of_subslice(input, info.leftover);

        output = format!(
            "{}Parsing goes wrong in line {} and column {} :\n",
            output, line, column
        );

        output = format!("{}{}\n", output, last_line);

        for _ in 0..(column - 1) {
            output = format!("{} ", output);
        }

        output = format!("{}^\n", output);

        for _ in 0..(column - 1) {
            output = format!("{} ", output);
        }

        output = format!("{}|\n", output);

        //output = format!("{}{:?}", output, info.kind);

        output = match &info.context {
            None => format!("{}{:?}", output, info.kind),
            Some(c) => format!("{}{}", output, c),
        };

        output
    }
}

trait SubsliceOffset {
    /**
    Returns the byte offset of an inner slice relative to an enclosing outer slice.

    Examples

    ```ignore
    let string = "a\nb\nc";
    let lines: Vec<&str> = string.lines().collect();
    assert!(string.subslice_offset(lines[0]) == Some(0)); // &"a"
    assert!(string.subslice_offset(lines[1]) == Some(2)); // &"b"
    assert!(string.subslice_offset(lines[2]) == Some(4)); // &"c"
    assert!(string.subslice_offset("other!") == None);
    ```
    */
    fn subslice_offset(&self, inner: &Self) -> Option<usize>;
}

impl SubsliceOffset for str {
    fn subslice_offset(&self, inner: &str) -> Option<usize> {
        let self_beg = self.as_ptr() as usize;
        let inner = inner.as_ptr() as usize;
        if inner < self_beg || inner > self_beg.wrapping_add(self.len()) {
            None
        } else {
            Some(inner.wrapping_sub(self_beg))
        }
    }
}

/// Given the raw input and a slice of the input.
/// Outputs the line and column number of the slice inside the input and
/// the whole last line which contains the slice.
///
/// # Warning
/// This function will most likely not work correctly with UTF8 strings because it iterates over bytes.
fn get_line_and_column_of_subslice<'a>(input: &'a str, slice: &'a str) -> (usize, usize, &'a str) {
    let offset = input.subslice_offset(slice).unwrap();
    let mut last_line_start = 0;
    let mut last_line_end = 1;
    let mut found = false;
    let mut lines = 0;
    let mut byte_number = 0;
    let mut last_byte = None;

    for (i, byte) in input.bytes().enumerate() {
        byte_number = i;
        if byte == b'\r' || byte == b'\n' {
            last_line_end = i + 1;

            if let Some(l_byte) = last_byte {
                if l_byte != b'\r' || l_byte != b'\n' {
                    lines += 1;
                }
            }

            if found {
                break;
            }

            last_line_start = last_line_end;
        }

        if i == offset {
            found = true;
        }

        last_byte = Some(byte);
    }

    // if the for loop did not found a newline in the last parsed line the end and start will be the same.
    if last_line_start == last_line_end {
        last_line_end = byte_number + 1;
        lines += 1;
    } else {
        last_line_end -= 1;
    }

    let last_line = &input[last_line_start..last_line_end];
    let column = offset - last_line_start + 1;

    (lines, column, last_line)
}

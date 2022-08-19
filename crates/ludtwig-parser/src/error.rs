use nom::error::{ContextError, ErrorKind};
use nom::lib::std::fmt::Formatter;
use std::borrow::Cow;
use std::cmp::min;
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
/// the whole last line which contains the slice beginning.
/// The last line does not contain any '\t' bytes and contains four ' ' spaces instead
/// (because columns are counted like this).
fn get_line_and_column_of_subslice<'a>(
    input: &'a str,
    slice: &'a str,
) -> (usize, usize, Cow<'a, str>) {
    let offset = input.subslice_offset(slice).unwrap();

    // count the lines and columns up to the slice position (offset)
    let mut line_start_offset: usize = 0;
    let mut line: usize = 1;
    let mut column: usize = 0;
    let mut tab_in_line_found: bool = false;
    for (i, char) in input[..min(input.len(), offset + 1)].char_indices() {
        if char == '\n' {
            line += 1;
            line_start_offset = i + char.len_utf8();
            column = 0;
            tab_in_line_found = false;
            continue;
        }

        if char == '\r' {
            // don't count this special byte as a column
            continue;
        }

        if char == '\t' {
            // count a tab as 4 columns
            column += 4;
            tab_in_line_found = true;
        } else {
            column += 1;
        }
    }

    if input.len() == offset {
        // the offset is outside the input string (so to points to basically to the next character behind the input string).
        // in this case the column should also point to that position.
        column += 1;
    }

    // find the next line ending (to construct the last line)
    let mut last_line_ending = if !input.is_empty() {
        input.len() - 1
    } else {
        0
    };
    let last_line_char_iter = input[min(last_line_ending, offset + 1)..].char_indices();
    for (i, char) in last_line_char_iter {
        // find the next line break ('\n' or '\r\n')
        if char == '\n' || char == '\r' {
            last_line_ending = i + offset;
            break;
        }

        if char == '\t' {
            tab_in_line_found = true;
        }
    }

    // get the last line slice
    let last_line = &input[line_start_offset..min(last_line_ending + 1, input.len())];

    if !tab_in_line_found {
        return (line, column, Cow::Borrowed(last_line));
    }

    // the last line needs special care because it contains tab bytes ('\t')
    let last_line = last_line.replace('\t', "    ");
    (line, column, Cow::Owned(last_line))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unix_line_and_column_info() {
        let whole = "first line\nsecond line\nthird line\nfourth line";
        let sub = &whole[29..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 3);
        assert_eq!(columns, 7);
        assert_eq!(last_line, "third line");
    }

    #[test]
    fn test_windows_line_and_column_info() {
        let whole = "first line\r\nsecond line\r\nthird line\r\nfourth line";
        let sub = &whole[31..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 3);
        assert_eq!(columns, 7);
        assert_eq!(last_line, "third line");
    }

    #[test]
    fn test_tabs_line_and_column_info() {
        let whole = "first line\nsecond\tline\nthird line\nfourth line";
        let sub = &whole[18..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 2);
        assert_eq!(columns, 11);
        // output should have sanitized the '\t' into four spaces because columns should count each tab as 4 spaces by default.
        assert_eq!(last_line, "second    line");
    }

    #[test]
    fn test_edges_line_and_column_info() {
        // slice starts at first character in the input
        let whole = "first\tline\nsecond line\nthird line\nfourth line";
        let sub = &whole[..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 1);
        assert_eq!(columns, 1);
        assert_eq!(last_line, "first    line");

        // slices starts at last character in the input
        let whole = "first line\nsecond line\nthird line\nfourth line";
        let sub = &whole[(whole.len() - 1)..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 4);
        assert_eq!(columns, 11);
        assert_eq!(last_line, "fourth line");

        // slice starts after last character in the input (slice is empty).
        let whole = "first line\nsecond line\nthird line\nfourth line";
        let sub = &whole[whole.len()..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 4);
        assert_eq!(columns, 12);
        assert_eq!(last_line, "fourth line");
    }

    #[test]
    fn test_utf8_chars_line_and_column_info() {
        let whole = "first line\nsecond line\nthird🙂line\nfourth line";
        let sub = &whole[32..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 3);
        assert_eq!(columns, 7);
        assert_eq!(last_line, "third🙂line");
    }

    #[test]
    fn test_line_and_column_info_at_eof() {
        let whole = "first line\nsecond line\nthird🙂line\nfourth line\n";
        let sub = &whole[whole.len()..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 5);
        assert_eq!(columns, 1);
        assert_eq!(last_line, "");
    }

    #[test]
    fn test_line_and_column_of_empty_string() {
        let whole = "";
        let sub = &whole[whole.len()..];

        let (lines, columns, last_line) = get_line_and_column_of_subslice(whole, sub);

        assert_eq!(lines, 1);
        assert_eq!(columns, 1);
        assert_eq!(last_line, "");
    }
}

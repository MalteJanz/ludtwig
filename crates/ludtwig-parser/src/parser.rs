use std::fmt::Write;

use rowan::GreenNode;

pub use parse_error::ParseError;
pub use parse_error::ParseErrorBuilder;

use crate::grammar::root;
use crate::lexer::Token;
use crate::parser::event::{CompletedMarker, EventCollection, Marker};
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
use crate::{lex, T};

pub(crate) mod event;
mod parse_error;
mod sink;
mod source;

/// Tokens which can lead to parsing of another element
/// (top level parsers under [`crate::grammar::parse_any_element`])
pub(crate) static GENERAL_RECOVERY_SET: &[SyntaxKind] =
    &[T!["{%"], T!["{{"], T!["{#"], T!["<"], T!["<!--"], T!["<!"]];

/// Parses a given string slice (of Twig+HTML code) into a syntax tree.
///
/// ## Example
/// ```
/// use ludtwig_parser::syntax::untyped::{debug_tree, SyntaxNode};
///
/// let parse = ludtwig_parser::parse("{{ 42 }}");
/// let (tree_root, errors) = parse.split();
///
/// assert_eq!(debug_tree(&tree_root), r##"ROOT@0..8
///   TWIG_VAR@0..8
///     TK_OPEN_CURLY_CURLY@0..2 "{{"
///     TWIG_EXPRESSION@2..5
///       TWIG_LITERAL_NUMBER@2..5
///         TK_WHITESPACE@2..3 " "
///         TK_NUMBER@3..5 "42"
///     TK_WHITESPACE@5..6 " "
///     TK_CLOSE_CURLY_CURLY@6..8 "}}""##);
/// ```
/// More examples can be found at the
/// [crate level documentation](crate).
#[must_use]
pub fn parse(input_text: &str) -> Parse {
    let lex_result = lex(input_text);
    let parser = Parser::new(&lex_result);
    let (parse_events, parse_errors) = parser.parse();
    let sink = Sink::new(&lex_result, parse_events, parse_errors);
    sink.finish()
}

/// Result of the parser
pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl Parse {
    /// Split the parse result into a syntax tree root node and
    /// the list of parse errors
    #[must_use]
    pub fn split(self) -> (SyntaxNode, Vec<ParseError>) {
        let root = SyntaxNode::new_root(self.green_node.clone());

        (root, self.errors)
    }

    #[must_use]
    pub fn debug_parse(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let mut s = debug_tree(&syntax_node);

        for error in &self.errors {
            let _ = write!(s, "\n{error}");
        }

        s
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Parser<'source> {
    source: Source<'source>,
    event_collection: EventCollection,
    parse_errors: Vec<ParseError>,
}

impl<'source> Parser<'source> {
    pub(crate) fn new(tokens: &'source [Token<'source>]) -> Self {
        Self {
            source: Source::new(tokens),
            event_collection: EventCollection::new(),
            parse_errors: vec![],
        }
    }

    fn parse(mut self) -> (EventCollection, Vec<ParseError>) {
        root(&mut self);
        (self.event_collection, self.parse_errors)
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token<'_>> {
        self.source.peek_token()
    }

    /// Lookahead is expensive!
    /// This lookahead doesn't skip further trivia tokens and is only there for combining the next n lexer tokens!
    /// for n of zero use `peek_token` instead!
    pub(crate) fn peek_nth_token(&mut self, n: usize) -> Option<&Token<'_>> {
        self.source.peek_nth_token(n)
    }

    pub(crate) fn get_pos(&self) -> usize {
        self.source.get_pos()
    }

    pub(crate) fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().is_some_and(|k| set.contains(&k))
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_following(&mut self, set: &[SyntaxKind]) -> bool {
        self.source.at_following(set)
    }

    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_following_content(&mut self, set: &[(SyntaxKind, Option<&str>)]) -> bool {
        self.source.at_following_content(set)
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    #[track_caller]
    pub(crate) fn bump(&mut self) -> &Token<'_> {
        let consumed = self
            .source
            .next_token()
            .expect("bump called, but there are no more tokens!");

        self.event_collection.add_token(consumed.kind);

        consumed
    }

    /// In most cases trivia like whitespace comes before any Tokens in a Node
    /// But sometimes it is necessary to consume the trivia even after the last Token in a Node.
    ///
    /// This does exactly that and can be used to consume trailing trivia in a string parser
    /// (where trivia should be inside as part of the string). Just call this before a call to parser.complete(...).
    pub(crate) fn explicitly_consume_trivia(&mut self) {
        self.event_collection.explicitly_consume_trivia();
    }

    #[track_caller]
    pub(crate) fn bump_as(&mut self, kind: SyntaxKind) -> Token<'_> {
        let consumed = self
            .source
            .next_token()
            .expect("bump called, but there are no more tokens!");

        self.event_collection.add_token(kind);

        Token {
            kind,
            text: consumed.text,
            range: consumed.range,
        }
    }

    #[track_caller]
    pub(crate) fn bump_next_n_as(&mut self, n: usize, kind: SyntaxKind) -> Vec<&Token<'_>> {
        let consumed = self.source.next_n_tokens(n);
        assert_eq!(
            consumed.len(),
            n,
            "bump_next_n_as called, but there are not enough tokens!"
        );

        self.event_collection.add_next_n_tokens_as(n, kind);

        consumed
    }

    pub(crate) fn expect(
        &mut self,
        kind: SyntaxKind,
        recovery_set: &[SyntaxKind],
    ) -> Option<&Token<'_>> {
        if self.at(kind) {
            Some(self.bump())
        } else {
            self.add_error(ParseErrorBuilder::new(format!("{kind}")));
            self.recover_expect(Some(kind), recovery_set)
        }
    }

    /// Recovers the parser after an error was found.
    /// It looks for either any token in the `GENERAL_RECOVERY_SET` or the
    /// provided `recovery_set` and wraps any tokens in between inside an error node.
    ///
    /// Important: in most cases this should not be called inside `parse_many`because it may
    /// consume future children.
    pub(crate) fn recover(&mut self, recovery_set: &[SyntaxKind]) {
        self.recover_expect(None, recovery_set);
    }

    fn recover_expect(
        &mut self,
        expected_kind: Option<SyntaxKind>,
        recovery_set: &[SyntaxKind],
    ) -> Option<&Token<'_>> {
        if self.at_end() || self.at_set(GENERAL_RECOVERY_SET) || self.at_set(recovery_set) {
            return None;
        }

        let error_m = self.start();
        loop {
            self.bump();

            if let Some(expected_kind) = expected_kind {
                if self.at(expected_kind) {
                    self.complete(error_m, SyntaxKind::ERROR);
                    return Some(self.bump());
                }
            }

            if self.at_end() || self.at_set(GENERAL_RECOVERY_SET) || self.at_set(recovery_set) {
                self.complete(error_m, SyntaxKind::ERROR);
                return None;
            }
        }
    }

    /// Adds a parser error but does not bump any tokens into the tree.
    pub(crate) fn add_error(&mut self, mut error_builder: ParseErrorBuilder) {
        // add missing information to builder
        if error_builder.range.is_none() || error_builder.found.is_none() {
            let current_token = self.source.peek_token();
            let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
                (Some(*kind), *range)
            } else {
                // If we're at the end of the input we use the range of the very last token
                // unwrap is fine, because error should not be called on empty file
                (
                    None,
                    self.source
                        .last_token_range()
                        .expect("parser error called on empty file which has no last token"),
                )
            };

            if error_builder.range.is_none() {
                error_builder.range = Some(range);
            }

            if error_builder.found.is_none() {
                error_builder.found = found;
            }
        }

        self.parse_errors.push(error_builder.build());
    }

    pub(crate) fn start(&mut self) -> Marker {
        self.event_collection.start()
    }

    #[track_caller]
    pub(crate) fn complete(&mut self, marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        self.event_collection.complete(marker, kind)
    }

    #[track_caller]
    pub(crate) fn precede(&mut self, completed_marker: CompletedMarker) -> Marker {
        self.event_collection.precede(completed_marker)
    }
}

#[cfg(test)]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn check_parse(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_parse());
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn parse_nothing() {
        check_parse("", expect!["ROOT@0..0"]);
    }
}

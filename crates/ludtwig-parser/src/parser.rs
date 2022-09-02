pub(crate) mod event;
mod parse_error;
mod sink;
mod source;

pub(crate) use parse_error::ParseError;
use rowan::GreenNode;
use std::mem;

use crate::grammar::root;
use crate::lexer::Token;
use crate::parser::event::{CompletedMarker, EventCollection, Marker};
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
use crate::{lex, T};

static RECOVERY_SET: &[SyntaxKind] = &[
    T!["{%"],
    T!["{{"],
    T!["%}"],
    T!["}}"],
    T!["<"],
    T!["</"],
    T![">"],
    T!["/>"],
];

pub fn parse(input_text: &str) -> Parse {
    let lex_result = lex(input_text);
    // println!("lexer result:\n{:#?}", lex_result);
    // println!("lexer result length: {}", lex_result.len());

    let parser = Parser::new(&lex_result);
    let parse_events = parser.parse();
    // println!("parse events:\n{:#?}", parse_events);

    let sink = Sink::new(&lex_result, parse_events);
    let parse = sink.finish();

    // TODO: remove debug print statements
    println!("{}", parse.debug_parse());

    parse
}

/// Result of the parser
pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
    /// returns false if the parse has not consumed all the tokens!
    /// That means the syntax tree doesn't represent the complete original file!
    pub finished: bool,
}

impl Parse {
    pub fn debug_parse(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let mut s = debug_tree(syntax_node);

        s.push_str(&*format!(
            "\nparsing consumed all tokens: {}",
            self.finished
        ));

        for error in &self.errors {
            s.push_str(&format!("\n{}", error));
        }

        s
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Parser<'source> {
    source: Source<'source>,
    event_collection: EventCollection,
    expected_kinds: Vec<SyntaxKind>,
}

impl<'source> Parser<'source> {
    fn new(tokens: &'source [Token<'source>]) -> Self {
        Self {
            source: Source::new(tokens),
            event_collection: EventCollection::new(),
            expected_kinds: vec![],
        }
    }

    fn parse(mut self) -> EventCollection {
        root(&mut self);
        self.event_collection
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    /// Look at the nth (zero indexed, zero means next, one is the one after that, ...) [SyntaxToken]
    /// but only if it matches the correct [SyntaxKind].
    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_nth_token(&mut self, kind: SyntaxKind, nth: usize) -> Option<&Token> {
        self.expected_kinds.push(kind);
        let peek = self.source.peek_nth_token(nth);
        if let Some(Token { kind: found, .. }) = peek {
            if *found == kind {
                return peek;
            }
        }

        None
    }

    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_following(&mut self, set: &[SyntaxKind]) -> bool {
        for kind in set {
            self.expected_kinds.push(*kind);
        }

        self.source.at_following(set)
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn bump(&mut self) -> &Token {
        self.expected_kinds.clear();
        let consumed = self
            .source
            .next_token()
            .expect("bump called, but there are no more tokens!");

        self.event_collection.add_token();

        consumed
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> Option<&Token> {
        if self.at(kind) {
            Some(self.bump())
        } else {
            self.error();
            None
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.source.peek_token();
        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we're at the end of the input we use the range of the very last token
            // unwrap is fine, because error should not be called on empty file
            (None, self.source.last_token_range().unwrap())
        };

        self.event_collection.add_error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        });

        if !self.at_set(RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            self.complete(m, SyntaxKind::ERROR);
        }
    }

    pub(crate) fn start(&mut self) -> Marker {
        self.event_collection.start()
    }

    #[track_caller]
    pub(crate) fn complete(&mut self, marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        marker.complete(&mut self.event_collection, kind)
    }

    #[track_caller]
    pub(crate) fn precede(&mut self, completed_marker: CompletedMarker) -> Marker {
        completed_marker.precede(&mut self.event_collection)
    }
}

#[cfg(test)]
pub(crate) fn check_parse(input: &str, expected_tree: expect_test::Expect) {
    use crate::syntax::untyped::TextLen;

    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_parse());
    assert!(parse.finished, "unparsed source tokens left");
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check_parse("", expect!["ROOT@0..0\nparsing consumed all tokens: true"]);
    }
}

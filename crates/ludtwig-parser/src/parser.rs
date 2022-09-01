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

static RECOVERY_SET: &[SyntaxKind] = &[T!["{%"] /*T!["{{"], T!["<"], T!["</"] */];

pub fn parse(input_text: &str) -> Parse {
    let lex_result = lex(input_text);
    // println!("lexer result:\n{:#?}", lex_result);
    // println!("lexer result length: {}", lex_result.len());

    let parser = Parser::new(&lex_result);
    let parse_events = parser.parse();
    // println!("parse events:\n{:#?}", parse_events);

    let sink = Sink::new(&lex_result, parse_events);
    let parse = sink.finish();

    println!("parsing errors: {:#?}", parse.errors);
    // TODO: remove debug print statements
    // println!("{}", debug_tree(syntax_tree.clone()));

    //syntax_tree
    parse
}

/// Result of the parser
pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
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

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source
            .next_token()
            .expect("bump called, but there are no more tokens!");

        self.event_collection.add_token();
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
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
}

#[cfg(test)]
pub(crate) fn check_tree(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    let root = SyntaxNode::new_root(parse.green_node);
    expected_tree.assert_eq(&crate::syntax::untyped::debug_tree(root));
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check_tree("", expect!["ROOT@0..0"]);
    }
}

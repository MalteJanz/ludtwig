use std::fmt::Write;
use std::mem;

use rowan::GreenNode;

pub use parse_error::ParseError;

use crate::grammar::root;
use crate::lexer::Token;
use crate::parser::event::{CompletedMarker, EventCollection, Marker};
use crate::parser::parse_error::{Expectation, ExpectationChain};
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
use crate::{lex, T};

pub(crate) mod event;
mod parse_error;
mod sink;
mod source;

pub(crate) static RECOVERY_SET: &[SyntaxKind] = &[
    T!["{%"],
    T!["%}"],
    T!["<"],
    T!["</"],
    T![">"],
    T!["/>"],
    T!["{{"],
    T!["}}"],
    T!["{#"],
    T!["#}"],
];

pub fn parse(input_text: &str) -> Parse {
    let lex_result = lex(input_text);
    let parser = Parser::new(&lex_result);
    let parse_events = parser.parse();
    let sink = Sink::new(&lex_result, parse_events);
    sink.finish()
}

/// Result of the parser
pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<ParseError>,
}

impl Parse {
    pub fn debug_parse(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let mut s = debug_tree(&syntax_node);

        for error in &self.errors {
            let _ = write!(s, "\n{}", error);
        }

        s
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Parser<'source> {
    source: Source<'source>,
    event_collection: EventCollection,
    expectation_chains: Vec<ExpectationChain>,
    pub(crate) capture_expectations: bool,
}

impl<'source> Parser<'source> {
    pub(crate) fn new(tokens: &'source [Token<'source>]) -> Self {
        Self {
            source: Source::new(tokens),
            event_collection: EventCollection::new(),
            expectation_chains: vec![],
            capture_expectations: true,
        }
    }

    fn parse(mut self) -> EventCollection {
        root(&mut self);
        self.event_collection
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn get_pos(&self) -> usize {
        self.source.get_pos()
    }

    pub(crate) fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        if self.capture_expectations && kind != SyntaxKind::TK_UNKNOWN {
            // unknown tokens should not be expected user input (not visible in the error message)
            self.expectation_chains
                .push(ExpectationChain::from(Expectation::from(kind)));
        }
        self.peek() == Some(kind)
    }

    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_following(&mut self, set: &[SyntaxKind]) -> bool {
        if self.capture_expectations {
            let mut expectation_chain = ExpectationChain::new(vec![]);
            for kind in set {
                if *kind != SyntaxKind::TK_UNKNOWN {
                    // unknown tokens should not be expected user input (not visible in the error message)
                    expectation_chain.chain.push(Expectation::from(*kind));
                }
            }
            self.expectation_chains.push(expectation_chain);
        }

        self.source.at_following(set)
    }

    /// Only use this if absolutely necessary, because it is expensive to lookahead!
    pub(crate) fn at_following_content(&mut self, set: &[(SyntaxKind, Option<&str>)]) -> bool {
        if self.capture_expectations {
            let mut expectation_chain = ExpectationChain::new(vec![]);
            for (kind, content) in set {
                if *kind != SyntaxKind::TK_UNKNOWN {
                    // unknown tokens should not be expected user input (not visible in the error message)
                    expectation_chain
                        .chain
                        .push(Expectation::new(*kind, content.map(|s| s.to_owned())));
                }
            }
            self.expectation_chains.push(expectation_chain);
        }

        self.source.at_following_content(set)
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    #[track_caller]
    pub(crate) fn bump(&mut self) -> &Token {
        self.expectation_chains.clear();
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
            (
                None,
                self.source
                    .last_token_range()
                    .expect("parser error called on empty file which has no last token"),
            )
        };

        self.event_collection.add_error(ParseError {
            expected: mem::take(&mut self.expectation_chains),
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
        self.event_collection.complete(marker, kind)
    }

    #[track_caller]
    pub(crate) fn precede(&mut self, completed_marker: CompletedMarker) -> Marker {
        self.event_collection.precede(completed_marker)
    }
}

#[cfg(test)]
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

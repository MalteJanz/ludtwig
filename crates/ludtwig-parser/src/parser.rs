mod event;
mod marker;
mod sink;
mod source;
mod twig;

use crate::lex;
use crate::lexer::Token;
use crate::parser::event::Event;
use crate::parser::marker::Marker;
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::parser::twig::parse_twig_block;
use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
use crate::T;

pub fn parse(input_text: &str) -> SyntaxNode {
    let lex_result = lex(input_text);
    // println!("lexer result:\n{:#?}", lex_result);
    // println!("lexer result length: {}", lex_result.len());

    let parser = Parser::new(&lex_result);
    let parse_events = parser.parse();
    // println!("parse events:\n{:#?}", parse_events);

    let sink = Sink::new(&lex_result, parse_events);
    let syntax_tree = SyntaxNode::new_root(sink.finish());

    // TODO: remove debug print statements
    println!("{}", debug_tree(syntax_tree.clone()));

    syntax_tree
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Parser<'source> {
    source: Source<'source>,
    events: Vec<Event>,
}

impl<'source> Parser<'source> {
    fn new(tokens: &'source [Token<'source>]) -> Self {
        Self {
            source: Source::new(tokens),
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event> {
        let root_marker = self.start();
        parse_item(&mut self);
        root_marker.complete(&mut self, SyntaxKind::ROOT);
        self.events
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
    }

    fn bump(&mut self) {
        self.source
            .next_token()
            .expect("bump called, but there are no more tokens!");

        self.events.push(Event::AddToken);
    }

    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        Marker::new(pos)
    }
}

fn parse_item(parser: &mut Parser) {
    while let Some(kind) = parser.peek() {
        match kind {
            T!["{%"] => parse_twig_block(parser),
            _ => {
                // TODO: implement more parts with proper parsing
                parser.bump(); // consume everything as tokens for testing purposes
            }
        }
    }
}

#[cfg(test)]
fn check_tree(input: &str, expected_tree: expect_test::Expect) {
    let tree = parse(input);
    expected_tree.assert_eq(&crate::syntax::untyped::debug_tree(tree));
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

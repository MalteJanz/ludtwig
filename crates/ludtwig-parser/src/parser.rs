mod event;
mod sink;
mod source;
mod twig;

use crate::lex;
use crate::lexer::Lexeme;
use crate::parser::event::Event;
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::parser::twig::parse_twig_block;
use crate::syntax::untyped::{print_syntax_tree, SyntaxKind, SyntaxNode};
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
    print_syntax_tree(0, syntax_tree.clone().into());

    syntax_tree
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Parser<'source> {
    source: Source<'source>,
    events: Vec<Event<'source>>,
}

impl<'source> Parser<'source> {
    fn new(lexemes: &'source [Lexeme<'source>]) -> Self {
        Self {
            source: Source::new(lexemes),
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event<'source>> {
        self.start_node(SyntaxKind::ROOT);
        parse_item(&mut self);
        self.finish_node();
        self.events
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    fn bump(&mut self) {
        let Lexeme { kind, text } = self
            .source
            .next_lexeme()
            .expect("bump called, but there are no more tokens!");

        self.events.push(Event::AddToken { kind: *kind, text });
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn checkpoint(&self) -> usize {
        self.events.len()
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

pub fn debug_tree(syntax_node: SyntaxNode) -> String {
    let formatted = format!("{:#?}", syntax_node);
    // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
    formatted[0..formatted.len() - 1].to_string()
}

#[cfg(test)]
fn check_tree(input: &str, expected_tree: expect_test::Expect) {
    let tree = parse(input);
    expected_tree.assert_eq(&debug_tree(tree));
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

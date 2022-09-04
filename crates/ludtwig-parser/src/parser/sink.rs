use crate::lexer::Token;
use crate::parser::event::{Event, EventCollection};
use crate::parser::{Parse, ParseError};
use crate::syntax::untyped::{GreenNodeBuilder, Language, TemplateLanguage};
use std::mem;

/// Sink for all the generated Events by the parser which
/// the sink can transform into the syntax tree.
#[derive(Debug)]
pub(super) struct Sink<'source> {
    tokens: &'source [Token<'source>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParseError>,
    builder: GreenNodeBuilder<'source>,
}

impl<'source> Sink<'source> {
    pub(super) fn new(
        tokens: &'source [Token<'source>],
        event_collection: EventCollection,
    ) -> Self {
        Self {
            tokens,
            cursor: 0,
            events: event_collection.into_event_list(),
            errors: vec![],
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(super) fn finish(mut self) -> Parse {
        let mut forward_kinds = Vec::new();

        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    // For events[A, B, C], B is A's forward_parent, C is B's forward_parent,
                    // in the normal control flow, the parent-child relation: `A -> B -> C`,
                    // while with the magic forward_parent, it writes: `C <- B <- A`.
                    forward_kinds.push(kind);

                    // Walk through the forward parent of the forward parent and so on
                    // and collect their SyntaxKinds
                    let mut idx = idx;
                    let mut forward_parent = forward_parent;
                    while let Some(fp) = forward_parent {
                        idx += fp;

                        forward_parent = if let Event::StartNode {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[idx], Event::Placeholder)
                        {
                            forward_kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!()
                        };
                    }

                    // create the start nodes (in reverse order, because forward parents come first)
                    for kind in forward_kinds.drain(..).rev() {
                        self.builder.start_node(TemplateLanguage::kind_to_raw(kind));
                    }
                }
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Error(error) => self.errors.push(error),
                Event::Placeholder => {}
            }

            self.consume_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
            finished: self.cursor == self.tokens.len(),
        }
    }

    fn consume_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            // automatically create a token for this whitespace
            // (the parser works on Source and doesn't see whitespace)
            self.token();
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder
            .token(TemplateLanguage::kind_to_raw(kind), text);
        self.cursor += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
    use crate::T;
    use expect_test::expect;

    #[test]
    fn green_node_builder_lifetime() {
        let green = {
            let source = String::from("hellohello");
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            // these tokens should be passed to rowans interner and cached
            builder.token(SyntaxKind::TK_WORD.into(), &source[..5]);
            builder.token(SyntaxKind::TK_WORD.into(), &source[5..]);
            builder.finish_node();

            drop(source); // original source code gets dropped -> rowan should have copied the required memory
            builder.finish()
        };

        let tree = SyntaxNode::new_root(green);
        assert_eq!(tree.text(), "hellohello");
    }

    #[test]
    fn sink_eat_whitespace() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "hello"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![word], "world"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
        ];
        let mut event_collection = EventCollection::new();
        let m = event_collection.start();
        event_collection.add_token();
        event_collection.add_token();
        event_collection.complete(m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection);
        let parse = sink.finish();
        let tree = SyntaxNode::new_root(parse.green_node);

        let expected = expect![[r#"
            ROOT@0..20
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              TK_WORD@3..8 "hello"
              TK_LINE_BREAK@8..9 "\n"
              TK_LINE_BREAK@9..10 "\n"
              TK_WHITESPACE@10..12 "  "
              TK_WORD@12..17 "world"
              TK_LINE_BREAK@17..18 "\n"
              TK_WHITESPACE@18..20 "  ""#]];

        // check that no lexing input (whitespace) got lost
        expected.assert_eq(&debug_tree(tree));
    }

    #[test]
    fn sink_non_reported_token_by_parser() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "hello"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![word], "world"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
        ];
        let mut event_collection = EventCollection::new();
        let m = event_collection.start();
        event_collection.add_token();
        // One token missing here
        event_collection.complete(m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection);
        let parse = sink.finish();
        let tree = SyntaxNode::new_root(parse.green_node);

        let expected = expect![[r#"
            ROOT@0..12
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              TK_WORD@3..8 "hello"
              TK_LINE_BREAK@8..9 "\n"
              TK_LINE_BREAK@9..10 "\n"
              TK_WHITESPACE@10..12 "  ""#]];

        // check that no lexing input (whitespace) got lost
        expected.assert_eq(&debug_tree(tree));
    }

    #[test]
    fn sink_forward_parent_handling() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "hello"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![word], "world"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
        ];
        let mut event_collection = EventCollection::new();
        let outer_m = event_collection.start();
        event_collection.add_token();

        let inner_m = event_collection.start();
        event_collection.add_token();
        let inner_completed = event_collection.complete(inner_m, SyntaxKind::HTML_STRING);
        let inner_wrapper_m = event_collection.precede(inner_completed);
        let inner_wrapper_completed = event_collection.complete(inner_wrapper_m, SyntaxKind::BODY);
        let outer_wrapper_m = event_collection.precede(inner_wrapper_completed);

        event_collection.complete(outer_wrapper_m, SyntaxKind::ERROR);
        event_collection.complete(outer_m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection);
        let parse = sink.finish();
        let tree = SyntaxNode::new_root(parse.green_node);

        let expected = expect![[r#"
            ROOT@0..20
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              TK_WORD@3..8 "hello"
              TK_LINE_BREAK@8..9 "\n"
              TK_LINE_BREAK@9..10 "\n"
              TK_WHITESPACE@10..12 "  "
              ERROR@12..20
                BODY@12..20
                  HTML_STRING@12..20
                    TK_WORD@12..17 "world"
                    TK_LINE_BREAK@17..18 "\n"
                    TK_WHITESPACE@18..20 "  ""#]];

        // check that no lexing input (whitespace) got lost
        expected.assert_eq(&debug_tree(tree));
    }
}

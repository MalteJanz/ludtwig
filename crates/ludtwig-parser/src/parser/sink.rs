use std::mem;

use crate::lexer::Token;
use crate::parser::event::{Event, EventCollection};
use crate::parser::{Parse, ParseError};
use crate::syntax::untyped::{GreenNodeBuilder, Language, SyntaxKind, TemplateLanguage};

/// Sink for all the generated Events by the parser which
/// the sink can transform into the syntax tree.
#[derive(Debug)]
pub(super) struct Sink<'source> {
    tokens: &'source [Token<'source>],
    cursor: usize,
    events: Vec<Event>,
    parser_errors: Vec<ParseError>,
    builder: GreenNodeBuilder<'source>,
}

impl<'source> Sink<'source> {
    pub(super) fn new(
        tokens: &'source [Token<'source>],
        event_collection: EventCollection,
        parser_errors: Vec<ParseError>,
    ) -> Self {
        Self {
            tokens,
            cursor: 0,
            events: event_collection.into_event_list(),
            parser_errors,
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(super) fn finish(mut self) -> Parse {
        let mut forward_kinds = Vec::new();

        for idx in 0..self.events.len() {
            if matches!(
                self.events[idx],
                Event::AddToken { .. } | Event::AddNextNTokensAs { .. }
            ) || idx == self.events.len() - 1
            {
                // consume trivia before any token event or the last event
                self.consume_trivia();
            }

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
                Event::AddToken { kind } => self.token_as(kind),
                Event::AddNextNTokensAs { n, kind } => self.next_n_tokens_as(n, kind),
                Event::ExplicitlyConsumeTrivia => self.consume_trivia(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Placeholder => {}
            }
        }

        assert_eq!(
            self.cursor,
            self.tokens.len(),
            "Parser did not consume all tokens! This is a error in the parsing logic!"
        );

        Parse {
            green_node: self.builder.finish(),
            errors: self.parser_errors,
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

    /// add the token with it's original kind that came from the lexer
    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder
            .token(TemplateLanguage::kind_to_raw(kind), text);
        self.cursor += 1;
    }

    /// add the token with another kind that the parser has specified
    fn token_as(&mut self, kind: SyntaxKind) {
        let Token { text, .. } = self.tokens[self.cursor];

        self.builder
            .token(TemplateLanguage::kind_to_raw(kind), text);
        self.cursor += 1;
    }

    fn next_n_tokens_as(&mut self, n: usize, kind: SyntaxKind) {
        let combined_string: String = self.tokens[self.cursor..(self.cursor + n)]
            .iter()
            .map(|t| t.text)
            .collect();

        self.builder
            .token(TemplateLanguage::kind_to_raw(kind), &combined_string);
        self.cursor += n;
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::syntax::untyped::{debug_tree, SyntaxKind, SyntaxNode};
    use crate::T;

    use super::*;

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
        event_collection.add_token(SyntaxKind::TK_WORD);
        event_collection.add_token(SyntaxKind::TK_WORD);
        event_collection.complete(m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection, vec![]);
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
        expected.assert_eq(&debug_tree(&tree));
    }

    #[test]
    #[should_panic(expected = "Parser did not consume all tokens!")]
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
        event_collection.add_token(SyntaxKind::TK_WORD);
        // One token missing here
        event_collection.complete(m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection, vec![]);
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
        expected.assert_eq(&debug_tree(&tree));
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
        event_collection.add_token(SyntaxKind::TK_WORD);

        let inner_m = event_collection.start();
        event_collection.add_token(SyntaxKind::TK_WORD);
        let inner_completed = event_collection.complete(inner_m, SyntaxKind::HTML_STRING);
        let inner_wrapper_m = event_collection.precede(inner_completed);
        let inner_wrapper_completed = event_collection.complete(inner_wrapper_m, SyntaxKind::BODY);
        let outer_wrapper_m = event_collection.precede(inner_wrapper_completed);

        event_collection.complete(outer_wrapper_m, SyntaxKind::ERROR);
        event_collection.complete(outer_m, SyntaxKind::ROOT);

        let sink = Sink::new(&tokens, event_collection, vec![]);
        let parse = sink.finish();
        let tree = SyntaxNode::new_root(parse.green_node);

        let expected = expect![[r#"
            ROOT@0..20
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              TK_WORD@3..8 "hello"
              ERROR@8..17
                BODY@8..17
                  HTML_STRING@8..17
                    TK_LINE_BREAK@8..9 "\n"
                    TK_LINE_BREAK@9..10 "\n"
                    TK_WHITESPACE@10..12 "  "
                    TK_WORD@12..17 "world"
              TK_LINE_BREAK@17..18 "\n"
              TK_WHITESPACE@18..20 "  ""#]];

        // check that no lexing input (whitespace) got lost
        expected.assert_eq(&debug_tree(&tree));
    }
}

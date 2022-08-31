use crate::lexer::Lexeme;
use crate::parser::event::Event;
use crate::syntax::untyped::{GreenNode, GreenNodeBuilder, Language, SyntaxKind, TemplateLanguage};
use crate::T;

/// Sink for all the generated Events by the parser which
/// the sink can transform into the syntax tree.
#[derive(Debug)]
pub(super) struct Sink<'source> {
    lexemes: &'source [Lexeme<'source>],
    cursor: usize,
    events: Vec<Event<'source>>,
    builder: GreenNodeBuilder<'source>,
}

impl<'source> Sink<'source> {
    pub(super) fn new(lexemes: &'source [Lexeme<'source>], events: Vec<Event<'source>>) -> Self {
        Self {
            lexemes,
            cursor: 0,
            events,
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        let mut reordered_events = self.events.clone();

        for (idx, event) in self.events.iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(idx);
                reordered_events.insert(*checkpoint, Event::StartNode { kind: *kind });
            }
        }

        for event in reordered_events {
            match event {
                Event::StartNode { kind } => {
                    self.builder.start_node(TemplateLanguage::kind_to_raw(kind))
                }
                Event::AddToken { kind, text } => self.token(kind, text),
                Event::FinishNode => self.builder.finish_node(),
                Event::StartNodeAt { .. } => unreachable!(),
            }

            self.consume_whitespace();
        }

        self.builder.finish()
    }

    fn consume_whitespace(&mut self) {
        while let Some(lexeme) = self.lexemes.get(self.cursor) {
            if !matches!(lexeme.kind, T![ws] | T![lb]) {
                break;
            }

            // automatically create a token for this whitespace
            // (the parser works on Source and doesn't see whitespace)
            self.token(lexeme.kind, lexeme.text);
        }
    }

    fn token(&mut self, kind: SyntaxKind, text: &'source str) {
        self.builder
            .token(TemplateLanguage::kind_to_raw(kind), text);
        self.cursor += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::debug_tree;
    use crate::syntax::untyped::{SyntaxKind, SyntaxNode};
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
        let lexemes = vec![
            Lexeme::new(T![ws], "  "),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![word], "word"),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![ws], "  "),
        ];
        let events = vec![
            Event::StartNode {
                kind: SyntaxKind::ROOT,
            },
            Event::AddToken {
                kind: T![word],
                text: "word",
            },
            Event::FinishNode,
        ];

        let sink = Sink::new(&lexemes, events);
        let green = sink.finish();

        // check that no lexing input (whitespace) got lost
        assert_eq!(green.to_string(), "  \nword\n  ");
    }

    #[test]
    fn sink_simple_checkpoint() {
        let lexemes = vec![
            Lexeme::new(T![ws], "  "),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![word], "hello"),
            Lexeme::new(T![word], "word"),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![ws], "  "),
        ];
        let events = vec![
            Event::StartNode {
                kind: SyntaxKind::ROOT,
            },
            Event::AddToken {
                kind: T![word],
                text: "hello",
            },
            Event::AddToken {
                kind: T![word],
                text: "word",
            },
            Event::StartNodeAt {
                kind: SyntaxKind::ERROR,
                checkpoint: 2, // inner checkpoint first
            },
            Event::StartNodeAt {
                kind: SyntaxKind::BODY,
                checkpoint: 1, // then outer
            },
            Event::FinishNode,
            Event::FinishNode,
            Event::FinishNode,
        ];

        let sink = Sink::new(&lexemes, events);
        let green = sink.finish();
        let tree = SyntaxNode::new_root(green);

        let expected = expect![[r#"
            ROOT@0..15
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              BODY@3..15
                TK_WORD@3..8 "hello"
                ERROR@8..15
                  TK_WORD@8..12 "word"
                  TK_LINE_BREAK@12..13 "\n"
                  TK_WHITESPACE@13..15 "  ""#]];

        expected.assert_eq(&debug_tree(tree));
    }

    #[test]
    fn sink_reversed_checkpoint() {
        let lexemes = vec![
            Lexeme::new(T![ws], "  "),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![word], "hello"),
            Lexeme::new(T![word], "word"),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![ws], "  "),
        ];
        let events = vec![
            Event::StartNode {
                kind: SyntaxKind::ROOT,
            },
            Event::AddToken {
                kind: T![word],
                text: "hello",
            },
            Event::AddToken {
                kind: T![word],
                text: "word",
            },
            Event::StartNodeAt {
                kind: SyntaxKind::BODY,
                checkpoint: 1, // this time outer first
            },
            Event::StartNodeAt {
                kind: SyntaxKind::ERROR,
                checkpoint: 2, // then inner checkpoint
            },
            Event::FinishNode,
            Event::FinishNode,
            Event::FinishNode,
        ];

        let sink = Sink::new(&lexemes, events);
        let green = sink.finish();
        let tree = SyntaxNode::new_root(green);

        let expected = expect![[r#"
            ROOT@0..15
              TK_WHITESPACE@0..2 "  "
              TK_LINE_BREAK@2..3 "\n"
              BODY@3..15
                TK_WORD@3..8 "hello"
                ERROR@8..15
                  TK_WORD@8..12 "word"
                  TK_LINE_BREAK@12..13 "\n"
                  TK_WHITESPACE@13..15 "  ""#]];

        expected.assert_eq(&debug_tree(tree));
    }
}

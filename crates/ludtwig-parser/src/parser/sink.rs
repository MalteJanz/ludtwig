use crate::lexer::Token;
use crate::parser::event::Event;
use crate::syntax::untyped::{GreenNode, GreenNodeBuilder, Language, TemplateLanguage};

/// Sink for all the generated Events by the parser which
/// the sink can transform into the syntax tree.
#[derive(Debug)]
pub(super) struct Sink<'source> {
    tokens: &'source [Token<'source>],
    cursor: usize,
    events: Vec<Event>,
    builder: GreenNodeBuilder<'source>,
}

impl<'source> Sink<'source> {
    pub(super) fn new(tokens: &'source [Token<'source>], events: Vec<Event>) -> Self {
        Self {
            tokens,
            cursor: 0,
            events,
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        for idx in 0..self.events.len() {
            let event = &self.events[idx];

            match event {
                Event::StartNode { kind } => self
                    .builder
                    .start_node(TemplateLanguage::kind_to_raw(*kind)),
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Placeholder => unreachable!(),
            }

            self.consume_trivia();
        }

        self.builder.finish()
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
        let Token { kind, text } = self.tokens[self.cursor];

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
            Token::new(T![ws], "  "),
            Token::new(T![lb], "\n"),
            Token::new(T![word], "hello"),
            Token::new(T![lb], "\n"),
            Token::new(T![lb], "\n"),
            Token::new(T![ws], "  "),
            Token::new(T![word], "world"),
            Token::new(T![lb], "\n"),
            Token::new(T![ws], "  "),
        ];
        let events = vec![
            Event::StartNode {
                kind: SyntaxKind::ROOT,
            },
            Event::AddToken,
            Event::AddToken,
            Event::FinishNode,
        ];

        let sink = Sink::new(&tokens, events);
        let green = sink.finish();
        let tree = SyntaxNode::new_root(green);

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
            Token::new(T![ws], "  "),
            Token::new(T![lb], "\n"),
            Token::new(T![word], "hello"),
            Token::new(T![lb], "\n"),
            Token::new(T![lb], "\n"),
            Token::new(T![ws], "  "),
            Token::new(T![word], "world"),
            Token::new(T![lb], "\n"),
            Token::new(T![ws], "  "),
        ];
        let events = vec![
            Event::StartNode {
                kind: SyntaxKind::ROOT,
            },
            Event::AddToken,
            // One token missing here
            Event::FinishNode,
        ];

        let sink = Sink::new(&tokens, events);
        let green = sink.finish();
        let tree = SyntaxNode::new_root(green);

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
}

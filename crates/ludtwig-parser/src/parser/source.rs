use crate::lexer::Token;
use crate::syntax::untyped::SyntaxKind;

/// Wrapper around lexing tokens to only get the non-whitespace tokens back
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) struct Source<'source> {
    tokens: &'source [Token<'source>],
    cursor: usize,
}

impl<'source> Source<'source> {
    pub(super) fn new(tokens: &'source [Token<'source>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(super) fn next_token(&mut self) -> Option<&'source Token<'source>> {
        self.eat_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    pub(super) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, SyntaxKind::is_trivia)
    }

    fn peek_kind_raw(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.cursor).map(|Token { kind, .. }| *kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::T;

    #[test]
    fn source_skip_whitespace() {
        let tokens = vec![
            Token::new(T![ws], "  "),
            Token::new(T![lb], "\n"),
            Token::new(T![word], "word"),
            Token::new(T![lb], "\n"),
            Token::new(T![ws], "  "),
        ];

        let mut source = Source::new(&tokens);
        assert_eq!(source.peek_kind(), Some(T![word]));
        assert_eq!(source.next_token(), Some(&Token::new(T![word], "word")));
        assert_eq!(source.peek_kind(), None);
        assert_eq!(source.next_token(), None);
    }
}

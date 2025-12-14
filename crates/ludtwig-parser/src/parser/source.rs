use rowan::TextRange;

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

    pub(super) fn next_n_tokens(&mut self, n: usize) -> Vec<&'source Token<'source>> {
        self.eat_trivia();

        let token = self.tokens[self.cursor..].iter().take(n).collect();
        self.cursor += n;

        token
    }

    pub(super) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    pub(super) fn peek_token(&mut self) -> Option<&Token<'_>> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    /// Lookahead is expensive!
    /// This lookahead doesn't skip further trivia tokens and is only there for combining the next n lexer tokens!
    /// for n of zero use `peek_token` instead!
    pub(super) fn peek_nth_token(&mut self, n: usize) -> Option<&Token<'_>> {
        self.eat_trivia();
        self.tokens[self.cursor..].get(n)
    }

    pub(super) fn at_following(&mut self, set: &[SyntaxKind]) -> bool {
        self.eat_trivia();
        if self.cursor == self.tokens.len() {
            return false; // end already reached
        }

        let mut tokens_iter = self.tokens[self.cursor..]
            .iter()
            .map(|t| t.kind)
            .filter(|k| !k.is_trivia());
        let mut set_iter = set.iter();

        loop {
            match (tokens_iter.next(), set_iter.next()) {
                (Some(token), Some(set)) if token == *set => continue,
                (None | Some(_), None) => return true,
                _ => return false,
            }
        }
    }

    pub(super) fn at_following_content(&mut self, set: &[(SyntaxKind, Option<&str>)]) -> bool {
        self.eat_trivia();
        if self.cursor == self.tokens.len() {
            return false; // end already reached
        }

        let mut tokens_iter = self.tokens[self.cursor..]
            .iter()
            .filter(|t| !t.kind.is_trivia());
        let mut set_iter = set.iter();

        loop {
            match (tokens_iter.next(), set_iter.next()) {
                (Some(token), Some((set_kind, set_content)))
                    if token.kind == *set_kind
                        && set_content.is_none_or(|content| content == token.text) =>
                {
                    continue;
                }
                (None | Some(_), None) => return true,
                _ => return false,
            }
        }
    }

    pub(super) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    pub(super) fn get_pos(&self) -> usize {
        self.cursor
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().is_some_and(SyntaxKind::is_trivia)
    }

    fn peek_kind_raw(&self) -> Option<SyntaxKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token<'_>> {
        self.tokens.get(self.cursor)
    }
}

#[cfg(test)]
mod tests {
    use crate::T;

    use super::*;

    #[test]
    fn source_skip_whitespace() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "word"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
        ];

        let mut source = Source::new(&tokens);
        assert_eq!(source.peek_kind(), Some(T![word]));
        assert_eq!(
            source.next_token(),
            Some(&Token::new_wrong_range(T![word], "word"))
        );
        assert_eq!(source.peek_kind(), None);
        assert_eq!(source.next_token(), None);
    }

    #[test]
    fn source_at_following() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "hello"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T!["<"], "<"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![">"], ">"),
            Token::new_wrong_range(T![ws], "  "),
        ];

        let mut source = Source::new(&tokens);
        assert!(source.at_following(&[T![word], T!["<"], T![">"]]));
        assert!(source.at_following(&[T![word], T!["<"]]));
        assert!(source.at_following(&[T![word]]));

        assert!(!source.at_following(&[T![word], T!["<"], T![">"], T![word]]));
        assert!(!source.at_following(&[T!["<"]]));
        assert!(!source.at_following(&[T![word], T![">"]]));

        source.next_token();
        source.next_token();
        source.next_token();

        // nothing more to compare
        assert!(!source.at_following(&[T![word]]));
    }

    #[test]
    fn source_at_following_content() {
        let tokens = vec![
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![word], "hello"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![ws], "  "),
            Token::new_wrong_range(T!["<"], "<"),
            Token::new_wrong_range(T![lb], "\n"),
            Token::new_wrong_range(T![">"], ">"),
            Token::new_wrong_range(T![ws], "  "),
        ];

        let mut source = Source::new(&tokens);
        assert!(source.at_following_content(&[
            (T![word], Some("hello")),
            (T!["<"], None),
            (T![">"], None)
        ]));
        assert!(source.at_following_content(&[(T![word], None), (T!["<"], None), (T![">"], None)]));
        assert!(source.at_following_content(&[(T![word], Some("hello")), (T!["<"], None)]));
        assert!(source.at_following_content(&[(T![word], Some("hello"))]));
        assert!(source.at_following_content(&[
            (T![word], None),
            (T!["<"], None),
            (T![">"], Some(">"))
        ]));

        assert!(!source.at_following_content(&[(T![word], Some("nonExistent"))]));
        assert!(!source.at_following_content(&[
            (T![word], Some("nonExistent")),
            (T!["<"], None),
            (T![">"], None)
        ]));

        source.next_token();
        source.next_token();
        source.next_token();

        // nothing more to compare
        assert!(!source.at_following_content(&[(T![word], None)]));
    }
}

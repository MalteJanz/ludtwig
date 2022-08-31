use crate::lexer::Lexeme;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

/// Wrapper around lexemes (lexing tokens) to only get the non-whitespace tokens back
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) struct Source<'source> {
    lexemes: &'source [Lexeme<'source>],
    cursor: usize,
}

impl<'source> Source<'source> {
    pub(super) fn new(lexemes: &'source [Lexeme<'source>]) -> Self {
        Self { lexemes, cursor: 0 }
    }

    pub(super) fn next_lexeme(&mut self) -> Option<&'source Lexeme<'source>> {
        self.eat_whitespace();

        let lexeme = self.lexemes.get(self.cursor)?;
        self.cursor += 1;

        Some(lexeme)
    }

    pub(super) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.eat_whitespace();
        self.peek_kind_raw()
    }

    fn eat_whitespace(&mut self) {
        while matches!(self.peek_kind_raw(), Some(T![ws] | T![lb])) {
            self.cursor += 1;
        }
    }

    fn peek_kind_raw(&self) -> Option<SyntaxKind> {
        self.lexemes
            .get(self.cursor)
            .map(|Lexeme { kind, .. }| *kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_skip_whitespace() {
        let lexemes = vec![
            Lexeme::new(T![ws], "  "),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![word], "word"),
            Lexeme::new(T![lb], "\n"),
            Lexeme::new(T![ws], "  "),
        ];

        let mut source = Source::new(&lexemes);
        assert_eq!(source.peek_kind(), Some(T![word]));
        assert_eq!(source.next_lexeme(), Some(&Lexeme::new(T![word], "word")));
        assert_eq!(source.peek_kind(), None);
        assert_eq!(source.next_lexeme(), None);
    }
}

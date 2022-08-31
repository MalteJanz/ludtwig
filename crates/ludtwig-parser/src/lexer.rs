use crate::syntax::untyped::SyntaxKind;
use logos::Logos;

/// Lex the source code into a Vec of tokens with their corresponding span (position in source code).
/// These tokens are produced by a dumb lexer and don't have any meaning / semantic attached to them.
pub(crate) fn lex(source: &str) -> Vec<Lexeme> {
    let mut lexer = SyntaxKind::lexer(source);
    let mut result = vec![];

    while let Some(kind) = lexer.next() {
        result.push(Lexeme {
            kind,
            text: lexer.slice(),
        })
    }

    result
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Lexeme<'source> {
    pub(crate) kind: SyntaxKind,
    pub(crate) text: &'source str,
}

impl<'source> Lexeme<'source> {
    pub(crate) fn new(kind: SyntaxKind, text: &'source str) -> Self {
        Self { kind, text }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::T;

    fn check(input: &str, kind: SyntaxKind) {
        let lexer_results = lex(input);

        // compare lex result
        assert_eq!(lexer_results, vec![Lexeme { kind, text: input }]);
    }

    #[test]
    fn test_lex_simple_output() {
        let results = lex("</div>");

        assert_eq!(
            results,
            vec![
                Lexeme::new(T!["</"], "</"),
                Lexeme::new(T![word], "div"),
                Lexeme::new(T![">"], ">")
            ]
        );
    }

    #[test]
    fn test_lex_whitespace() {
        check("   ", T![ws]);
        check(" \t  ", T![ws]);
        check("\t", T![ws]);
    }

    #[test]
    fn test_lex_line_break() {
        check("\n", T![lb]);
        check("\r\n", T![lb]);
    }

    #[test]
    fn test_lex_word() {
        check("hello", T![word]);
        check("hello123", T![word]);
        check("camelCase", T![word]);
        check("kebab-case", T![word]);
        check("snake_case", T![word]);
        check(":hello123", T![word]);
        check("#hello123", T![word]);
        check("@hello123", T![word]);
        check("block1", T![word]);
        check("block_", T![word]);
        check("blocks", T![word]);
    }

    #[test]
    fn test_lex_less_than() {
        check("<", T!["<"]);
    }

    #[test]
    fn test_lex_less_than_slash() {
        check("</", T!["</"]);
    }

    #[test]
    fn test_lex_greater_than() {
        check(">", T![">"]);
    }

    #[test]
    fn test_lex_slash_greater_than() {
        check("/>", T!["/>"]);
    }

    #[test]
    fn test_lex_equal() {
        check("=", T!["="]);
    }

    #[test]
    fn test_lex_double_quotes() {
        check("\"", T!["\""]);
    }

    #[test]
    fn test_lex_curly_percent() {
        check("{%", T!["{%"]);
    }

    #[test]
    fn test_lex_percent_curly() {
        check("%}", T!["%}"]);
    }

    #[test]
    fn test_lex_open_curly_curly() {
        check("{{", T!["{{"]);
    }

    #[test]
    fn test_lex_close_curly_curly() {
        check("}}", T!["}}"]);
    }

    #[test]
    fn test_lex_block() {
        check("block", T!["block"]);
    }

    #[test]
    fn test_lex_endblock() {
        check("endblock", T!["endblock"]);
    }
}

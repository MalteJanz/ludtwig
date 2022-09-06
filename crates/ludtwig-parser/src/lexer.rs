use crate::syntax::untyped::{SyntaxKind, TextRange, TextSize};
use logos::Logos;

/// Lex the source code into a Vec of tokens with their corresponding span (position in source code).
/// These tokens are produced by a dumb lexer and don't have any meaning / semantic attached to them.
pub(crate) fn lex(source: &str) -> Vec<Token> {
    let mut lexer = SyntaxKind::lexer(source);
    let mut result = vec![];

    while let Some(kind) = lexer.next() {
        let range = {
            let span = lexer.span();
            let start = TextSize::try_from(span.start).unwrap();
            let end = TextSize::try_from(span.end).unwrap();
            TextRange::new(start, end)
        };

        result.push(Token {
            kind,
            text: lexer.slice(),
            range,
        })
    }

    result
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Token<'source> {
    pub(crate) kind: SyntaxKind,
    pub(crate) text: &'source str,
    pub(crate) range: TextRange,
}

impl<'source> Token<'source> {
    #[cfg(test)]
    pub(crate) fn new(kind: SyntaxKind, text: &'source str, range: TextRange) -> Self {
        Self { kind, text, range }
    }

    #[cfg(test)]
    pub(crate) fn new_wrong_range(kind: SyntaxKind, text: &'source str) -> Self {
        use crate::syntax::untyped::TextLen;
        let range = TextRange::up_to(text.text_len());

        Self { kind, text, range }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::untyped::TextLen;
    use crate::T;

    fn check(input: &str, kind: SyntaxKind) {
        let range = TextRange::up_to(input.text_len());
        let lexer_results = lex(input);

        // compare lex result
        assert_eq!(
            lexer_results,
            vec![Token {
                kind,
                text: input,
                range
            }]
        );
    }

    #[test]
    fn lex_simple_output() {
        let results = lex("</div>");

        assert_eq!(
            results,
            vec![
                Token::new(T!["</"], "</", TextRange::up_to("</".text_len())),
                Token::new(
                    T![word],
                    "div",
                    TextRange::at(TextSize::from(2), "div".text_len())
                ),
                Token::new(
                    T![">"],
                    ">",
                    TextRange::at(TextSize::from(5), ">".text_len())
                )
            ]
        );
    }

    #[test]
    fn lex_whitespace() {
        check("   ", T![ws]);
        check(" \t  ", T![ws]);
        check("\t", T![ws]);
    }

    #[test]
    fn lex_line_break() {
        check("\n", T![lb]);
        check("\r\n", T![lb]);
    }

    #[test]
    fn lex_word() {
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
    fn lex_less_than() {
        check("<", T!["<"]);
    }

    #[test]
    fn lex_less_than_slash() {
        check("</", T!["</"]);
    }

    #[test]
    fn lex_greater_than() {
        check(">", T![">"]);
    }

    #[test]
    fn lex_slash_greater_than() {
        check("/>", T!["/>"]);
    }

    #[test]
    fn lex_less_than_exclamation_mark_minus_minus() {
        check("<!--", T!["<!--"]);
    }

    #[test]
    fn lex_minus_minus_greater_than() {
        check("-->", T!["-->"]);
    }

    #[test]
    fn lex_equal() {
        check("=", T!["="]);
    }

    #[test]
    fn lex_double_quotes() {
        check("\"", T!["\""]);
    }

    #[test]
    fn lex_single_quotes() {
        check("'", T!["'"]);
    }

    #[test]
    fn lex_curly_percent() {
        check("{%", T!["{%"]);
    }

    #[test]
    fn lex_percent_curly() {
        check("%}", T!["%}"]);
    }

    #[test]
    fn lex_open_curly_curly() {
        check("{{", T!["{{"]);
    }

    #[test]
    fn lex_close_curly_curly() {
        check("}}", T!["}}"]);
    }

    #[test]
    fn lex_open_curly_hashtag() {
        check("{#", T!["{#"]);
    }

    #[test]
    fn lex_hashtag_close_curly() {
        check("#}", T!["#}"]);
    }

    #[test]
    fn lex_block() {
        check("block", T!["block"]);
    }

    #[test]
    fn lex_endblock() {
        check("endblock", T!["endblock"]);
    }

    #[test]
    fn lex_if() {
        check("if", T!["if"]);
    }

    #[test]
    fn lex_else_if() {
        check("elseif", T!["elseif"]);
    }

    #[test]
    fn lex_else() {
        check("else", T!["else"]);
    }

    #[test]
    fn lex_endif() {
        check("endif", T!["endif"]);
    }
}

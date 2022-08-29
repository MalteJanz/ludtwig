use logos::{Lexer, Logos, Span};

/// Outer lexer for template documents.
/// Needs to switch to [TokenTwig] lexer when encountering a twig token
/// (can be done manually by `.morph`).
#[derive(Debug, Clone, Eq, PartialEq, Hash, Logos)]
enum TokenTemplate {
    #[regex(r"[ \t]+")]
    Whitespace,
    #[regex(r"[\n\r\f]+")]
    LineBreak,
    #[regex("[a-zA-Z]+")]
    Text,
    #[token("{%")]
    TwigBlockStart,
    #[token("{{")]
    TwigVarStart,
    /// Some unspecified pattern
    #[error]
    Error,
}

/// Inner lexer for inside twig brackets.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Logos)]
enum TokenTwig {
    #[regex(r"[ \t]+")]
    Whitespace,
    #[regex(r"[\n\r\f]+")]
    LineBreak,
    #[regex("[a-zA-Z0-9_]+")]
    Name,
    #[token("block")]
    KwBlock,
    #[token("endblock")]
    KwEndBlock,
    #[token("%}")]
    TwigBlockEnd,
    #[token("}}")]
    TwigVarEnd,
    /// Some unspecified pattern
    #[error]
    Error,
}

enum Modes<'source> {
    Template(Lexer<'source, TokenTemplate>),
    Twig(Lexer<'source, TokenTwig>),
}

impl<'source> Modes<'source> {
    fn new(s: &'source str) -> Self {
        Self::Template(TokenTemplate::lexer(s))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Tokens {
    TemplateToken(TokenTemplate),
    TwigToken(TokenTwig),
}

struct ModeBridge<'source> {
    mode: Modes<'source>,
}

// Clones as we switch between modes
impl<'source> Iterator for ModeBridge<'source> {
    type Item = (Tokens, Span);

    fn next(&mut self) -> Option<Self::Item> {
        use Tokens::*;
        match &mut self.mode {
            Modes::Template(lexer) => {
                let result = lexer.next();
                let span = lexer.span();
                match result {
                    Some(TokenTemplate::TwigBlockStart) | Some(TokenTemplate::TwigVarStart) => {
                        self.mode = Modes::Twig(lexer.to_owned().morph());
                    }
                    _ => {}
                }
                result.map(TemplateToken).map(|t| (t, span))
            }
            Modes::Twig(lexer) => {
                let result = lexer.next();
                let span = lexer.span();
                match result {
                    Some(TokenTwig::TwigBlockEnd) | Some(TokenTwig::TwigVarEnd) => {
                        self.mode = Modes::Template(lexer.to_owned().morph());
                    }
                    _ => {}
                }
                result.map(TwigToken).map(|t| (t, span))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let s = "Create ridiculously {% block my_block %} fast {% endblock %} Lexers.\n\tNext line";
        let lex = ModeBridge {
            mode: Modes::new(s),
        };

        let results: Vec<_> = lex.collect();

        println!("{:#?}", results);
    }
}

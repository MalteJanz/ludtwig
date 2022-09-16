use crate::grammar::html::parse_any_html;
use crate::grammar::twig::parse_any_twig;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;

mod html;
mod twig;

/// Type used to pass concrete fn (function pointers) around that are parsing functions
type ParseFunction = fn(&mut Parser) -> Option<CompletedMarker>;

pub(super) fn root(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();

    parse_many(
        parser,
        |_| false,
        |p| {
            if parse_any_element(p).is_none() && !p.at_end() {
                // not parsable element encountered

                // at least consume unparseable input TODO: maybe throw parser error?!
                // call to parser.error() could result in infinite loop here!
                let error_m = p.start();
                p.bump();
                p.complete(error_m, SyntaxKind::ERROR);
            }
        },
    );

    parser.complete(m, SyntaxKind::ROOT)
}

fn parse_many<E, P>(parser: &mut Parser, mut early_exit_closure: E, mut child_parser: P)
where
    E: FnMut(&mut Parser) -> bool,
    P: FnMut(&mut Parser),
{
    loop {
        // capture parser token position to prevent infinite loops!
        let parser_pos = parser.get_pos();

        if parser.at(SyntaxKind::ERROR) {
            // TODO: is this really the right way to handle invalid lexing tokens?
            // at least consume unparseable input TODO: maybe throw parser error?!
            // call to parser.error() could result in infinite loop here!
            parser.bump(); // allow / ignore errors tokens
        } else if parser.at_end() || early_exit_closure(parser) || {
            child_parser(parser);
            parser.get_pos() == parser_pos
        } {
            break;
        }
    }
}

fn parse_any_element(parser: &mut Parser) -> Option<CompletedMarker> {
    parse_any_twig(parser, parse_any_element).or_else(|| parse_any_html(parser))
}

#[cfg(test)]
mod tests {
    use crate::grammar::parse_many;
    use crate::lex;
    use expect_test::expect;

    use crate::parser::{check_parse, Parser};
    use crate::syntax::untyped::SyntaxKind;

    #[test]
    fn parse_synthetic_minimal() {
        check_parse(
            "{% block my-block %}
    <div claSs=\"my-div\">
        world
    </div>
{% endblock %}",
            expect![[r#"
                ROOT@0..85
                  TWIG_BLOCK@0..85
                    TWIG_STARTING_BLOCK@0..20
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..17 "my-block"
                      TK_WHITESPACE@17..18 " "
                      TK_PERCENT_CURLY@18..20 "%}"
                    BODY@20..70
                      HTML_TAG@20..70
                        HTML_STARTING_TAG@20..45
                          TK_LINE_BREAK@20..21 "\n"
                          TK_WHITESPACE@21..25 "    "
                          TK_LESS_THAN@25..26 "<"
                          TK_WORD@26..29 "div"
                          HTML_ATTRIBUTE@29..44
                            TK_WHITESPACE@29..30 " "
                            TK_WORD@30..35 "claSs"
                            TK_EQUAL@35..36 "="
                            HTML_STRING@36..44
                              TK_DOUBLE_QUOTES@36..37 "\""
                              TK_WORD@37..43 "my-div"
                              TK_DOUBLE_QUOTES@43..44 "\""
                          TK_GREATER_THAN@44..45 ">"
                        BODY@45..59
                          HTML_TEXT@45..59
                            TK_LINE_BREAK@45..46 "\n"
                            TK_WHITESPACE@46..54 "        "
                            TK_WORD@54..59 "world"
                        HTML_ENDING_TAG@59..70
                          TK_LINE_BREAK@59..60 "\n"
                          TK_WHITESPACE@60..64 "    "
                          TK_LESS_THAN_SLASH@64..66 "</"
                          TK_WORD@66..69 "div"
                          TK_GREATER_THAN@69..70 ">"
                    TWIG_ENDING_BLOCK@70..85
                      TK_LINE_BREAK@70..71 "\n"
                      TK_CURLY_PERCENT@71..73 "{%"
                      TK_WHITESPACE@73..74 " "
                      TK_ENDBLOCK@74..82 "endblock"
                      TK_WHITESPACE@82..83 " "
                      TK_PERCENT_CURLY@83..85 "%}""#]],
        );
    }

    #[test]
    fn parse_many_should_have_no_infinite_loop() {
        let lex_result = lex("a b c");
        let mut parser = Parser::new(&lex_result);

        let before_pos = parser.get_pos();
        parse_many(
            &mut parser,
            |_| false,
            |p| {
                let m = p.start();
                // don't consume / call parser bump
                p.complete(m, SyntaxKind::ERROR);
            },
        );
        assert_eq!(before_pos, parser.get_pos());
    }
}

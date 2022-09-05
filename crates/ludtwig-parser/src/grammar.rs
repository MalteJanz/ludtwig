use crate::grammar::html::parse_any_html;
use crate::grammar::twig::parse_any_twig;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;

mod html;
mod twig;

pub(super) fn root(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();

    loop {
        if parse_any_element(parser).is_none() {
            if !parser.at_end() {
                // at least consume unparseable input TODO: maybe throw parser error?!
                // call to parser.error() could result in infinite loop here!
                let error_m = parser.start();
                parser.bump();
                parser.complete(error_m, SyntaxKind::ERROR);
            } else {
                break;
            }
        }
    }

    parser.complete(m, SyntaxKind::ROOT)
}

fn parse_any_element(parser: &mut Parser) -> Option<CompletedMarker> {
    parse_any_twig(parser).or_else(|| parse_any_html(parser))
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

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
                    TWIG_STARTING_BLOCK@0..25
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..17 "my-block"
                      TK_WHITESPACE@17..18 " "
                      TK_PERCENT_CURLY@18..20 "%}"
                      TK_LINE_BREAK@20..21 "\n"
                      TK_WHITESPACE@21..25 "    "
                    BODY@25..71
                      HTML_TAG@25..71
                        HTML_STARTING_TAG@25..54
                          TK_LESS_THAN@25..26 "<"
                          TK_WORD@26..29 "div"
                          TK_WHITESPACE@29..30 " "
                          HTML_ATTRIBUTE@30..44
                            TK_WORD@30..35 "claSs"
                            TK_EQUAL@35..36 "="
                            HTML_STRING@36..44
                              TK_DOUBLE_QUOTES@36..37 "\""
                              TK_WORD@37..43 "my-div"
                              TK_DOUBLE_QUOTES@43..44 "\""
                          TK_GREATER_THAN@44..45 ">"
                          TK_LINE_BREAK@45..46 "\n"
                          TK_WHITESPACE@46..54 "        "
                        BODY@54..64
                          HTML_TEXT@54..64
                            TK_WORD@54..59 "world"
                            TK_LINE_BREAK@59..60 "\n"
                            TK_WHITESPACE@60..64 "    "
                        HTML_ENDING_TAG@64..71
                          TK_LESS_THAN_SLASH@64..66 "</"
                          TK_WORD@66..69 "div"
                          TK_GREATER_THAN@69..70 ">"
                          TK_LINE_BREAK@70..71 "\n"
                    TWIG_ENDING_BLOCK@71..85
                      TK_CURLY_PERCENT@71..73 "{%"
                      TK_WHITESPACE@73..74 " "
                      TK_ENDBLOCK@74..82 "endblock"
                      TK_WHITESPACE@82..83 " "
                      TK_PERCENT_CURLY@83..85 "%}"
                parsing consumed all tokens: true"#]],
        );
    }
}

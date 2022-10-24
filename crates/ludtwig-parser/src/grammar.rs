use crate::grammar::html::parse_any_html;
use crate::grammar::twig::parse_any_twig;
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

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

                // at least consume unparseable input TODO: maybe handle this in a better way?
                p.add_error(ParseErrorBuilder::new("html, text or twig element"));
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

        if parser.at(SyntaxKind::TK_UNKNOWN) {
            // TODO: what to do with unkown token between elements?
            // parser.add_error(ParseErrorBuilder::new("not this unknown token"));
            // let error_m = parser.start();
            parser.bump(); // skip / ignore unknown tokens
                           // parser.complete(error_m, SyntaxKind::ERROR);
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

fn parse_ludtwig_directive(
    parser: &mut Parser,
    outer: Marker,
    closing_kind: SyntaxKind,
) -> CompletedMarker {
    debug_assert!(parser.at_set(&[T!["ludtwig-ignore-file"], T!["ludtwig-ignore"]]));
    let ignore_kind = if parser.at(T!["ludtwig-ignore-file"]) {
        SyntaxKind::LUDTWIG_DIRECTIVE_FILE_IGNORE
    } else {
        SyntaxKind::LUDTWIG_DIRECTIVE_IGNORE
    };
    parser.bump();

    let rule_list_m = parser.start();
    parse_many(
        parser,
        |p| p.at(closing_kind),
        |p| {
            p.expect(T![word]);
            if p.at(T![","]) {
                p.bump();
            }
        },
    );
    parser.complete(rule_list_m, SyntaxKind::LUDTWIG_DIRECTIVE_RULE_LIST);

    parser.expect(closing_kind);
    parser.complete(outer, ignore_kind)
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
                              HTML_STRING_INNER@37..43
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

    #[test]
    fn parse_twig_comment_ludtwig_directive_ignore_file() {
        check_parse(
            "{# ludtwig-ignore-file twig-block-line-breaks, twig-block-name-snake-case #}",
            expect![[r##"
                ROOT@0..76
                  LUDTWIG_DIRECTIVE_FILE_IGNORE@0..76
                    TK_OPEN_CURLY_HASHTAG@0..2 "{#"
                    TK_WHITESPACE@2..3 " "
                    TK_LUDTWIG_IGNORE_FILE@3..22 "ludtwig-ignore-file"
                    LUDTWIG_DIRECTIVE_RULE_LIST@22..73
                      TK_WHITESPACE@22..23 " "
                      TK_WORD@23..45 "twig-block-line-breaks"
                      TK_COMMA@45..46 ","
                      TK_WHITESPACE@46..47 " "
                      TK_WORD@47..73 "twig-block-name-snake ..."
                    TK_WHITESPACE@73..74 " "
                    TK_HASHTAG_CLOSE_CURLY@74..76 "#}""##]],
        )
    }

    #[test]
    fn parse_twig_comment_ludtwig_directive_ignore() {
        check_parse(
            "{# ludtwig-ignore twig-block-line-breaks, twig-block-name-snake-case #}",
            expect![[r##"
                ROOT@0..71
                  LUDTWIG_DIRECTIVE_IGNORE@0..71
                    TK_OPEN_CURLY_HASHTAG@0..2 "{#"
                    TK_WHITESPACE@2..3 " "
                    TK_LUDTWIG_IGNORE@3..17 "ludtwig-ignore"
                    LUDTWIG_DIRECTIVE_RULE_LIST@17..68
                      TK_WHITESPACE@17..18 " "
                      TK_WORD@18..40 "twig-block-line-breaks"
                      TK_COMMA@40..41 ","
                      TK_WHITESPACE@41..42 " "
                      TK_WORD@42..68 "twig-block-name-snake ..."
                    TK_WHITESPACE@68..69 " "
                    TK_HASHTAG_CLOSE_CURLY@69..71 "#}""##]],
        )
    }

    #[test]
    fn parse_html_comment_ludtwig_directive_ignore_file() {
        check_parse(
            "<!-- ludtwig-ignore-file twig-block-line-breaks, twig-block-name-snake-case -->",
            expect![[r#"
                ROOT@0..79
                  LUDTWIG_DIRECTIVE_FILE_IGNORE@0..79
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@0..4 "<!--"
                    TK_WHITESPACE@4..5 " "
                    TK_LUDTWIG_IGNORE_FILE@5..24 "ludtwig-ignore-file"
                    LUDTWIG_DIRECTIVE_RULE_LIST@24..75
                      TK_WHITESPACE@24..25 " "
                      TK_WORD@25..47 "twig-block-line-breaks"
                      TK_COMMA@47..48 ","
                      TK_WHITESPACE@48..49 " "
                      TK_WORD@49..75 "twig-block-name-snake ..."
                    TK_WHITESPACE@75..76 " "
                    TK_MINUS_MINUS_GREATER_THAN@76..79 "-->""#]],
        )
    }

    #[test]
    fn parse_html_comment_ludtwig_directive_ignore() {
        check_parse(
            "<!-- ludtwig-ignore twig-block-line-breaks, twig-block-name-snake-case -->",
            expect![[r#"
                ROOT@0..74
                  LUDTWIG_DIRECTIVE_IGNORE@0..74
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@0..4 "<!--"
                    TK_WHITESPACE@4..5 " "
                    TK_LUDTWIG_IGNORE@5..19 "ludtwig-ignore"
                    LUDTWIG_DIRECTIVE_RULE_LIST@19..70
                      TK_WHITESPACE@19..20 " "
                      TK_WORD@20..42 "twig-block-line-breaks"
                      TK_COMMA@42..43 ","
                      TK_WHITESPACE@43..44 " "
                      TK_WORD@44..70 "twig-block-name-snake ..."
                    TK_WHITESPACE@70..71 " "
                    TK_MINUS_MINUS_GREATER_THAN@71..74 "-->""#]],
        )
    }
}

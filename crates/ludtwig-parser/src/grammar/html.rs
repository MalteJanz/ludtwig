use crate::grammar::parse_any_element;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_html_text(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T![word]));
    let m = parser.start();
    parser.bump();

    while parser.at(T![word]) {
        parser.bump();
    }

    parser.complete(m, SyntaxKind::HTML_TEXT)
}

pub(super) fn parse_html_element(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<"]));
    let m = parser.start();

    // parse start tag
    let starting_tag_m = parser.start();
    parser.bump();
    let tag_name = parser.expect(T![word]).map_or("", |t| t.text);
    while parse_html_attribute(parser).is_some() {}
    parser.expect(T![">"]);
    parser.complete(starting_tag_m, SyntaxKind::HTML_STARTING_TAG);

    // parse all the children
    let body_m = parser.start();
    while !parser.at(T!["</"]) {
        if parse_any_element(parser).is_none() {
            break;
        }
    }
    parser.complete(body_m, SyntaxKind::BODY);

    // parse matching end tag TODO
    let end_tag_m = parser.start();
    parser.expect(T!["</"]);
    parser.expect(T![word]);
    parser.expect(T![">"]);
    parser.complete(end_tag_m, SyntaxKind::HTML_ENDING_TAG);

    parser.complete(m, SyntaxKind::HTML_TAG)
}

pub(super) fn parse_html_end_tag(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["</"]));
    let m = parser.start();
    parser.bump();

    parser.expect(T![word]);
    parser.expect(T![">"]);

    parser.complete(m, SyntaxKind::HTML_ENDING_TAG)
}

fn parse_html_attribute(parser: &mut Parser) -> Option<CompletedMarker> {
    if !parser.at(T![word]) {
        return None;
    }

    let m = parser.start();
    parser.bump();

    if parser.at(T!["="]) {
        // attribute value
        parser.bump();

        let str_m = parser.start();
        parser.expect(T!["\""]);
        // consume anything for now?
        while !parser.at(T!["\""]) {
            parser.bump();
        }
        parser.expect(T!["\""]);
        parser.complete(str_m, SyntaxKind::HTML_STRING);
    }

    Some(parser.complete(m, SyntaxKind::HTML_ATTRIBUTE))
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

    #[test]
    fn parse_simple_html_element() {
        check_parse(
            "<div></div>",
            expect![[r#"
                ROOT@0..11
                  HTML_TAG@0..11
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..5
                    HTML_ENDING_TAG@5..11
                      TK_LESS_THAN_SLASH@5..7 "</"
                      TK_WORD@7..10 "div"
                      TK_GREATER_THAN@10..11 ">"
                parsing consumed all tokens: true"#]],
        );
    }

    #[test]
    fn parse_html_element_with_attributes() {
        check_parse(
            "<div class=\"my-class1 my-class2\" style=\"color: blue;\"></div>",
            expect![[r#"
                ROOT@0..60
                  HTML_TAG@0..60
                    HTML_STARTING_TAG@0..54
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_WHITESPACE@4..5 " "
                      HTML_ATTRIBUTE@5..33
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..33
                          TK_DOUBLE_QUOTES@11..12 "\""
                          TK_WORD@12..21 "my-class1"
                          TK_WHITESPACE@21..22 " "
                          TK_WORD@22..31 "my-class2"
                          TK_DOUBLE_QUOTES@31..32 "\""
                          TK_WHITESPACE@32..33 " "
                      HTML_ATTRIBUTE@33..53
                        TK_WORD@33..38 "style"
                        TK_EQUAL@38..39 "="
                        HTML_STRING@39..53
                          TK_DOUBLE_QUOTES@39..40 "\""
                          TK_WORD@40..46 "color:"
                          TK_WHITESPACE@46..47 " "
                          TK_WORD@47..51 "blue"
                          ERROR@51..52 ";"
                          TK_DOUBLE_QUOTES@52..53 "\""
                      TK_GREATER_THAN@53..54 ">"
                    BODY@54..54
                    HTML_ENDING_TAG@54..60
                      TK_LESS_THAN_SLASH@54..56 "</"
                      TK_WORD@56..59 "div"
                      TK_GREATER_THAN@59..60 ">"
                parsing consumed all tokens: true"#]],
        );
    }

    #[test]
    fn parse_html_element_with_children() {
        check_parse(
            "<div>hello<span>world</span>!</div>",
            expect![[r#"
                ROOT@0..35
                  HTML_TAG@0..35
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..29
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..28
                        HTML_STARTING_TAG@10..16
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..15 "span"
                          TK_GREATER_THAN@15..16 ">"
                        BODY@16..21
                          HTML_TEXT@16..21
                            TK_WORD@16..21 "world"
                        HTML_ENDING_TAG@21..28
                          TK_LESS_THAN_SLASH@21..23 "</"
                          TK_WORD@23..27 "span"
                          TK_GREATER_THAN@27..28 ">"
                      HTML_TEXT@28..29
                        TK_WORD@28..29 "!"
                    HTML_ENDING_TAG@29..35
                      TK_LESS_THAN_SLASH@29..31 "</"
                      TK_WORD@31..34 "div"
                      TK_GREATER_THAN@34..35 ">"
                parsing consumed all tokens: true"#]],
        );
    }

    /*
    #[test]
    fn parse_html_element_with_children_missing_closing_tag() {
        check_parse(
            "<div>hello<span>world!</div>",
            expect![[r#"
                ROOT@0..5
                  HTML_TAG@0..5
                    TK_LESS_THAN@0..1 "<"
                    TK_WORD@1..4 "div"
                    TK_GREATER_THAN@4..5 ">""#]],
        );
    }
     */
}

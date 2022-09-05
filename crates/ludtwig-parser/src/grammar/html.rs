use crate::grammar::parse_any_element;
use crate::lexer::Token;
use crate::parser::event::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_any_html(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["<"]) {
        Some(parse_html_element(parser))
    } else if parser.at(T![word]) {
        Some(parse_html_text(parser))
    } else if parser.at(T!["<!--"]) {
        Some(parse_html_comment(parser))
    } else {
        None
    }
}

fn parse_html_text(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T![word]));
    let m = parser.start();
    parser.bump();

    while parser.at(T![word]) {
        parser.bump();
    }

    parser.complete(m, SyntaxKind::HTML_TEXT)
}

fn parse_html_comment(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<!--"]));
    let m = parser.start();
    parser.bump();

    loop {
        if parser.at_end() || parser.at(T!["-->"]) {
            break;
        }

        parser.bump();
    }

    parser.expect(T!["-->"]);
    parser.complete(m, SyntaxKind::HTML_COMMENT)
}

fn parse_html_element(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<"]));
    let m = parser.start();

    // parse start tag
    let starting_tag_m = parser.start();
    parser.bump();
    let tag_name = parser.expect(T![word]).map_or("", |t| t.text).to_owned();
    while parse_html_attribute(parser).is_some() {}
    let is_self_closing = if parser.at(T!["/>"]) {
        parser.bump();
        true
    } else {
        parser.expect(T![">"]);
        false
    };

    parser.complete(starting_tag_m, SyntaxKind::HTML_STARTING_TAG);

    if is_self_closing {
        return parser.complete(m, SyntaxKind::HTML_TAG);
    }

    // parse all the children
    let body_m = parser.start();
    let mut matching_end_tag_encountered = false;
    loop {
        if parser.at(T!["</"]) {
            if let Some(Token { kind, text, .. }) = parser.at_nth_token(T![word], 1) {
                if *kind == T![word] && *text == tag_name {
                    matching_end_tag_encountered = true;
                    break; // found matching closing tag
                }
            }
        }
        if parse_any_element(parser).is_none() {
            break;
        }
    }
    parser.complete(body_m, SyntaxKind::BODY);

    // parse matching end tag if exists
    if matching_end_tag_encountered {
        // found matching closing tag
        let end_tag_m = parser.start();
        parser.expect(T!["</"]);
        parser.expect(T![word]);
        parser.expect(T![">"]);
        parser.complete(end_tag_m, SyntaxKind::HTML_ENDING_TAG);
    } else {
        // no matching end tag found!
        parser.error();
    }

    parser.complete(m, SyntaxKind::HTML_TAG)
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
        while !parser.at(T!["\""]) && !parser.at(T![">"]) && !parser.at(T!["/>"]) {
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

    #[test]
    fn parse_html_element_with_multiple_children() {
        check_parse(
            "<div>\
                    hello<span>world</span>\
                    <p>paragraph</p>
                    <div>something</div>
                    </div>",
            expect![[r#"
                ROOT@0..112
                  HTML_TAG@0..112
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..106
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
                      HTML_TAG@28..65
                        HTML_STARTING_TAG@28..31
                          TK_LESS_THAN@28..29 "<"
                          TK_WORD@29..30 "p"
                          TK_GREATER_THAN@30..31 ">"
                        BODY@31..40
                          HTML_TEXT@31..40
                            TK_WORD@31..40 "paragraph"
                        HTML_ENDING_TAG@40..65
                          TK_LESS_THAN_SLASH@40..42 "</"
                          TK_WORD@42..43 "p"
                          TK_GREATER_THAN@43..44 ">"
                          TK_LINE_BREAK@44..45 "\n"
                          TK_WHITESPACE@45..65 "                    "
                      HTML_TAG@65..106
                        HTML_STARTING_TAG@65..70
                          TK_LESS_THAN@65..66 "<"
                          TK_WORD@66..69 "div"
                          TK_GREATER_THAN@69..70 ">"
                        BODY@70..79
                          HTML_TEXT@70..79
                            TK_WORD@70..79 "something"
                        HTML_ENDING_TAG@79..106
                          TK_LESS_THAN_SLASH@79..81 "</"
                          TK_WORD@81..84 "div"
                          TK_GREATER_THAN@84..85 ">"
                          TK_LINE_BREAK@85..86 "\n"
                          TK_WHITESPACE@86..106 "                    "
                    HTML_ENDING_TAG@106..112
                      TK_LESS_THAN_SLASH@106..108 "</"
                      TK_WORD@108..111 "div"
                      TK_GREATER_THAN@111..112 ">"
                parsing consumed all tokens: true"#]],
        );
    }

    #[test]
    fn parse_html_element_with_children_missing_closing_tag() {
        check_parse(
            "<div>hello<span>world!</div>",
            expect![[r#"
                ROOT@0..28
                  HTML_TAG@0..28
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..22
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..22
                        HTML_STARTING_TAG@10..16
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..15 "span"
                          TK_GREATER_THAN@15..16 ">"
                        BODY@16..22
                          HTML_TEXT@16..22
                            TK_WORD@16..22 "world!"
                    HTML_ENDING_TAG@22..28
                      TK_LESS_THAN_SLASH@22..24 "</"
                      TK_WORD@24..27 "div"
                      TK_GREATER_THAN@27..28 ">"
                parsing consumed all tokens: true
                error at 22..22: expected word, </, word, {%, {{, {#, <, word or <!--, but found </"#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_single_quotes() {
        check_parse(
            "<div claSs='my-div'>
        hello world
    </div>",
            expect![[r#"
                ROOT@0..51
                  HTML_TAG@0..51
                    HTML_STARTING_TAG@0..29
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_WHITESPACE@4..5 " "
                      HTML_ATTRIBUTE@5..19
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..19
                          ERROR@11..12
                            TK_SINGLE_QUOTES@11..12 "'"
                          TK_WORD@12..18 "my-div"
                          TK_SINGLE_QUOTES@18..19 "'"
                      TK_GREATER_THAN@19..20 ">"
                      TK_LINE_BREAK@20..21 "\n"
                      TK_WHITESPACE@21..29 "        "
                    BODY@29..45
                      HTML_TEXT@29..45
                        TK_WORD@29..34 "hello"
                        TK_WHITESPACE@34..35 " "
                        TK_WORD@35..40 "world"
                        TK_LINE_BREAK@40..41 "\n"
                        TK_WHITESPACE@41..45 "    "
                    HTML_ENDING_TAG@45..51
                      TK_LESS_THAN_SLASH@45..47 "</"
                      TK_WORD@47..50 "div"
                      TK_GREATER_THAN@50..51 ">"
                parsing consumed all tokens: true
                error at 11..11: expected ", but found '
                error at 19..19: expected ", > or ", but found >"#]],
        );
    }

    #[test]
    fn parse_html_comment() {
        check_parse(
            "<!-- this is a comment --> this not <!-- but this again -->",
            expect![[r#"
                ROOT@0..59
                  HTML_COMMENT@0..27
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@0..4 "<!--"
                    TK_WHITESPACE@4..5 " "
                    TK_WORD@5..9 "this"
                    TK_WHITESPACE@9..10 " "
                    TK_WORD@10..12 "is"
                    TK_WHITESPACE@12..13 " "
                    TK_WORD@13..14 "a"
                    TK_WHITESPACE@14..15 " "
                    TK_WORD@15..22 "comment"
                    TK_WHITESPACE@22..23 " "
                    TK_MINUS_MINUS_GREATER_THAN@23..26 "-->"
                    TK_WHITESPACE@26..27 " "
                  HTML_TEXT@27..36
                    TK_WORD@27..31 "this"
                    TK_WHITESPACE@31..32 " "
                    TK_WORD@32..35 "not"
                    TK_WHITESPACE@35..36 " "
                  HTML_COMMENT@36..59
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@36..40 "<!--"
                    TK_WHITESPACE@40..41 " "
                    TK_WORD@41..44 "but"
                    TK_WHITESPACE@44..45 " "
                    TK_WORD@45..49 "this"
                    TK_WHITESPACE@49..50 " "
                    TK_WORD@50..55 "again"
                    TK_WHITESPACE@55..56 " "
                    TK_MINUS_MINUS_GREATER_THAN@56..59 "-->"
                parsing consumed all tokens: true"#]],
        );
    }

    #[test]
    fn test_html_self_closing_tag() {
        check_parse(
            "<hr/>plain<img/>text<custom/>",
            expect![[r#"
            ROOT@0..29
              HTML_TAG@0..5
                HTML_STARTING_TAG@0..5
                  TK_LESS_THAN@0..1 "<"
                  TK_WORD@1..3 "hr"
                  TK_SLASH_GREATER_THAN@3..5 "/>"
              HTML_TEXT@5..10
                TK_WORD@5..10 "plain"
              HTML_TAG@10..16
                HTML_STARTING_TAG@10..16
                  TK_LESS_THAN@10..11 "<"
                  TK_WORD@11..14 "img"
                  TK_SLASH_GREATER_THAN@14..16 "/>"
              HTML_TEXT@16..20
                TK_WORD@16..20 "text"
              HTML_TAG@20..29
                HTML_STARTING_TAG@20..29
                  TK_LESS_THAN@20..21 "<"
                  TK_WORD@21..27 "custom"
                  TK_SLASH_GREATER_THAN@27..29 "/>"
            parsing consumed all tokens: true"#]],
        );
    }
}

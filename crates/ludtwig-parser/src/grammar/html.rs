use crate::grammar::twig::parse_any_twig;
use crate::grammar::{parse_any_element, parse_ludtwig_directive, parse_many, ParseFunction};
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser, RECOVERY_SET};
use crate::syntax::untyped::SyntaxKind;
use crate::T;
use once_cell::sync::Lazy;
use regex::Regex;

// Every token value that matches this regex is allowed for html attribute names
static HTML_NAME_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"^([a-zA-Z]|([:@\#_\$][a-zA-Z]))[a-zA-Z0-9_\-]*$"#).unwrap());

static HTML_VOID_ELEMENTS: &[&str] = &[
    "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link",
    "meta", "param", "source", "track", "wbr",
];

pub(super) fn parse_any_html(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["<"]) {
        Some(parse_html_element(parser))
    } else if parser.at(T!["<!--"]) {
        Some(parse_html_comment(parser))
    } else if parser.at(T!["<!"]) {
        Some(parse_html_doctype(parser))
    } else {
        parse_html_text(parser)
    }
}

fn parse_html_doctype(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<!"]));
    let m = parser.start();
    parser.bump();

    parser.expect(T!["DOCTYPE"]);
    parser.expect(T![word]);
    parser.expect(T![">"]);

    parser.complete(m, SyntaxKind::HTML_DOCTYPE)
}

fn parse_html_text(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at_end() || parser.at_set(RECOVERY_SET) {
        return None;
    }

    let m = parser.start();

    parse_many(
        parser,
        |p| p.at_set(RECOVERY_SET),
        |p| {
            p.bump();
        },
    );

    Some(parser.complete(m, SyntaxKind::HTML_TEXT))
}

fn parse_html_comment(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<!--"]));
    let m = parser.start();
    parser.bump();

    if parser.at_set(&[T!["ludtwig-ignore-file"], T!["ludtwig-ignore"]]) {
        parse_ludtwig_directive(parser, m, T!["-->"])
    } else {
        parse_plain_html_comment(parser, m)
    }
}

fn parse_plain_html_comment(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    parse_many(
        parser,
        |p| p.at(T!["-->"]),
        |p| {
            p.bump();
        },
    );

    parser.expect(T!["-->"]);
    parser.complete(outer, SyntaxKind::HTML_COMMENT)
}

fn parse_html_element(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<"]));
    let m = parser.start();

    // parse start tag
    let starting_tag_m = parser.start();
    parser.bump();
    let tag_name = parser.expect(T![word]).map_or("", |t| t.text).to_owned();
    // parse attributes (can include twig)
    parse_many(
        parser,
        |p| p.at(T![">"]) || p.at(T!["/>"]),
        |p| {
            parse_html_attribute_or_twig(p);
        },
    );
    // parse end of starting tag
    let mut is_self_closing = if parser.at(T!["/>"]) {
        parser.bump();
        true
    } else {
        parser.expect(T![">"]);
        false
    };

    if HTML_VOID_ELEMENTS.contains(&&*tag_name) {
        is_self_closing = true; // void elements never have children or an end tag
    }

    parser.complete(starting_tag_m, SyntaxKind::HTML_STARTING_TAG);

    // early return in case of self closing
    if is_self_closing {
        return parser.complete(m, SyntaxKind::HTML_TAG);
    }

    // parse all the children
    let body_m = parser.start();
    let mut matching_end_tag_encountered = false;

    parse_many(
        parser,
        |p| {
            if p.at_following_content(&[(T!["</"], None), (T![word], Some(&tag_name))]) {
                matching_end_tag_encountered = true;
                return true; // found matching closing tag
            }

            // TODO: needs special care for future endfor, endif, ...
            if p.at_following(&[T!["{%"], T!["endblock"]])
                || p.at_following(&[T!["{%"], T!["endif"]])
                || p.at_following(&[T!["{%"], T!["elseif"]])
                || p.at_following(&[T!["{%"], T!["else"]])
            {
                return true; // endblock in the wild may mean this tag has a missing closing tag
            }

            false
        },
        |p| {
            parse_any_element(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    // parse matching end tag or report missing (the tag itself is not self closing!)
    let end_tag_m = parser.start();
    if matching_end_tag_encountered {
        // found matching closing tag
        parser.expect(T!["</"]);
        parser.expect(T![word]);
        parser.expect(T![">"]);
    } else {
        // no matching end tag found!
        parser.add_error(ParseErrorBuilder::new(format!(
            "</{}> ending tag",
            tag_name
        )));
    }
    parser.complete(end_tag_m, SyntaxKind::HTML_ENDING_TAG);

    parser.complete(m, SyntaxKind::HTML_TAG)
}

fn parse_html_attribute_or_twig(parser: &mut Parser) -> Option<CompletedMarker> {
    let token_text = parser.peek_token()?.text;
    if !HTML_NAME_REGEX.is_match(token_text) {
        // parse any twig syntax where its children can only be html attributes (this parser)
        return parse_any_twig(parser, parse_html_attribute_or_twig);
    }

    // normal html attribute
    let m = parser.start();
    parser.bump_as(T![word]);

    if parser.at(T!["="]) {
        // attribute value
        parser.bump();
        parse_html_string_including_twig(parser);
    }

    Some(parser.complete(m, SyntaxKind::HTML_ATTRIBUTE))
}

fn parse_html_string_including_twig(parser: &mut Parser) -> CompletedMarker {
    let m = parser.start();
    let quote_kind = if parser.at_set(&[T!["\""], T!["'"]]) {
        let starting_quote_token = parser.bump();
        Some(starting_quote_token.kind)
    } else {
        // the HTML specification also allows no quotes but then
        // the value must only be a single word
        None
    };

    fn inner_double_quote_parser(parser: &mut Parser) -> Option<CompletedMarker> {
        parse_many(
            parser,
            |p| {
                if p.at(T!("\"")) {
                    return true;
                }

                child_early_return(p)
            },
            |p| child_parser(p, inner_double_quote_parser),
        );
        None
    }

    fn inner_single_quote_parser(parser: &mut Parser) -> Option<CompletedMarker> {
        parse_many(
            parser,
            |p| {
                if p.at(T!("'")) {
                    return true;
                }

                child_early_return(p)
            },
            |p| child_parser(p, inner_single_quote_parser),
        );
        None
    }

    fn inner_no_quote_parser(parser: &mut Parser) -> Option<CompletedMarker> {
        if parser.at(T![word]) {
            parser.bump();
        } else {
            parser.add_error(ParseErrorBuilder::new("html attribute value"))
        }

        None
    }

    fn child_early_return(p: &mut Parser) -> bool {
        // TODO: needs special care for future endfor, endif, ...
        if p.at_following(&[T!["{%"], T!["endblock"]])
            || p.at_following(&[T!["{%"], T!["elseif"]])
            || p.at_following(&[T!["{%"], T!["else"]])
            || p.at_following(&[T!["{%"], T!["endif"]])
        {
            return true;
        }

        false
    }

    fn child_parser(p: &mut Parser, inner_twig_child_parser: ParseFunction) {
        if parse_any_twig(p, inner_twig_child_parser).is_none() {
            if p.at_set(RECOVERY_SET) || p.at_end() {
                return;
            }

            p.bump();
        }
    }

    let inner_m = parser.start();
    // run the appropriate inner parser
    match quote_kind {
        Some(quote_kind) if quote_kind == T!["\""] => {
            inner_double_quote_parser(parser);
        }
        Some(quote_kind) if quote_kind == T!["'"] => {
            inner_single_quote_parser(parser);
        }
        None => {
            inner_no_quote_parser(parser);
        }
        Some(_) => unreachable!(),
    }
    parser.explicitly_consume_trivia(); // consume any trailing trivia to be inside the inner string
    parser.complete(inner_m, SyntaxKind::HTML_STRING_INNER);

    // expect the same closing quote if a starting quote existed
    if let Some(quote_kind) = quote_kind {
        parser.expect(quote_kind);
    } else {
        // check for unexpected quote which this parser still consumes to make missing leading quote errors simpler
        if parser.at_set(&[T!["\""], T!["'"]]) {
            let error_m = parser.start();
            let quote = parser.bump();
            let parser_err =
                ParseErrorBuilder::new("no trailing quote because there is no leading quote")
                    .at_token(quote);
            parser.add_error(parser_err);
            parser.complete(error_m, SyntaxKind::ERROR);
        }
    }

    parser.complete(m, SyntaxKind::HTML_STRING)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::parser::check_parse;

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
                      TK_GREATER_THAN@10..11 ">""#]],
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
                      HTML_ATTRIBUTE@4..32
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..32
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..31
                            TK_WORD@12..21 "my-class1"
                            TK_WHITESPACE@21..22 " "
                            TK_WORD@22..31 "my-class2"
                          TK_DOUBLE_QUOTES@31..32 "\""
                      HTML_ATTRIBUTE@32..53
                        TK_WHITESPACE@32..33 " "
                        TK_WORD@33..38 "style"
                        TK_EQUAL@38..39 "="
                        HTML_STRING@39..53
                          TK_DOUBLE_QUOTES@39..40 "\""
                          HTML_STRING_INNER@40..52
                            TK_WORD@40..45 "color"
                            TK_COLON@45..46 ":"
                            TK_WHITESPACE@46..47 " "
                            TK_WORD@47..51 "blue"
                            TK_SEMICOLON@51..52 ";"
                          TK_DOUBLE_QUOTES@52..53 "\""
                      TK_GREATER_THAN@53..54 ">"
                    BODY@54..54
                    HTML_ENDING_TAG@54..60
                      TK_LESS_THAN_SLASH@54..56 "</"
                      TK_WORD@56..59 "div"
                      TK_GREATER_THAN@59..60 ">""#]],
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
                        TK_EXCLAMATION_MARK@28..29 "!"
                    HTML_ENDING_TAG@29..35
                      TK_LESS_THAN_SLASH@29..31 "</"
                      TK_WORD@31..34 "div"
                      TK_GREATER_THAN@34..35 ">""#]],
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
                    BODY@5..85
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
                      HTML_TAG@28..44
                        HTML_STARTING_TAG@28..31
                          TK_LESS_THAN@28..29 "<"
                          TK_WORD@29..30 "p"
                          TK_GREATER_THAN@30..31 ">"
                        BODY@31..40
                          HTML_TEXT@31..40
                            TK_WORD@31..40 "paragraph"
                        HTML_ENDING_TAG@40..44
                          TK_LESS_THAN_SLASH@40..42 "</"
                          TK_WORD@42..43 "p"
                          TK_GREATER_THAN@43..44 ">"
                      HTML_TAG@44..85
                        HTML_STARTING_TAG@44..70
                          TK_LINE_BREAK@44..45 "\n"
                          TK_WHITESPACE@45..65 "                    "
                          TK_LESS_THAN@65..66 "<"
                          TK_WORD@66..69 "div"
                          TK_GREATER_THAN@69..70 ">"
                        BODY@70..79
                          HTML_TEXT@70..79
                            TK_WORD@70..79 "something"
                        HTML_ENDING_TAG@79..85
                          TK_LESS_THAN_SLASH@79..81 "</"
                          TK_WORD@81..84 "div"
                          TK_GREATER_THAN@84..85 ">"
                    HTML_ENDING_TAG@85..112
                      TK_LINE_BREAK@85..86 "\n"
                      TK_WHITESPACE@86..106 "                    "
                      TK_LESS_THAN_SLASH@106..108 "</"
                      TK_WORD@108..111 "div"
                      TK_GREATER_THAN@111..112 ">""#]],
        );
    }

    #[test]
    fn parse_html_element_with_children_self_closing() {
        check_parse(
            "<div>hello<hr/></div>",
            expect![[r#"
            ROOT@0..21
              HTML_TAG@0..21
                HTML_STARTING_TAG@0..5
                  TK_LESS_THAN@0..1 "<"
                  TK_WORD@1..4 "div"
                  TK_GREATER_THAN@4..5 ">"
                BODY@5..15
                  HTML_TEXT@5..10
                    TK_WORD@5..10 "hello"
                  HTML_TAG@10..15
                    HTML_STARTING_TAG@10..15
                      TK_LESS_THAN@10..11 "<"
                      TK_WORD@11..13 "hr"
                      TK_SLASH_GREATER_THAN@13..15 "/>"
                HTML_ENDING_TAG@15..21
                  TK_LESS_THAN_SLASH@15..17 "</"
                  TK_WORD@17..20 "div"
                  TK_GREATER_THAN@20..21 ">""#]],
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
                            TK_WORD@16..21 "world"
                            TK_EXCLAMATION_MARK@21..22 "!"
                        HTML_ENDING_TAG@22..22
                    HTML_ENDING_TAG@22..28
                      TK_LESS_THAN_SLASH@22..24 "</"
                      TK_WORD@24..27 "div"
                      TK_GREATER_THAN@27..28 ">"
                error at 22..24: expected </span> ending tag but found </"#]],
        );
    }

    #[test]
    fn parse_html_tag_missing_twig_endblock_in_children() {
        check_parse(
            "<div>{% block inner %} hello </div>",
            expect![[r#"
                ROOT@0..35
                  HTML_TAG@0..35
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..28
                      TWIG_BLOCK@5..28
                        TWIG_STARTING_BLOCK@5..22
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..19 "inner"
                          TK_WHITESPACE@19..20 " "
                          TK_PERCENT_CURLY@20..22 "%}"
                        BODY@22..28
                          HTML_TEXT@22..28
                            TK_WHITESPACE@22..23 " "
                            TK_WORD@23..28 "hello"
                        TWIG_ENDING_BLOCK@28..28
                    HTML_ENDING_TAG@28..35
                      TK_WHITESPACE@28..29 " "
                      TK_LESS_THAN_SLASH@29..31 "</"
                      TK_WORD@31..34 "div"
                      TK_GREATER_THAN@34..35 ">"
                error at 29..31: expected {% but found </
                error at 29..31: expected endblock but found </
                error at 29..31: expected %} but found </"#]],
        )
    }

    #[test]
    fn parse_html_tag_missing_twig_endblock_and_closing_tag_in_children() {
        check_parse(
            "<div>{% block inner %}<span>hello</div>",
            expect![[r#"
                ROOT@0..39
                  HTML_TAG@0..39
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..33
                      TWIG_BLOCK@5..33
                        TWIG_STARTING_BLOCK@5..22
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..19 "inner"
                          TK_WHITESPACE@19..20 " "
                          TK_PERCENT_CURLY@20..22 "%}"
                        BODY@22..33
                          HTML_TAG@22..33
                            HTML_STARTING_TAG@22..28
                              TK_LESS_THAN@22..23 "<"
                              TK_WORD@23..27 "span"
                              TK_GREATER_THAN@27..28 ">"
                            BODY@28..33
                              HTML_TEXT@28..33
                                TK_WORD@28..33 "hello"
                            HTML_ENDING_TAG@33..33
                        TWIG_ENDING_BLOCK@33..33
                    HTML_ENDING_TAG@33..39
                      TK_LESS_THAN_SLASH@33..35 "</"
                      TK_WORD@35..38 "div"
                      TK_GREATER_THAN@38..39 ">"
                error at 33..35: expected </span> ending tag but found </
                error at 33..35: expected {% but found </
                error at 33..35: expected endblock but found </
                error at 33..35: expected %} but found </"#]],
        )
    }

    #[test]
    fn parse_html_string_with_leading_and_trailing_trivia() {
        check_parse(
            "<div class=\" my-class \"></div>",
            expect![[r#"
            ROOT@0..30
              HTML_TAG@0..30
                HTML_STARTING_TAG@0..24
                  TK_LESS_THAN@0..1 "<"
                  TK_WORD@1..4 "div"
                  HTML_ATTRIBUTE@4..23
                    TK_WHITESPACE@4..5 " "
                    TK_WORD@5..10 "class"
                    TK_EQUAL@10..11 "="
                    HTML_STRING@11..23
                      TK_DOUBLE_QUOTES@11..12 "\""
                      HTML_STRING_INNER@12..22
                        TK_WHITESPACE@12..13 " "
                        TK_WORD@13..21 "my-class"
                        TK_WHITESPACE@21..22 " "
                      TK_DOUBLE_QUOTES@22..23 "\""
                  TK_GREATER_THAN@23..24 ">"
                BODY@24..24
                HTML_ENDING_TAG@24..30
                  TK_LESS_THAN_SLASH@24..26 "</"
                  TK_WORD@26..29 "div"
                  TK_GREATER_THAN@29..30 ">""#]],
        );
    }

    #[test]
    fn parse_html_string_with_twig_var() {
        check_parse(
            "<div class=\"hello {{ twig }}\"></div>",
            expect![[r#"
                ROOT@0..36
                  HTML_TAG@0..36
                    HTML_STARTING_TAG@0..30
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..29
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..29
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..28
                            TK_WORD@12..17 "hello"
                            TWIG_VAR@17..28
                              TK_WHITESPACE@17..18 " "
                              TK_OPEN_CURLY_CURLY@18..20 "{{"
                              TWIG_EXPRESSION@20..25
                                TWIG_LITERAL_NAME@20..25
                                  TK_WHITESPACE@20..21 " "
                                  TK_WORD@21..25 "twig"
                              TK_WHITESPACE@25..26 " "
                              TK_CLOSE_CURLY_CURLY@26..28 "}}"
                          TK_DOUBLE_QUOTES@28..29 "\""
                      TK_GREATER_THAN@29..30 ">"
                    BODY@30..30
                    HTML_ENDING_TAG@30..36
                      TK_LESS_THAN_SLASH@30..32 "</"
                      TK_WORD@32..35 "div"
                      TK_GREATER_THAN@35..36 ">""#]],
        );
    }

    #[test]
    fn parse_html_string_with_twig_comment() {
        check_parse(
            "<div class=\"{# hello twig #}\"></div>",
            expect![[r##"
                ROOT@0..36
                  HTML_TAG@0..36
                    HTML_STARTING_TAG@0..30
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..29
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..29
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..28
                            TWIG_COMMENT@12..28
                              TK_OPEN_CURLY_HASHTAG@12..14 "{#"
                              TK_WHITESPACE@14..15 " "
                              TK_WORD@15..20 "hello"
                              TK_WHITESPACE@20..21 " "
                              TK_WORD@21..25 "twig"
                              TK_WHITESPACE@25..26 " "
                              TK_HASHTAG_CLOSE_CURLY@26..28 "#}"
                          TK_DOUBLE_QUOTES@28..29 "\""
                      TK_GREATER_THAN@29..30 ">"
                    BODY@30..30
                    HTML_ENDING_TAG@30..36
                      TK_LESS_THAN_SLASH@30..32 "</"
                      TK_WORD@32..35 "div"
                      TK_GREATER_THAN@35..36 ">""##]],
        );
    }

    #[test]
    fn parse_html_string_with_twig_block() {
        check_parse(
            "<div class=\"hello {% block conditional %} twig {% endblock %}\"></div>",
            expect![[r#"
                ROOT@0..69
                  HTML_TAG@0..69
                    HTML_STARTING_TAG@0..63
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..62
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..62
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..61
                            TK_WORD@12..17 "hello"
                            TWIG_BLOCK@17..61
                              TWIG_STARTING_BLOCK@17..41
                                TK_WHITESPACE@17..18 " "
                                TK_CURLY_PERCENT@18..20 "{%"
                                TK_WHITESPACE@20..21 " "
                                TK_BLOCK@21..26 "block"
                                TK_WHITESPACE@26..27 " "
                                TK_WORD@27..38 "conditional"
                                TK_WHITESPACE@38..39 " "
                                TK_PERCENT_CURLY@39..41 "%}"
                              BODY@41..46
                                TK_WHITESPACE@41..42 " "
                                TK_WORD@42..46 "twig"
                              TWIG_ENDING_BLOCK@46..61
                                TK_WHITESPACE@46..47 " "
                                TK_CURLY_PERCENT@47..49 "{%"
                                TK_WHITESPACE@49..50 " "
                                TK_ENDBLOCK@50..58 "endblock"
                                TK_WHITESPACE@58..59 " "
                                TK_PERCENT_CURLY@59..61 "%}"
                          TK_DOUBLE_QUOTES@61..62 "\""
                      TK_GREATER_THAN@62..63 ">"
                    BODY@63..63
                    HTML_ENDING_TAG@63..69
                      TK_LESS_THAN_SLASH@63..65 "</"
                      TK_WORD@65..68 "div"
                      TK_GREATER_THAN@68..69 ">""#]],
        );
    }

    #[test]
    fn parse_html_string_with_twig_block_nested() {
        check_parse(
            "<div class=\"hello {% block outer %} outer {% block inner %} inner {% endblock %}{% endblock %}\"></div>",
            expect![[r#"
                ROOT@0..102
                  HTML_TAG@0..102
                    HTML_STARTING_TAG@0..96
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..95
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..95
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..94
                            TK_WORD@12..17 "hello"
                            TWIG_BLOCK@17..94
                              TWIG_STARTING_BLOCK@17..35
                                TK_WHITESPACE@17..18 " "
                                TK_CURLY_PERCENT@18..20 "{%"
                                TK_WHITESPACE@20..21 " "
                                TK_BLOCK@21..26 "block"
                                TK_WHITESPACE@26..27 " "
                                TK_WORD@27..32 "outer"
                                TK_WHITESPACE@32..33 " "
                                TK_PERCENT_CURLY@33..35 "%}"
                              BODY@35..80
                                TK_WHITESPACE@35..36 " "
                                TK_WORD@36..41 "outer"
                                TWIG_BLOCK@41..80
                                  TWIG_STARTING_BLOCK@41..59
                                    TK_WHITESPACE@41..42 " "
                                    TK_CURLY_PERCENT@42..44 "{%"
                                    TK_WHITESPACE@44..45 " "
                                    TK_BLOCK@45..50 "block"
                                    TK_WHITESPACE@50..51 " "
                                    TK_WORD@51..56 "inner"
                                    TK_WHITESPACE@56..57 " "
                                    TK_PERCENT_CURLY@57..59 "%}"
                                  BODY@59..65
                                    TK_WHITESPACE@59..60 " "
                                    TK_WORD@60..65 "inner"
                                  TWIG_ENDING_BLOCK@65..80
                                    TK_WHITESPACE@65..66 " "
                                    TK_CURLY_PERCENT@66..68 "{%"
                                    TK_WHITESPACE@68..69 " "
                                    TK_ENDBLOCK@69..77 "endblock"
                                    TK_WHITESPACE@77..78 " "
                                    TK_PERCENT_CURLY@78..80 "%}"
                              TWIG_ENDING_BLOCK@80..94
                                TK_CURLY_PERCENT@80..82 "{%"
                                TK_WHITESPACE@82..83 " "
                                TK_ENDBLOCK@83..91 "endblock"
                                TK_WHITESPACE@91..92 " "
                                TK_PERCENT_CURLY@92..94 "%}"
                          TK_DOUBLE_QUOTES@94..95 "\""
                      TK_GREATER_THAN@95..96 ">"
                    BODY@96..96
                    HTML_ENDING_TAG@96..102
                      TK_LESS_THAN_SLASH@96..98 "</"
                      TK_WORD@98..101 "div"
                      TK_GREATER_THAN@101..102 ">""#]],
        );
    }

    #[test]
    fn parse_html_string_with_twig_if_elseif_else() {
        check_parse(
            "<div class=\"hello {% if A > B %} greater {% elseif A == B %} equal {% else %} less {% endif %}\"></div>",
            expect![[r#"
                ROOT@0..102
                  HTML_TAG@0..102
                    HTML_STARTING_TAG@0..96
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..95
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..95
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..94
                            TK_WORD@12..17 "hello"
                            TWIG_IF@17..94
                              TWIG_IF_BLOCK@17..32
                                TK_WHITESPACE@17..18 " "
                                TK_CURLY_PERCENT@18..20 "{%"
                                TK_WHITESPACE@20..21 " "
                                TK_IF@21..23 "if"
                                TWIG_EXPRESSION@23..29
                                  TWIG_BINARY_EXPRESSION@23..29
                                    TWIG_EXPRESSION@23..25
                                      TWIG_LITERAL_NAME@23..25
                                        TK_WHITESPACE@23..24 " "
                                        TK_WORD@24..25 "A"
                                    TK_WHITESPACE@25..26 " "
                                    TK_GREATER_THAN@26..27 ">"
                                    TWIG_EXPRESSION@27..29
                                      TWIG_LITERAL_NAME@27..29
                                        TK_WHITESPACE@27..28 " "
                                        TK_WORD@28..29 "B"
                                TK_WHITESPACE@29..30 " "
                                TK_PERCENT_CURLY@30..32 "%}"
                              BODY@32..40
                                TK_WHITESPACE@32..33 " "
                                TK_WORD@33..40 "greater"
                              TWIG_ELSE_IF_BLOCK@40..60
                                TK_WHITESPACE@40..41 " "
                                TK_CURLY_PERCENT@41..43 "{%"
                                TK_WHITESPACE@43..44 " "
                                TK_ELSE_IF@44..50 "elseif"
                                TWIG_EXPRESSION@50..57
                                  TWIG_BINARY_EXPRESSION@50..57
                                    TWIG_EXPRESSION@50..52
                                      TWIG_LITERAL_NAME@50..52
                                        TK_WHITESPACE@50..51 " "
                                        TK_WORD@51..52 "A"
                                    TK_WHITESPACE@52..53 " "
                                    TK_DOUBLE_EQUAL@53..55 "=="
                                    TWIG_EXPRESSION@55..57
                                      TWIG_LITERAL_NAME@55..57
                                        TK_WHITESPACE@55..56 " "
                                        TK_WORD@56..57 "B"
                                TK_WHITESPACE@57..58 " "
                                TK_PERCENT_CURLY@58..60 "%}"
                              BODY@60..66
                                TK_WHITESPACE@60..61 " "
                                TK_WORD@61..66 "equal"
                              TWIG_ELSE_BLOCK@66..77
                                TK_WHITESPACE@66..67 " "
                                TK_CURLY_PERCENT@67..69 "{%"
                                TK_WHITESPACE@69..70 " "
                                TK_ELSE@70..74 "else"
                                TK_WHITESPACE@74..75 " "
                                TK_PERCENT_CURLY@75..77 "%}"
                              BODY@77..82
                                TK_WHITESPACE@77..78 " "
                                TK_WORD@78..82 "less"
                              TWIG_ENDIF_BLOCK@82..94
                                TK_WHITESPACE@82..83 " "
                                TK_CURLY_PERCENT@83..85 "{%"
                                TK_WHITESPACE@85..86 " "
                                TK_ENDIF@86..91 "endif"
                                TK_WHITESPACE@91..92 " "
                                TK_PERCENT_CURLY@92..94 "%}"
                          TK_DOUBLE_QUOTES@94..95 "\""
                      TK_GREATER_THAN@95..96 ">"
                    BODY@96..96
                    HTML_ENDING_TAG@96..102
                      TK_LESS_THAN_SLASH@96..98 "</"
                      TK_WORD@98..101 "div"
                      TK_GREATER_THAN@101..102 ">""#]],
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
                    HTML_STARTING_TAG@0..20
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..19
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..19
                          TK_SINGLE_QUOTES@11..12 "'"
                          HTML_STRING_INNER@12..18
                            TK_WORD@12..18 "my-div"
                          TK_SINGLE_QUOTES@18..19 "'"
                      TK_GREATER_THAN@19..20 ">"
                    BODY@20..40
                      HTML_TEXT@20..40
                        TK_LINE_BREAK@20..21 "\n"
                        TK_WHITESPACE@21..29 "        "
                        TK_WORD@29..34 "hello"
                        TK_WHITESPACE@34..35 " "
                        TK_WORD@35..40 "world"
                    HTML_ENDING_TAG@40..51
                      TK_LINE_BREAK@40..41 "\n"
                      TK_WHITESPACE@41..45 "    "
                      TK_LESS_THAN_SLASH@45..47 "</"
                      TK_WORD@47..50 "div"
                      TK_GREATER_THAN@50..51 ">""#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_trailing_single_quote_missing() {
        check_parse(
            "<div claSs='my-div>
        hello world
    </div>",
            expect![[r#"
                ROOT@0..50
                  HTML_TAG@0..50
                    HTML_STARTING_TAG@0..19
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..18
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..18
                          TK_SINGLE_QUOTES@11..12 "'"
                          HTML_STRING_INNER@12..18
                            TK_WORD@12..18 "my-div"
                      TK_GREATER_THAN@18..19 ">"
                    BODY@19..39
                      HTML_TEXT@19..39
                        TK_LINE_BREAK@19..20 "\n"
                        TK_WHITESPACE@20..28 "        "
                        TK_WORD@28..33 "hello"
                        TK_WHITESPACE@33..34 " "
                        TK_WORD@34..39 "world"
                    HTML_ENDING_TAG@39..50
                      TK_LINE_BREAK@39..40 "\n"
                      TK_WHITESPACE@40..44 "    "
                      TK_LESS_THAN_SLASH@44..46 "</"
                      TK_WORD@46..49 "div"
                      TK_GREATER_THAN@49..50 ">"
                error at 18..19: expected ' but found >"#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_leading_single_quote_missing() {
        check_parse(
            "<div claSs=my-div'>
        hello world
    </div>",
            expect![[r#"
                ROOT@0..50
                  HTML_TAG@0..50
                    HTML_STARTING_TAG@0..19
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..18
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..18
                          HTML_STRING_INNER@11..17
                            TK_WORD@11..17 "my-div"
                          ERROR@17..18
                            TK_SINGLE_QUOTES@17..18 "'"
                      TK_GREATER_THAN@18..19 ">"
                    BODY@19..39
                      HTML_TEXT@19..39
                        TK_LINE_BREAK@19..20 "\n"
                        TK_WHITESPACE@20..28 "        "
                        TK_WORD@28..33 "hello"
                        TK_WHITESPACE@33..34 " "
                        TK_WORD@34..39 "world"
                    HTML_ENDING_TAG@39..50
                      TK_LINE_BREAK@39..40 "\n"
                      TK_WHITESPACE@40..44 "    "
                      TK_LESS_THAN_SLASH@44..46 "</"
                      TK_WORD@46..49 "div"
                      TK_GREATER_THAN@49..50 ">"
                error at 17..18: expected no trailing quote because there is no leading quote but found '"#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_trailing_double_quote_missing() {
        check_parse(
            r#"<div claSs="my-div required>
        hello world
    </div>"#,
            expect![[r#"
                ROOT@0..59
                  HTML_TAG@0..59
                    HTML_STARTING_TAG@0..28
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..27
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..27
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..27
                            TK_WORD@12..18 "my-div"
                            TK_WHITESPACE@18..19 " "
                            TK_WORD@19..27 "required"
                      TK_GREATER_THAN@27..28 ">"
                    BODY@28..48
                      HTML_TEXT@28..48
                        TK_LINE_BREAK@28..29 "\n"
                        TK_WHITESPACE@29..37 "        "
                        TK_WORD@37..42 "hello"
                        TK_WHITESPACE@42..43 " "
                        TK_WORD@43..48 "world"
                    HTML_ENDING_TAG@48..59
                      TK_LINE_BREAK@48..49 "\n"
                      TK_WHITESPACE@49..53 "    "
                      TK_LESS_THAN_SLASH@53..55 "</"
                      TK_WORD@55..58 "div"
                      TK_GREATER_THAN@58..59 ">"
                error at 27..28: expected " but found >"#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_leading_double_quote_missing() {
        check_parse(
            r#"<div claSs=my-div" required>
        hello world
    </div>"#,
            expect![[r#"
                ROOT@0..59
                  HTML_TAG@0..59
                    HTML_STARTING_TAG@0..28
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..18
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..18
                          HTML_STRING_INNER@11..17
                            TK_WORD@11..17 "my-div"
                          ERROR@17..18
                            TK_DOUBLE_QUOTES@17..18 "\""
                      HTML_ATTRIBUTE@18..27
                        TK_WHITESPACE@18..19 " "
                        TK_WORD@19..27 "required"
                      TK_GREATER_THAN@27..28 ">"
                    BODY@28..48
                      HTML_TEXT@28..48
                        TK_LINE_BREAK@28..29 "\n"
                        TK_WHITESPACE@29..37 "        "
                        TK_WORD@37..42 "hello"
                        TK_WHITESPACE@42..43 " "
                        TK_WORD@43..48 "world"
                    HTML_ENDING_TAG@48..59
                      TK_LINE_BREAK@48..49 "\n"
                      TK_WHITESPACE@49..53 "    "
                      TK_LESS_THAN_SLASH@53..55 "</"
                      TK_WORD@55..58 "div"
                      TK_GREATER_THAN@58..59 ">"
                error at 17..18: expected no trailing quote because there is no leading quote but found ""#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_no_quotes() {
        check_parse(
            "<div claSs=my-div required>
        hello world
    </div>",
            expect![[r#"
                ROOT@0..58
                  HTML_TAG@0..58
                    HTML_STARTING_TAG@0..27
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..18
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..18
                          HTML_STRING_INNER@11..18
                            TK_WORD@11..17 "my-div"
                            TK_WHITESPACE@17..18 " "
                      HTML_ATTRIBUTE@18..26
                        TK_WORD@18..26 "required"
                      TK_GREATER_THAN@26..27 ">"
                    BODY@27..47
                      HTML_TEXT@27..47
                        TK_LINE_BREAK@27..28 "\n"
                        TK_WHITESPACE@28..36 "        "
                        TK_WORD@36..41 "hello"
                        TK_WHITESPACE@41..42 " "
                        TK_WORD@42..47 "world"
                    HTML_ENDING_TAG@47..58
                      TK_LINE_BREAK@47..48 "\n"
                      TK_WHITESPACE@48..52 "    "
                      TK_LESS_THAN_SLASH@52..54 "</"
                      TK_WORD@54..57 "div"
                      TK_GREATER_THAN@57..58 ">""#]],
        );
    }

    #[test]
    fn parse_html_attribute_with_no_quotes_and_twig_var() {
        check_parse(
            "<div claSs={{ my_class }}>
        hello world
    </div>",
            expect![[r#"
                ROOT@0..57
                  HTML_TAG@0..57
                    HTML_STARTING_TAG@0..26
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..11
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "claSs"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..11
                          HTML_STRING_INNER@11..11
                      TWIG_VAR@11..25
                        TK_OPEN_CURLY_CURLY@11..13 "{{"
                        TWIG_EXPRESSION@13..22
                          TWIG_LITERAL_NAME@13..22
                            TK_WHITESPACE@13..14 " "
                            TK_WORD@14..22 "my_class"
                        TK_WHITESPACE@22..23 " "
                        TK_CLOSE_CURLY_CURLY@23..25 "}}"
                      TK_GREATER_THAN@25..26 ">"
                    BODY@26..46
                      HTML_TEXT@26..46
                        TK_LINE_BREAK@26..27 "\n"
                        TK_WHITESPACE@27..35 "        "
                        TK_WORD@35..40 "hello"
                        TK_WHITESPACE@40..41 " "
                        TK_WORD@41..46 "world"
                    HTML_ENDING_TAG@46..57
                      TK_LINE_BREAK@46..47 "\n"
                      TK_WHITESPACE@47..51 "    "
                      TK_LESS_THAN_SLASH@51..53 "</"
                      TK_WORD@53..56 "div"
                      TK_GREATER_THAN@56..57 ">"
                error at 11..13: expected html attribute value but found {{"#]],
        );
    }

    #[test]
    fn parse_html_comment() {
        check_parse(
            "<!-- this is a comment --> this not <!-- but this again -->",
            expect![[r#"
                ROOT@0..59
                  HTML_COMMENT@0..26
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@0..4 "<!--"
                    TK_WHITESPACE@4..5 " "
                    TK_WORD@5..9 "this"
                    TK_WHITESPACE@9..10 " "
                    TK_IS@10..12 "is"
                    TK_WHITESPACE@12..13 " "
                    TK_WORD@13..14 "a"
                    TK_WHITESPACE@14..15 " "
                    TK_WORD@15..22 "comment"
                    TK_WHITESPACE@22..23 " "
                    TK_MINUS_MINUS_GREATER_THAN@23..26 "-->"
                  HTML_TEXT@26..35
                    TK_WHITESPACE@26..27 " "
                    TK_WORD@27..31 "this"
                    TK_WHITESPACE@31..32 " "
                    TK_NOT@32..35 "not"
                  HTML_COMMENT@35..59
                    TK_WHITESPACE@35..36 " "
                    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS@36..40 "<!--"
                    TK_WHITESPACE@40..41 " "
                    TK_WORD@41..44 "but"
                    TK_WHITESPACE@44..45 " "
                    TK_WORD@45..49 "this"
                    TK_WHITESPACE@49..50 " "
                    TK_WORD@50..55 "again"
                    TK_WHITESPACE@55..56 " "
                    TK_MINUS_MINUS_GREATER_THAN@56..59 "-->""#]],
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
                      TK_SLASH_GREATER_THAN@27..29 "/>""#]],
        );
    }

    #[test]
    fn test_html_attribute_twig_var() {
        check_parse(
            "<div class=\"hello\" {{ twig }}></div>",
            expect![[r#"
                ROOT@0..36
                  HTML_TAG@0..36
                    HTML_STARTING_TAG@0..30
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE@4..18
                        TK_WHITESPACE@4..5 " "
                        TK_WORD@5..10 "class"
                        TK_EQUAL@10..11 "="
                        HTML_STRING@11..18
                          TK_DOUBLE_QUOTES@11..12 "\""
                          HTML_STRING_INNER@12..17
                            TK_WORD@12..17 "hello"
                          TK_DOUBLE_QUOTES@17..18 "\""
                      TWIG_VAR@18..29
                        TK_WHITESPACE@18..19 " "
                        TK_OPEN_CURLY_CURLY@19..21 "{{"
                        TWIG_EXPRESSION@21..26
                          TWIG_LITERAL_NAME@21..26
                            TK_WHITESPACE@21..22 " "
                            TK_WORD@22..26 "twig"
                        TK_WHITESPACE@26..27 " "
                        TK_CLOSE_CURLY_CURLY@27..29 "}}"
                      TK_GREATER_THAN@29..30 ">"
                    BODY@30..30
                    HTML_ENDING_TAG@30..36
                      TK_LESS_THAN_SLASH@30..32 "</"
                      TK_WORD@32..35 "div"
                      TK_GREATER_THAN@35..36 ">""#]],
        );
    }

    #[test]
    fn test_html_attribute_twig_comment() {
        check_parse(
            "<div {# class=\"hello\" #}></div>",
            expect![[r##"
                ROOT@0..31
                  HTML_TAG@0..31
                    HTML_STARTING_TAG@0..25
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TWIG_COMMENT@4..24
                        TK_WHITESPACE@4..5 " "
                        TK_OPEN_CURLY_HASHTAG@5..7 "{#"
                        TK_WHITESPACE@7..8 " "
                        TK_WORD@8..13 "class"
                        TK_EQUAL@13..14 "="
                        TK_DOUBLE_QUOTES@14..15 "\""
                        TK_WORD@15..20 "hello"
                        TK_DOUBLE_QUOTES@20..21 "\""
                        TK_WHITESPACE@21..22 " "
                        TK_HASHTAG_CLOSE_CURLY@22..24 "#}"
                      TK_GREATER_THAN@24..25 ">"
                    BODY@25..25
                    HTML_ENDING_TAG@25..31
                      TK_LESS_THAN_SLASH@25..27 "</"
                      TK_WORD@27..30 "div"
                      TK_GREATER_THAN@30..31 ">""##]],
        );
    }

    #[test]
    fn test_html_attribute_twig_block() {
        check_parse(
            "<div {% block conditional %} class=\"hello\" {% endblock %}></div>",
            expect![[r#"
                ROOT@0..64
                  HTML_TAG@0..64
                    HTML_STARTING_TAG@0..58
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TWIG_BLOCK@4..57
                        TWIG_STARTING_BLOCK@4..28
                          TK_WHITESPACE@4..5 " "
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..25 "conditional"
                          TK_WHITESPACE@25..26 " "
                          TK_PERCENT_CURLY@26..28 "%}"
                        BODY@28..42
                          HTML_ATTRIBUTE@28..42
                            TK_WHITESPACE@28..29 " "
                            TK_WORD@29..34 "class"
                            TK_EQUAL@34..35 "="
                            HTML_STRING@35..42
                              TK_DOUBLE_QUOTES@35..36 "\""
                              HTML_STRING_INNER@36..41
                                TK_WORD@36..41 "hello"
                              TK_DOUBLE_QUOTES@41..42 "\""
                        TWIG_ENDING_BLOCK@42..57
                          TK_WHITESPACE@42..43 " "
                          TK_CURLY_PERCENT@43..45 "{%"
                          TK_WHITESPACE@45..46 " "
                          TK_ENDBLOCK@46..54 "endblock"
                          TK_WHITESPACE@54..55 " "
                          TK_PERCENT_CURLY@55..57 "%}"
                      TK_GREATER_THAN@57..58 ">"
                    BODY@58..58
                    HTML_ENDING_TAG@58..64
                      TK_LESS_THAN_SLASH@58..60 "</"
                      TK_WORD@60..63 "div"
                      TK_GREATER_THAN@63..64 ">""#]],
        );
    }

    #[test]
    fn test_html_attribute_twig_block_non_attribute_body() {
        check_parse(
            "<div {% block conditional %} <hr/> {% endblock %}></div>",
            expect![[r#"
                ROOT@0..56
                  HTML_TAG@0..34
                    HTML_STARTING_TAG@0..28
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TWIG_BLOCK@4..28
                        TWIG_STARTING_BLOCK@4..28
                          TK_WHITESPACE@4..5 " "
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..25 "conditional"
                          TK_WHITESPACE@25..26 " "
                          TK_PERCENT_CURLY@26..28 "%}"
                        BODY@28..28
                        TWIG_ENDING_BLOCK@28..28
                    BODY@28..34
                      HTML_TAG@28..34
                        HTML_STARTING_TAG@28..34
                          TK_WHITESPACE@28..29 " "
                          TK_LESS_THAN@29..30 "<"
                          TK_WORD@30..32 "hr"
                          TK_SLASH_GREATER_THAN@32..34 "/>"
                    HTML_ENDING_TAG@34..34
                  ERROR@34..37
                    TK_WHITESPACE@34..35 " "
                    TK_CURLY_PERCENT@35..37 "{%"
                  HTML_TEXT@37..46
                    TK_WHITESPACE@37..38 " "
                    TK_ENDBLOCK@38..46 "endblock"
                  ERROR@46..49
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}"
                  ERROR@49..50
                    TK_GREATER_THAN@49..50 ">"
                  ERROR@50..52
                    TK_LESS_THAN_SLASH@50..52 "</"
                  HTML_TEXT@52..55
                    TK_WORD@52..55 "div"
                  ERROR@55..56
                    TK_GREATER_THAN@55..56 ">"
                error at 29..30: expected {% but found <
                error at 29..30: expected endblock but found <
                error at 29..30: expected %} but found <
                error at 29..30: expected > but found <
                error at 35..37: expected </div> ending tag but found {%
                error at 38..46: expected twig tag but found endblock
                error at 47..49: expected html, text or twig element but found %}
                error at 49..50: expected html, text or twig element but found >
                error at 50..52: expected html, text or twig element but found </
                error at 55..56: expected html, text or twig element but found >"#]],
        );
    }

    #[test]
    fn test_html_attribute_twig_block_nested() {
        check_parse(
            "<div {% block outer %} class=\"hello\" {% block inner %} style=\"color: black\" {% endblock %}{% endblock %}></div>",
            expect![[r#"
                ROOT@0..111
                  HTML_TAG@0..111
                    HTML_STARTING_TAG@0..105
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TWIG_BLOCK@4..104
                        TWIG_STARTING_BLOCK@4..22
                          TK_WHITESPACE@4..5 " "
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..19 "outer"
                          TK_WHITESPACE@19..20 " "
                          TK_PERCENT_CURLY@20..22 "%}"
                        BODY@22..90
                          HTML_ATTRIBUTE@22..36
                            TK_WHITESPACE@22..23 " "
                            TK_WORD@23..28 "class"
                            TK_EQUAL@28..29 "="
                            HTML_STRING@29..36
                              TK_DOUBLE_QUOTES@29..30 "\""
                              HTML_STRING_INNER@30..35
                                TK_WORD@30..35 "hello"
                              TK_DOUBLE_QUOTES@35..36 "\""
                          TWIG_BLOCK@36..90
                            TWIG_STARTING_BLOCK@36..54
                              TK_WHITESPACE@36..37 " "
                              TK_CURLY_PERCENT@37..39 "{%"
                              TK_WHITESPACE@39..40 " "
                              TK_BLOCK@40..45 "block"
                              TK_WHITESPACE@45..46 " "
                              TK_WORD@46..51 "inner"
                              TK_WHITESPACE@51..52 " "
                              TK_PERCENT_CURLY@52..54 "%}"
                            BODY@54..75
                              HTML_ATTRIBUTE@54..75
                                TK_WHITESPACE@54..55 " "
                                TK_WORD@55..60 "style"
                                TK_EQUAL@60..61 "="
                                HTML_STRING@61..75
                                  TK_DOUBLE_QUOTES@61..62 "\""
                                  HTML_STRING_INNER@62..74
                                    TK_WORD@62..67 "color"
                                    TK_COLON@67..68 ":"
                                    TK_WHITESPACE@68..69 " "
                                    TK_WORD@69..74 "black"
                                  TK_DOUBLE_QUOTES@74..75 "\""
                            TWIG_ENDING_BLOCK@75..90
                              TK_WHITESPACE@75..76 " "
                              TK_CURLY_PERCENT@76..78 "{%"
                              TK_WHITESPACE@78..79 " "
                              TK_ENDBLOCK@79..87 "endblock"
                              TK_WHITESPACE@87..88 " "
                              TK_PERCENT_CURLY@88..90 "%}"
                        TWIG_ENDING_BLOCK@90..104
                          TK_CURLY_PERCENT@90..92 "{%"
                          TK_WHITESPACE@92..93 " "
                          TK_ENDBLOCK@93..101 "endblock"
                          TK_WHITESPACE@101..102 " "
                          TK_PERCENT_CURLY@102..104 "%}"
                      TK_GREATER_THAN@104..105 ">"
                    BODY@105..105
                    HTML_ENDING_TAG@105..111
                      TK_LESS_THAN_SLASH@105..107 "</"
                      TK_WORD@107..110 "div"
                      TK_GREATER_THAN@110..111 ">""#]],
        );
    }

    #[test]
    fn parse_html_attribute_name_special_token() {
        check_parse(
            r#"<label class="form-label" for="personalMail">
    hello
</label>"#,
            expect![[r#"
                ROOT@0..64
                  HTML_TAG@0..64
                    HTML_STARTING_TAG@0..45
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..6 "label"
                      HTML_ATTRIBUTE@6..25
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..12 "class"
                        TK_EQUAL@12..13 "="
                        HTML_STRING@13..25
                          TK_DOUBLE_QUOTES@13..14 "\""
                          HTML_STRING_INNER@14..24
                            TK_WORD@14..24 "form-label"
                          TK_DOUBLE_QUOTES@24..25 "\""
                      HTML_ATTRIBUTE@25..44
                        TK_WHITESPACE@25..26 " "
                        TK_WORD@26..29 "for"
                        TK_EQUAL@29..30 "="
                        HTML_STRING@30..44
                          TK_DOUBLE_QUOTES@30..31 "\""
                          HTML_STRING_INNER@31..43
                            TK_WORD@31..43 "personalMail"
                          TK_DOUBLE_QUOTES@43..44 "\""
                      TK_GREATER_THAN@44..45 ">"
                    BODY@45..55
                      HTML_TEXT@45..55
                        TK_LINE_BREAK@45..46 "\n"
                        TK_WHITESPACE@46..50 "    "
                        TK_WORD@50..55 "hello"
                    HTML_ENDING_TAG@55..64
                      TK_LINE_BREAK@55..56 "\n"
                      TK_LESS_THAN_SLASH@56..58 "</"
                      TK_WORD@58..63 "label"
                      TK_GREATER_THAN@63..64 ">""#]],
        );
    }

    #[test]
    fn parse_html_void_element() {
        check_parse(
            r#"<input type="submit" value="Submit">"#,
            expect![[r#"
            ROOT@0..36
              HTML_TAG@0..36
                HTML_STARTING_TAG@0..36
                  TK_LESS_THAN@0..1 "<"
                  TK_WORD@1..6 "input"
                  HTML_ATTRIBUTE@6..20
                    TK_WHITESPACE@6..7 " "
                    TK_WORD@7..11 "type"
                    TK_EQUAL@11..12 "="
                    HTML_STRING@12..20
                      TK_DOUBLE_QUOTES@12..13 "\""
                      HTML_STRING_INNER@13..19
                        TK_WORD@13..19 "submit"
                      TK_DOUBLE_QUOTES@19..20 "\""
                  HTML_ATTRIBUTE@20..35
                    TK_WHITESPACE@20..21 " "
                    TK_WORD@21..26 "value"
                    TK_EQUAL@26..27 "="
                    HTML_STRING@27..35
                      TK_DOUBLE_QUOTES@27..28 "\""
                      HTML_STRING_INNER@28..34
                        TK_WORD@28..34 "Submit"
                      TK_DOUBLE_QUOTES@34..35 "\""
                  TK_GREATER_THAN@35..36 ">""#]],
        );
    }

    #[test]
    fn parse_html_void_element_self_closing() {
        check_parse(
            r#"<hr/>"#,
            expect![[r#"
            ROOT@0..5
              HTML_TAG@0..5
                HTML_STARTING_TAG@0..5
                  TK_LESS_THAN@0..1 "<"
                  TK_WORD@1..3 "hr"
                  TK_SLASH_GREATER_THAN@3..5 "/>""#]],
        );
    }

    #[test]
    fn parse_html_void_element_wrong_used() {
        check_parse(
            r#"<input type="submit" value="Submit">hello</input>"#,
            expect![[r#"
                ROOT@0..49
                  HTML_TAG@0..36
                    HTML_STARTING_TAG@0..36
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..6 "input"
                      HTML_ATTRIBUTE@6..20
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "type"
                        TK_EQUAL@11..12 "="
                        HTML_STRING@12..20
                          TK_DOUBLE_QUOTES@12..13 "\""
                          HTML_STRING_INNER@13..19
                            TK_WORD@13..19 "submit"
                          TK_DOUBLE_QUOTES@19..20 "\""
                      HTML_ATTRIBUTE@20..35
                        TK_WHITESPACE@20..21 " "
                        TK_WORD@21..26 "value"
                        TK_EQUAL@26..27 "="
                        HTML_STRING@27..35
                          TK_DOUBLE_QUOTES@27..28 "\""
                          HTML_STRING_INNER@28..34
                            TK_WORD@28..34 "Submit"
                          TK_DOUBLE_QUOTES@34..35 "\""
                      TK_GREATER_THAN@35..36 ">"
                  HTML_TEXT@36..41
                    TK_WORD@36..41 "hello"
                  ERROR@41..43
                    TK_LESS_THAN_SLASH@41..43 "</"
                  HTML_TEXT@43..48
                    TK_WORD@43..48 "input"
                  ERROR@48..49
                    TK_GREATER_THAN@48..49 ">"
                error at 41..43: expected html, text or twig element but found </
                error at 48..49: expected html, text or twig element but found >"#]],
        );
    }

    #[test]
    fn parse_fuzzing_bump_error() {
        check_parse(
            "<d a={%%",
            expect![[r#"
                ROOT@0..8
                  HTML_TAG@0..8
                    HTML_STARTING_TAG@0..8
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..2 "d"
                      HTML_ATTRIBUTE@2..5
                        TK_WHITESPACE@2..3 " "
                        TK_WORD@3..4 "a"
                        TK_EQUAL@4..5 "="
                        HTML_STRING@5..5
                          HTML_STRING_INNER@5..5
                      ERROR@5..7
                        TK_CURLY_PERCENT@5..7 "{%"
                      ERROR@7..8
                        TK_PERCENT@7..8 "%"
                    BODY@8..8
                    HTML_ENDING_TAG@8..8
                error at 5..7: expected html attribute value but found {%
                error at 7..8: expected twig tag but found %
                error at 7..8: expected > but found %
                error at 7..8: expected </d> ending tag but reached end of file"#]],
        );
    }

    #[test]
    fn parse_html_tag_with_unknown_token_in_body() {
        check_parse(
            "<div> \\t unknown token </div>",
            expect![[r#"
                ROOT@0..29
                  HTML_TAG@0..29
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..22
                      HTML_TEXT@5..22
                        TK_WHITESPACE@5..6 " "
                        TK_BACKWARD_SLASH@6..7 "\\"
                        TK_WORD@7..8 "t"
                        TK_WHITESPACE@8..9 " "
                        TK_WORD@9..16 "unknown"
                        TK_WHITESPACE@16..17 " "
                        TK_WORD@17..22 "token"
                    HTML_ENDING_TAG@22..29
                      TK_WHITESPACE@22..23 " "
                      TK_LESS_THAN_SLASH@23..25 "</"
                      TK_WORD@25..28 "div"
                      TK_GREATER_THAN@28..29 ">""#]],
        )
    }

    #[test]
    fn parse_html_doctype() {
        check_parse(
            "<!doctype html>",
            expect![[r#"
            ROOT@0..15
              HTML_DOCTYPE@0..15
                TK_LESS_THAN_EXCLAMATION_MARK@0..2 "<!"
                TK_DOCTYPE@2..9 "doctype"
                TK_WHITESPACE@9..10 " "
                TK_WORD@10..14 "html"
                TK_GREATER_THAN@14..15 ">""#]],
        )
    }
}

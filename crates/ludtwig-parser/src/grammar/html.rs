use crate::T;
use crate::grammar::twig::{at_twig_termination_tag, parse_any_twig, parse_twig_var_statement};
use crate::grammar::{ParseFunction, parse_any_element, parse_ludtwig_directive, parse_many};
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{GENERAL_RECOVERY_SET, ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use regex::Regex;
use std::sync::LazyLock;

// Every token value that matches this regex is allowed for html attribute names
static HTML_ATTRIBUTE_NAME_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([a-zA-Z]|([:@\#_\$][a-zA-Z]))[a-zA-Z0-9_\-]*$").unwrap());

static HTML_TAG_NAME_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[a-zA-Z][a-zA-Z0-9\-]*$").unwrap());

/// See "void elements" in spec https://html.spec.whatwg.org/multipage/syntax.html#elements-2
static HTML_VOID_ELEMENTS: &[&str] = &[
    "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link",
    "meta", "param", "source", "track", "wbr",
];

/// See "raw text elements" in spec https://html.spec.whatwg.org/multipage/syntax.html#elements-2
static HTML_RAW_TEXT_ELEMENTS: &[&str] = &["script", "style", "textarea", "title"];

pub(super) fn parse_any_html(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(T!["<"])
        && parser.peek_nth_token(1).is_some_and(|t| {
            t.kind != T![ws] && t.kind != T![number] && !GENERAL_RECOVERY_SET.contains(&t.kind)
        })
    {
        // '<' should not be followed by EOF, a ws, a number or RECOVERY_SET token,
        // because then it is considered as arbitrary text (and parsed by the last else block)
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

    parser.expect(T!["DOCTYPE"], &[T![word], T![">"]]);
    parser.expect(T![word], &[T![">"]]);
    parser.expect(T![">"], &[]);

    parser.complete(m, SyntaxKind::HTML_DOCTYPE)
}

fn parse_html_text(parser: &mut Parser) -> Option<CompletedMarker> {
    fn parser_at_less_than_non_word(p: &mut Parser) -> bool {
        p.at(T!["<"])
            && p.peek_nth_token(1).is_none_or(|t| {
                t.kind == T![ws] || t.kind == T![number] || GENERAL_RECOVERY_SET.contains(&t.kind)
            })
    }

    if parser.at_end()
        || parser.at_set(&[T!["</"]])
        || (parser.at_set(GENERAL_RECOVERY_SET) && !parser_at_less_than_non_word(parser))
    {
        return None;
    }

    let m = parser.start();

    parse_many(
        parser,
        |p| {
            p.at_set(&[T!["</"]])
                || (p.at_set(GENERAL_RECOVERY_SET) && !parser_at_less_than_non_word(p))
        },
        |p| {
            p.bump();
        },
    );

    Some(parser.complete(m, SyntaxKind::HTML_TEXT))
}

/// parses any content until it finds the matching ending tag or encounters a wild twig ending block.
/// It will parse any twig syntax and nested blocks will also only contain raw text nodes.
/// returns true if it encountered the matching ending tag
fn parse_html_raw_text(
    parser: &mut Parser,
    starting_tag_tokentype: SyntaxKind,
    starting_tag_name: &str,
) -> bool {
    #[allow(clippy::unnecessary_wraps)]
    fn parse_html_raw_text_inner(parser: &mut Parser) -> Option<CompletedMarker> {
        let raw_text_m = parser.start();

        parse_many(
            parser,
            |p| {
                if at_twig_termination_tag(p) {
                    return true; // endblock in the wild may mean this tag has a missing closing tag
                }

                false
            },
            |p| {
                if parse_any_twig(p, parse_html_raw_text_inner).is_none() {
                    p.bump(); // just bump anything until the early exit closure stops us
                }
            },
        );
        Some(parser.complete(raw_text_m, SyntaxKind::HTML_RAW_TEXT))
    }

    let mut matching_end_tag_encountered = false;
    let raw_text_m = parser.start();

    parse_many(
        parser,
        |p| {
            if p.at_following_content(&[
                (T!["</"], None),
                (starting_tag_tokentype, Some(starting_tag_name)),
            ]) {
                matching_end_tag_encountered = true;
                return true; // found matching closing tag
            }

            if at_twig_termination_tag(p) {
                return true; // endblock in the wild may mean this tag has a missing closing tag
            }

            false
        },
        |p| {
            if parse_any_twig(p, parse_html_raw_text_inner).is_none() {
                p.bump(); // just bump anything until the early exit closure stops us
            }
        },
    );
    parser.complete(raw_text_m, SyntaxKind::HTML_RAW_TEXT);
    matching_end_tag_encountered
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

    parser.expect(T!["-->"], &[]);
    parser.complete(outer, SyntaxKind::HTML_COMMENT)
}

#[allow(clippy::too_many_lines)]
fn parse_html_element(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(T!["<"]));
    let m = parser.start();

    // parse start tag
    let starting_tag_m = parser.start();
    parser.bump();

    let tag_name = parser.peek_token().map_or("", |t| t.text).to_owned();
    let tag_name_lowercase = tag_name.to_ascii_lowercase();
    let tag_name_tokentype = parser.peek_token().map_or(SyntaxKind::TK_WORD, |t| t.kind);

    if tag_name_tokentype == T![twig component name] {
        parser.bump();
    } else if HTML_TAG_NAME_REGEX.is_match(&tag_name) {
        // normal html tag name
        parser.bump_as(T![word]);
    } else {
        parser.add_error(ParseErrorBuilder::new("HTML Tag Name"));
        parser.recover(&[T![">"], T!["/>"], T!["</"], T![word], T![">"]]);
    }

    // parse attributes (can include twig)
    let attributes_m = parser.start();
    parse_many(
        parser,
        |p| p.at(T![">"]) || p.at(T!["/>"]),
        |p| {
            parse_html_attribute_or_twig(p);
        },
    );
    parser.complete(attributes_m, SyntaxKind::HTML_ATTRIBUTE_LIST);

    // parse end of starting tag
    let mut is_self_closing = if parser.at(T!["/>"]) {
        parser.bump();
        true
    } else {
        parser.expect(T![">"], &[T!["</"], T![word], T![">"]]);
        false
    };

    if HTML_VOID_ELEMENTS.contains(&&*tag_name_lowercase) {
        is_self_closing = true; // void elements never have children or an end tag
    }

    parser.complete(starting_tag_m, SyntaxKind::HTML_STARTING_TAG);

    // early return in case of self-closing
    if is_self_closing {
        return parser.complete(m, SyntaxKind::HTML_TAG);
    }

    // parse all the children
    let body_m = parser.start();
    let mut matching_end_tag_encountered = false;

    if HTML_RAW_TEXT_ELEMENTS.contains(&&*tag_name_lowercase) {
        matching_end_tag_encountered = parse_html_raw_text(parser, tag_name_tokentype, &tag_name);
    } else {
        parse_many(
            parser,
            |p| {
                if p.at_following_content(&[
                    (T!["</"], None),
                    (tag_name_tokentype, Some(&tag_name)),
                ]) {
                    matching_end_tag_encountered = true;
                    return true; // found matching closing tag
                }

                if at_twig_termination_tag(p) {
                    return true; // endblock in the wild may mean this tag has a missing closing tag
                }

                false
            },
            |p| {
                parse_any_element(p);
            },
        );
    }
    parser.complete(body_m, SyntaxKind::BODY);

    // parse matching end tag or report missing (the tag itself is not self-closing!)
    let end_tag_m = parser.start();
    if matching_end_tag_encountered {
        // found matching closing tag
        parser.expect(T!["</"], &[tag_name_tokentype, T![">"]]);

        if parser.at(T![twig component name]) {
            parser.bump();
        } else if parser.at(tag_name_tokentype) {
            parser.bump_as(T![word]);
        } else {
            parser.add_error(ParseErrorBuilder::new(format!(
                "{tag_name} as ending tag name"
            )));
            parser.recover(&[T![">"]]);
        }

        parser.expect(T![">"], &[]);
    } else {
        // no matching end tag found!
        parser.add_error(ParseErrorBuilder::new(format!("</{tag_name}> ending tag")));
        parser.recover(&[]);
    }
    parser.complete(end_tag_m, SyntaxKind::HTML_ENDING_TAG);

    parser.complete(m, SyntaxKind::HTML_TAG)
}

fn parse_html_attribute_or_twig(parser: &mut Parser) -> Option<CompletedMarker> {
    let token_text = if parser.at(T![":"]) {
        format!(":{}", parser.peek_nth_token(1)?.text)
    } else {
        parser.peek_token()?.text.to_owned()
    };

    let attribute_m = if HTML_ATTRIBUTE_NAME_REGEX.is_match(&token_text) {
        // normal html attribute name
        let attribute_m = parser.start();
        if parser.at(T![":"]) {
            parser.bump_next_n_as(2, T![word]);
        } else {
            parser.bump_as(T![word]);
        }

        attribute_m
    } else {
        // is the attribute name a twig var expression?
        if parser.at(T!["{{"]) {
            let twig_name_attribute_m = parser.start();
            parse_twig_var_statement(parser);
            twig_name_attribute_m
        } else {
            // parse any twig block / comment syntax where its children can only be html attributes (this parser)
            // this structure itself doesn't count as an HTML_ATTRIBUTE node
            return parse_any_twig(parser, parse_html_attribute_or_twig);
        }
    };

    if parser.at(T!["="]) {
        // attribute value
        parser.bump();
        parse_html_attribute_value_string(parser);
    }

    Some(parser.complete(attribute_m, SyntaxKind::HTML_ATTRIBUTE))
}

/// html attribute value can be either a single word or twig var expression or
/// a single / double quoted string (which can contain arbitrary twig syntax)
/// In either case it will be wrapped into an `HTML_STRING` node which may or may
/// not contain quotes
fn parse_html_attribute_value_string(parser: &mut Parser) -> CompletedMarker {
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
        } else if parser.at(T!["{{"]) {
            // a single twig var expression with missing quotes should also count as an html attribute value
            parse_twig_var_statement(parser);
        } else {
            parser.add_error(ParseErrorBuilder::new("html attribute value"));
            parser.recover(&[T![word], T![">"], T!["/>"]]);
        }

        None
    }

    fn child_early_return(p: &mut Parser) -> bool {
        if at_twig_termination_tag(p) {
            return true;
        }

        false
    }

    fn child_parser(p: &mut Parser, inner_twig_child_parser: ParseFunction) {
        if parse_any_twig(p, inner_twig_child_parser).is_none() {
            if p.at_set(&[T![">"], T!["/>"]]) || p.at_set(GENERAL_RECOVERY_SET) || p.at_end() {
                return;
            }

            p.bump();
        }
    }

    let m = parser.start();
    let quote_kind = if parser.at_set(&[T!["\""], T!["'"]]) {
        let starting_quote_token = parser.bump();
        Some(starting_quote_token.kind)
    } else {
        // the HTML specification also allows no quotes but then
        // the value must only be a single word
        None
    };

    let inner_m = parser.start();
    // run the appropriate inner parser
    match quote_kind {
        Some(T!["\""]) => {
            inner_double_quote_parser(parser);
        }
        Some(T!["'"]) => {
            inner_single_quote_parser(parser);
        }
        None => {
            inner_no_quote_parser(parser);
        }
        Some(_) => unreachable!(),
    }

    if quote_kind.is_some() {
        // consume any trailing trivia to be inside the inner string
        // but only when quotation exists (otherwise only the single word should be inside the HTML_STRING_INNER node)
        parser.explicitly_consume_trivia();
    }

    parser.complete(inner_m, SyntaxKind::HTML_STRING_INNER);

    // expect the same closing quote if a starting quote existed
    if let Some(quote_kind) = quote_kind {
        parser.expect(quote_kind, &[T![">"], T!["/>"]]);
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
                      HTML_ATTRIBUTE_LIST@4..4
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
                      HTML_ATTRIBUTE_LIST@4..53
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
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..29
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..28
                        HTML_STARTING_TAG@10..16
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..15 "span"
                          HTML_ATTRIBUTE_LIST@15..15
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
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..85
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..28
                        HTML_STARTING_TAG@10..16
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..15 "span"
                          HTML_ATTRIBUTE_LIST@15..15
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
                          HTML_ATTRIBUTE_LIST@30..30
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
                          HTML_ATTRIBUTE_LIST@69..69
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
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..15
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..15
                        HTML_STARTING_TAG@10..15
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..13 "hr"
                          HTML_ATTRIBUTE_LIST@13..13
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
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..28
                      HTML_TEXT@5..10
                        TK_WORD@5..10 "hello"
                      HTML_TAG@10..28
                        HTML_STARTING_TAG@10..16
                          TK_LESS_THAN@10..11 "<"
                          TK_WORD@11..15 "span"
                          HTML_ATTRIBUTE_LIST@15..15
                          TK_GREATER_THAN@15..16 ">"
                        BODY@16..22
                          HTML_TEXT@16..22
                            TK_WORD@16..21 "world"
                            TK_EXCLAMATION_MARK@21..22 "!"
                        HTML_ENDING_TAG@22..28
                          ERROR@22..28
                            TK_LESS_THAN_SLASH@22..24 "</"
                            TK_WORD@24..27 "div"
                            TK_GREATER_THAN@27..28 ">"
                    HTML_ENDING_TAG@28..28
                error at 22..24: expected </span> ending tag but found </
                error at 27..28: expected </div> ending tag but reached end of file"#]],
        );
    }

    #[test]
    fn parse_html_element_with_cutoff_closing_tag() {
        check_parse(
            r"<div>
            {% block a %}
            <p>
                hello
            {% endblock %}
            <span>world</span>
            </p>
            </div>",
            expect![[r#"
                ROOT@0..163
                  HTML_TAG@0..163
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..127
                      TWIG_BLOCK@5..96
                        TWIG_STARTING_BLOCK@5..31
                          TK_LINE_BREAK@5..6 "\n"
                          TK_WHITESPACE@6..18 "            "
                          TK_CURLY_PERCENT@18..20 "{%"
                          TK_WHITESPACE@20..21 " "
                          TK_BLOCK@21..26 "block"
                          TK_WHITESPACE@26..27 " "
                          TK_WORD@27..28 "a"
                          TK_WHITESPACE@28..29 " "
                          TK_PERCENT_CURLY@29..31 "%}"
                        BODY@31..69
                          HTML_TAG@31..69
                            HTML_STARTING_TAG@31..47
                              TK_LINE_BREAK@31..32 "\n"
                              TK_WHITESPACE@32..44 "            "
                              TK_LESS_THAN@44..45 "<"
                              TK_WORD@45..46 "p"
                              HTML_ATTRIBUTE_LIST@46..46
                              TK_GREATER_THAN@46..47 ">"
                            BODY@47..69
                              HTML_TEXT@47..69
                                TK_LINE_BREAK@47..48 "\n"
                                TK_WHITESPACE@48..64 "                "
                                TK_WORD@64..69 "hello"
                            HTML_ENDING_TAG@69..69
                        TWIG_ENDING_BLOCK@69..96
                          TK_LINE_BREAK@69..70 "\n"
                          TK_WHITESPACE@70..82 "            "
                          TK_CURLY_PERCENT@82..84 "{%"
                          TK_WHITESPACE@84..85 " "
                          TK_ENDBLOCK@85..93 "endblock"
                          TK_WHITESPACE@93..94 " "
                          TK_PERCENT_CURLY@94..96 "%}"
                      HTML_TAG@96..127
                        HTML_STARTING_TAG@96..115
                          TK_LINE_BREAK@96..97 "\n"
                          TK_WHITESPACE@97..109 "            "
                          TK_LESS_THAN@109..110 "<"
                          TK_WORD@110..114 "span"
                          HTML_ATTRIBUTE_LIST@114..114
                          TK_GREATER_THAN@114..115 ">"
                        BODY@115..120
                          HTML_TEXT@115..120
                            TK_WORD@115..120 "world"
                        HTML_ENDING_TAG@120..127
                          TK_LESS_THAN_SLASH@120..122 "</"
                          TK_WORD@122..126 "span"
                          TK_GREATER_THAN@126..127 ">"
                    HTML_ENDING_TAG@127..163
                      ERROR@127..163
                        TK_LINE_BREAK@127..128 "\n"
                        TK_WHITESPACE@128..140 "            "
                        TK_LESS_THAN_SLASH@140..142 "</"
                        TK_WORD@142..143 "p"
                        TK_GREATER_THAN@143..144 ">"
                        TK_LINE_BREAK@144..145 "\n"
                        TK_WHITESPACE@145..157 "            "
                        TK_LESS_THAN_SLASH@157..159 "</"
                        TK_WORD@159..162 "div"
                        TK_GREATER_THAN@162..163 ">"
                error at 82..84: expected </p> ending tag but found {%
                error at 140..142: expected </div> ending tag but found </"#]],
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
                      HTML_ATTRIBUTE_LIST@4..4
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
        );
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
                      HTML_ATTRIBUTE_LIST@4..4
                      TK_GREATER_THAN@4..5 ">"
                    BODY@5..39
                      TWIG_BLOCK@5..39
                        TWIG_STARTING_BLOCK@5..22
                          TK_CURLY_PERCENT@5..7 "{%"
                          TK_WHITESPACE@7..8 " "
                          TK_BLOCK@8..13 "block"
                          TK_WHITESPACE@13..14 " "
                          TK_WORD@14..19 "inner"
                          TK_WHITESPACE@19..20 " "
                          TK_PERCENT_CURLY@20..22 "%}"
                        BODY@22..39
                          HTML_TAG@22..39
                            HTML_STARTING_TAG@22..28
                              TK_LESS_THAN@22..23 "<"
                              TK_WORD@23..27 "span"
                              HTML_ATTRIBUTE_LIST@27..27
                              TK_GREATER_THAN@27..28 ">"
                            BODY@28..33
                              HTML_TEXT@28..33
                                TK_WORD@28..33 "hello"
                            HTML_ENDING_TAG@33..39
                              ERROR@33..39
                                TK_LESS_THAN_SLASH@33..35 "</"
                                TK_WORD@35..38 "div"
                                TK_GREATER_THAN@38..39 ">"
                        TWIG_ENDING_BLOCK@39..39
                    HTML_ENDING_TAG@39..39
                error at 33..35: expected </span> ending tag but found </
                error at 38..39: expected {% but reached end of file
                error at 38..39: expected endblock but reached end of file
                error at 38..39: expected %} but reached end of file
                error at 38..39: expected </div> ending tag but reached end of file"#]],
        );
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
                      HTML_ATTRIBUTE_LIST@4..23
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
                      HTML_ATTRIBUTE_LIST@4..29
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
                      HTML_ATTRIBUTE_LIST@4..29
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
                      HTML_ATTRIBUTE_LIST@4..62
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
                      HTML_ATTRIBUTE_LIST@4..95
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
                      HTML_ATTRIBUTE_LIST@4..95
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
                      HTML_ATTRIBUTE_LIST@4..19
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
                      HTML_ATTRIBUTE_LIST@4..18
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
                      HTML_ATTRIBUTE_LIST@4..18
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
                      HTML_ATTRIBUTE_LIST@4..27
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
                      HTML_ATTRIBUTE_LIST@4..27
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
                      HTML_ATTRIBUTE_LIST@4..26
                        HTML_ATTRIBUTE@4..17
                          TK_WHITESPACE@4..5 " "
                          TK_WORD@5..10 "claSs"
                          TK_EQUAL@10..11 "="
                          HTML_STRING@11..17
                            HTML_STRING_INNER@11..17
                              TK_WORD@11..17 "my-div"
                        HTML_ATTRIBUTE@17..26
                          TK_WHITESPACE@17..18 " "
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
                      HTML_ATTRIBUTE_LIST@3..3
                      TK_SLASH_GREATER_THAN@3..5 "/>"
                  HTML_TEXT@5..10
                    TK_WORD@5..10 "plain"
                  HTML_TAG@10..16
                    HTML_STARTING_TAG@10..16
                      TK_LESS_THAN@10..11 "<"
                      TK_WORD@11..14 "img"
                      HTML_ATTRIBUTE_LIST@14..14
                      TK_SLASH_GREATER_THAN@14..16 "/>"
                  HTML_TEXT@16..20
                    TK_WORD@16..20 "text"
                  HTML_TAG@20..29
                    HTML_STARTING_TAG@20..29
                      TK_LESS_THAN@20..21 "<"
                      TK_WORD@21..27 "custom"
                      HTML_ATTRIBUTE_LIST@27..27
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
                      HTML_ATTRIBUTE_LIST@4..29
                        HTML_ATTRIBUTE@4..18
                          TK_WHITESPACE@4..5 " "
                          TK_WORD@5..10 "class"
                          TK_EQUAL@10..11 "="
                          HTML_STRING@11..18
                            TK_DOUBLE_QUOTES@11..12 "\""
                            HTML_STRING_INNER@12..17
                              TK_WORD@12..17 "hello"
                            TK_DOUBLE_QUOTES@17..18 "\""
                        HTML_ATTRIBUTE@18..29
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
                      HTML_ATTRIBUTE_LIST@4..24
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
                      HTML_ATTRIBUTE_LIST@4..57
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
                      HTML_ATTRIBUTE_LIST@4..28
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
                          HTML_ATTRIBUTE_LIST@32..32
                          TK_SLASH_GREATER_THAN@32..34 "/>"
                    HTML_ENDING_TAG@34..34
                  ERROR@34..37
                    TK_WHITESPACE@34..35 " "
                    TK_CURLY_PERCENT@35..37 "{%"
                  HTML_TEXT@37..50
                    TK_WHITESPACE@37..38 " "
                    TK_ENDBLOCK@38..46 "endblock"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}"
                    TK_GREATER_THAN@49..50 ">"
                  ERROR@50..56
                    TK_LESS_THAN_SLASH@50..52 "</"
                    TK_WORD@52..55 "div"
                    TK_GREATER_THAN@55..56 ">"
                error at 29..30: expected {% but found <
                error at 29..30: expected endblock but found <
                error at 29..30: expected %} but found <
                error at 29..30: expected > but found <
                error at 35..37: expected </div> ending tag but found {%
                error at 38..46: expected twig tag but found endblock
                error at 50..52: expected html, text or twig element but found </"#]],
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
                      HTML_ATTRIBUTE_LIST@4..104
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
                      HTML_ATTRIBUTE_LIST@6..44
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
    fn parse_html_attribute_name_as_twig_var_expression() {
        check_parse(
            r#"<div {{ dataBsDismissAttr }}="modal">hello</div>"#,
            expect![[r#"
                ROOT@0..48
                  HTML_TAG@0..48
                    HTML_STARTING_TAG@0..37
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE_LIST@4..36
                        HTML_ATTRIBUTE@4..36
                          TWIG_VAR@4..28
                            TK_WHITESPACE@4..5 " "
                            TK_OPEN_CURLY_CURLY@5..7 "{{"
                            TWIG_EXPRESSION@7..25
                              TWIG_LITERAL_NAME@7..25
                                TK_WHITESPACE@7..8 " "
                                TK_WORD@8..25 "dataBsDismissAttr"
                            TK_WHITESPACE@25..26 " "
                            TK_CLOSE_CURLY_CURLY@26..28 "}}"
                          TK_EQUAL@28..29 "="
                          HTML_STRING@29..36
                            TK_DOUBLE_QUOTES@29..30 "\""
                            HTML_STRING_INNER@30..35
                              TK_WORD@30..35 "modal"
                            TK_DOUBLE_QUOTES@35..36 "\""
                      TK_GREATER_THAN@36..37 ">"
                    BODY@37..42
                      HTML_TEXT@37..42
                        TK_WORD@37..42 "hello"
                    HTML_ENDING_TAG@42..48
                      TK_LESS_THAN_SLASH@42..44 "</"
                      TK_WORD@44..47 "div"
                      TK_GREATER_THAN@47..48 ">""#]],
        );
    }

    #[test]
    fn parse_html_attribute_name_as_twig_var_expression_and_value_as_string_with_twig_var_expression()
     {
        check_parse(
            r##"<div {{ dataBsTargetAttr }}="#{{ filterItemId }}">hello</div>"##,
            expect![[r##"
                ROOT@0..61
                  HTML_TAG@0..61
                    HTML_STARTING_TAG@0..50
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..4 "div"
                      HTML_ATTRIBUTE_LIST@4..49
                        HTML_ATTRIBUTE@4..49
                          TWIG_VAR@4..27
                            TK_WHITESPACE@4..5 " "
                            TK_OPEN_CURLY_CURLY@5..7 "{{"
                            TWIG_EXPRESSION@7..24
                              TWIG_LITERAL_NAME@7..24
                                TK_WHITESPACE@7..8 " "
                                TK_WORD@8..24 "dataBsTargetAttr"
                            TK_WHITESPACE@24..25 " "
                            TK_CLOSE_CURLY_CURLY@25..27 "}}"
                          TK_EQUAL@27..28 "="
                          HTML_STRING@28..49
                            TK_DOUBLE_QUOTES@28..29 "\""
                            HTML_STRING_INNER@29..48
                              TK_HASHTAG@29..30 "#"
                              TWIG_VAR@30..48
                                TK_OPEN_CURLY_CURLY@30..32 "{{"
                                TWIG_EXPRESSION@32..45
                                  TWIG_LITERAL_NAME@32..45
                                    TK_WHITESPACE@32..33 " "
                                    TK_WORD@33..45 "filterItemId"
                                TK_WHITESPACE@45..46 " "
                                TK_CLOSE_CURLY_CURLY@46..48 "}}"
                            TK_DOUBLE_QUOTES@48..49 "\""
                      TK_GREATER_THAN@49..50 ">"
                    BODY@50..55
                      HTML_TEXT@50..55
                        TK_WORD@50..55 "hello"
                    HTML_ENDING_TAG@55..61
                      TK_LESS_THAN_SLASH@55..57 "</"
                      TK_WORD@57..60 "div"
                      TK_GREATER_THAN@60..61 ">""##]],
        );
    }

    #[test]
    fn parse_html_attribute_with_no_quotes_and_twig_var_expression() {
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
                      HTML_ATTRIBUTE_LIST@4..25
                        HTML_ATTRIBUTE@4..25
                          TK_WHITESPACE@4..5 " "
                          TK_WORD@5..10 "claSs"
                          TK_EQUAL@10..11 "="
                          HTML_STRING@11..25
                            HTML_STRING_INNER@11..25
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
                      TK_GREATER_THAN@56..57 ">""#]],
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
                      HTML_ATTRIBUTE_LIST@6..35
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
            r"<hr/>",
            expect![[r#"
                ROOT@0..5
                  HTML_TAG@0..5
                    HTML_STARTING_TAG@0..5
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..3 "hr"
                      HTML_ATTRIBUTE_LIST@3..3
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
                      HTML_ATTRIBUTE_LIST@6..35
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
                  ERROR@41..49
                    TK_LESS_THAN_SLASH@41..43 "</"
                    TK_WORD@43..48 "input"
                    TK_GREATER_THAN@48..49 ">"
                error at 41..43: expected html, text or twig element but found </"#]],
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
                      HTML_ATTRIBUTE_LIST@2..7
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
                      HTML_ATTRIBUTE_LIST@4..4
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
        );
    }

    #[test]
    fn parse_html_tag_with_special_js_framework_attributes() {
        check_parse(
            r#"
                <template #slot>
                    <my-component :bind="hello" @click="onClick">
                        hello
                    </my-component>
                </template>
            "#,
            expect![[r##"
                ROOT@0..206
                  HTML_TAG@0..193
                    HTML_STARTING_TAG@0..33
                      TK_LINE_BREAK@0..1 "\n"
                      TK_WHITESPACE@1..17 "                "
                      TK_LESS_THAN@17..18 "<"
                      TK_WORD@18..26 "template"
                      HTML_ATTRIBUTE_LIST@26..32
                        HTML_ATTRIBUTE@26..32
                          TK_WHITESPACE@26..27 " "
                          TK_WORD@27..32 "#slot"
                      TK_GREATER_THAN@32..33 ">"
                    BODY@33..165
                      HTML_TAG@33..165
                        HTML_STARTING_TAG@33..99
                          TK_LINE_BREAK@33..34 "\n"
                          TK_WHITESPACE@34..54 "                    "
                          TK_LESS_THAN@54..55 "<"
                          TK_WORD@55..67 "my-component"
                          HTML_ATTRIBUTE_LIST@67..98
                            HTML_ATTRIBUTE@67..81
                              TK_WHITESPACE@67..68 " "
                              TK_WORD@68..73 ":bind"
                              TK_EQUAL@73..74 "="
                              HTML_STRING@74..81
                                TK_DOUBLE_QUOTES@74..75 "\""
                                HTML_STRING_INNER@75..80
                                  TK_WORD@75..80 "hello"
                                TK_DOUBLE_QUOTES@80..81 "\""
                            HTML_ATTRIBUTE@81..98
                              TK_WHITESPACE@81..82 " "
                              TK_WORD@82..88 "@click"
                              TK_EQUAL@88..89 "="
                              HTML_STRING@89..98
                                TK_DOUBLE_QUOTES@89..90 "\""
                                HTML_STRING_INNER@90..97
                                  TK_WORD@90..97 "onClick"
                                TK_DOUBLE_QUOTES@97..98 "\""
                          TK_GREATER_THAN@98..99 ">"
                        BODY@99..129
                          HTML_TEXT@99..129
                            TK_LINE_BREAK@99..100 "\n"
                            TK_WHITESPACE@100..124 "                        "
                            TK_WORD@124..129 "hello"
                        HTML_ENDING_TAG@129..165
                          TK_LINE_BREAK@129..130 "\n"
                          TK_WHITESPACE@130..150 "                    "
                          TK_LESS_THAN_SLASH@150..152 "</"
                          TK_WORD@152..164 "my-component"
                          TK_GREATER_THAN@164..165 ">"
                    HTML_ENDING_TAG@165..193
                      TK_LINE_BREAK@165..166 "\n"
                      TK_WHITESPACE@166..182 "                "
                      TK_LESS_THAN_SLASH@182..184 "</"
                      TK_WORD@184..192 "template"
                      TK_GREATER_THAN@192..193 ">"
                  TK_LINE_BREAK@193..194 "\n"
                  TK_WHITESPACE@194..206 "            ""##]],
        );
    }

    #[test]
    fn parse_html_tag_with_token_collision_name() {
        check_parse(
            r#"<source srcset="...">"#,
            expect![[r#"
                ROOT@0..21
                  HTML_TAG@0..21
                    HTML_STARTING_TAG@0..21
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..7 "source"
                      HTML_ATTRIBUTE_LIST@7..20
                        HTML_ATTRIBUTE@7..20
                          TK_WHITESPACE@7..8 " "
                          TK_WORD@8..14 "srcset"
                          TK_EQUAL@14..15 "="
                          HTML_STRING@15..20
                            TK_DOUBLE_QUOTES@15..16 "\""
                            HTML_STRING_INNER@16..19
                              TK_TRIPLE_DOT@16..19 "..."
                            TK_DOUBLE_QUOTES@19..20 "\""
                      TK_GREATER_THAN@20..21 ">""#]],
        );
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
        );
    }

    #[test]
    fn parse_twig_component_tag() {
        check_parse(
            r#"<twig:Alert message="Or use the fun HTML syntax!" />"#,
            expect![[r#"
                ROOT@0..52
                  HTML_TAG@0..52
                    HTML_STARTING_TAG@0..52
                      TK_LESS_THAN@0..1 "<"
                      TK_TWIG_COMPONENT_NAME@1..11 "twig:Alert"
                      HTML_ATTRIBUTE_LIST@11..49
                        HTML_ATTRIBUTE@11..49
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..19 "message"
                          TK_EQUAL@19..20 "="
                          HTML_STRING@20..49
                            TK_DOUBLE_QUOTES@20..21 "\""
                            HTML_STRING_INNER@21..48
                              TK_WORD@21..23 "Or"
                              TK_WHITESPACE@23..24 " "
                              TK_USE@24..27 "use"
                              TK_WHITESPACE@27..28 " "
                              TK_WORD@28..31 "the"
                              TK_WHITESPACE@31..32 " "
                              TK_WORD@32..35 "fun"
                              TK_WHITESPACE@35..36 " "
                              TK_WORD@36..40 "HTML"
                              TK_WHITESPACE@40..41 " "
                              TK_WORD@41..47 "syntax"
                              TK_EXCLAMATION_MARK@47..48 "!"
                            TK_DOUBLE_QUOTES@48..49 "\""
                      TK_WHITESPACE@49..50 " "
                      TK_SLASH_GREATER_THAN@50..52 "/>""#]],
        );
    }

    #[test]
    fn parse_twig_component_tag_with_body() {
        check_parse(
            r#"<twig:swButton variant="danger">My button</twig:swButton>"#,
            expect![[r#"
                ROOT@0..57
                  HTML_TAG@0..57
                    HTML_STARTING_TAG@0..32
                      TK_LESS_THAN@0..1 "<"
                      TK_TWIG_COMPONENT_NAME@1..14 "twig:swButton"
                      HTML_ATTRIBUTE_LIST@14..31
                        HTML_ATTRIBUTE@14..31
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..22 "variant"
                          TK_EQUAL@22..23 "="
                          HTML_STRING@23..31
                            TK_DOUBLE_QUOTES@23..24 "\""
                            HTML_STRING_INNER@24..30
                              TK_WORD@24..30 "danger"
                            TK_DOUBLE_QUOTES@30..31 "\""
                      TK_GREATER_THAN@31..32 ">"
                    BODY@32..41
                      HTML_TEXT@32..41
                        TK_WORD@32..34 "My"
                        TK_WHITESPACE@34..35 " "
                        TK_WORD@35..41 "button"
                    HTML_ENDING_TAG@41..57
                      TK_LESS_THAN_SLASH@41..43 "</"
                      TK_TWIG_COMPONENT_NAME@43..56 "twig:swButton"
                      TK_GREATER_THAN@56..57 ">""#]],
        );
    }

    #[test]
    fn parse_twig_component_namespaced_tag() {
        check_parse(
            r#"<twig:Sw:Product:WishlistButton :messageParam="variable">hello</twig:Sw:Product:WishlistButton>"#,
            expect![[r#"
                ROOT@0..95
                  HTML_TAG@0..95
                    HTML_STARTING_TAG@0..57
                      TK_LESS_THAN@0..1 "<"
                      TK_TWIG_COMPONENT_NAME@1..31 "twig:Sw:Product:Wishl ..."
                      HTML_ATTRIBUTE_LIST@31..56
                        HTML_ATTRIBUTE@31..56
                          TK_WHITESPACE@31..32 " "
                          TK_WORD@32..45 ":messageParam"
                          TK_EQUAL@45..46 "="
                          HTML_STRING@46..56
                            TK_DOUBLE_QUOTES@46..47 "\""
                            HTML_STRING_INNER@47..55
                              TK_WORD@47..55 "variable"
                            TK_DOUBLE_QUOTES@55..56 "\""
                      TK_GREATER_THAN@56..57 ">"
                    BODY@57..62
                      HTML_TEXT@57..62
                        TK_WORD@57..62 "hello"
                    HTML_ENDING_TAG@62..95
                      TK_LESS_THAN_SLASH@62..64 "</"
                      TK_TWIG_COMPONENT_NAME@64..94 "twig:Sw:Product:Wishl ..."
                      TK_GREATER_THAN@94..95 ">""#]],
        );
    }

    #[test]
    fn parse_twig_component_nested() {
        check_parse(
            r#"<twig:swCard variant="large"><twig:swButton variant="primary">click me</twig:swButton></twig:swCard>"#,
            expect![[r#"
                ROOT@0..100
                  HTML_TAG@0..100
                    HTML_STARTING_TAG@0..29
                      TK_LESS_THAN@0..1 "<"
                      TK_TWIG_COMPONENT_NAME@1..12 "twig:swCard"
                      HTML_ATTRIBUTE_LIST@12..28
                        HTML_ATTRIBUTE@12..28
                          TK_WHITESPACE@12..13 " "
                          TK_WORD@13..20 "variant"
                          TK_EQUAL@20..21 "="
                          HTML_STRING@21..28
                            TK_DOUBLE_QUOTES@21..22 "\""
                            HTML_STRING_INNER@22..27
                              TK_WORD@22..27 "large"
                            TK_DOUBLE_QUOTES@27..28 "\""
                      TK_GREATER_THAN@28..29 ">"
                    BODY@29..86
                      HTML_TAG@29..86
                        HTML_STARTING_TAG@29..62
                          TK_LESS_THAN@29..30 "<"
                          TK_TWIG_COMPONENT_NAME@30..43 "twig:swButton"
                          HTML_ATTRIBUTE_LIST@43..61
                            HTML_ATTRIBUTE@43..61
                              TK_WHITESPACE@43..44 " "
                              TK_WORD@44..51 "variant"
                              TK_EQUAL@51..52 "="
                              HTML_STRING@52..61
                                TK_DOUBLE_QUOTES@52..53 "\""
                                HTML_STRING_INNER@53..60
                                  TK_WORD@53..60 "primary"
                                TK_DOUBLE_QUOTES@60..61 "\""
                          TK_GREATER_THAN@61..62 ">"
                        BODY@62..70
                          HTML_TEXT@62..70
                            TK_WORD@62..67 "click"
                            TK_WHITESPACE@67..68 " "
                            TK_WORD@68..70 "me"
                        HTML_ENDING_TAG@70..86
                          TK_LESS_THAN_SLASH@70..72 "</"
                          TK_TWIG_COMPONENT_NAME@72..85 "twig:swButton"
                          TK_GREATER_THAN@85..86 ">"
                    HTML_ENDING_TAG@86..100
                      TK_LESS_THAN_SLASH@86..88 "</"
                      TK_TWIG_COMPONENT_NAME@88..99 "twig:swCard"
                      TK_GREATER_THAN@99..100 ">""#]],
        );
    }

    #[test]
    fn parse_text_with_lesser_than_sign_inside() {
        check_parse(
            "<p>
    downloads < 100
</p>",
            expect![[r#"
                ROOT@0..28
                  HTML_TAG@0..28
                    HTML_STARTING_TAG@0..3
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..2 "p"
                      HTML_ATTRIBUTE_LIST@2..2
                      TK_GREATER_THAN@2..3 ">"
                    BODY@3..23
                      HTML_TEXT@3..23
                        TK_LINE_BREAK@3..4 "\n"
                        TK_WHITESPACE@4..8 "    "
                        TK_WORD@8..17 "downloads"
                        TK_WHITESPACE@17..18 " "
                        TK_LESS_THAN@18..19 "<"
                        TK_WHITESPACE@19..20 " "
                        TK_NUMBER@20..23 "100"
                    HTML_ENDING_TAG@23..28
                      TK_LINE_BREAK@23..24 "\n"
                      TK_LESS_THAN_SLASH@24..26 "</"
                      TK_WORD@26..27 "p"
                      TK_GREATER_THAN@27..28 ">""#]],
        );
    }

    #[test]
    fn parse_less_than_sign_followed_by_ws_and_text() {
        check_parse(
            "<p>
        < 100
</p>",
            expect![[r#"
                ROOT@0..22
                  HTML_TAG@0..22
                    HTML_STARTING_TAG@0..3
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..2 "p"
                      HTML_ATTRIBUTE_LIST@2..2
                      TK_GREATER_THAN@2..3 ">"
                    BODY@3..17
                      HTML_TEXT@3..17
                        TK_LINE_BREAK@3..4 "\n"
                        TK_WHITESPACE@4..12 "        "
                        TK_LESS_THAN@12..13 "<"
                        TK_WHITESPACE@13..14 " "
                        TK_NUMBER@14..17 "100"
                    HTML_ENDING_TAG@17..22
                      TK_LINE_BREAK@17..18 "\n"
                      TK_LESS_THAN_SLASH@18..20 "</"
                      TK_WORD@20..21 "p"
                      TK_GREATER_THAN@21..22 ">""#]],
        );
    }

    #[test]
    fn parse_alone_standing_less_than_sign() {
        check_parse(
            "{% block s %}<{% endblock %}",
            expect![[r#"
                ROOT@0..28
                  TWIG_BLOCK@0..28
                    TWIG_STARTING_BLOCK@0..13
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..10 "s"
                      TK_WHITESPACE@10..11 " "
                      TK_PERCENT_CURLY@11..13 "%}"
                    BODY@13..14
                      HTML_TEXT@13..14
                        TK_LESS_THAN@13..14 "<"
                    TWIG_ENDING_BLOCK@14..28
                      TK_CURLY_PERCENT@14..16 "{%"
                      TK_WHITESPACE@16..17 " "
                      TK_ENDBLOCK@17..25 "endblock"
                      TK_WHITESPACE@25..26 " "
                      TK_PERCENT_CURLY@26..28 "%}""#]],
        );
    }

    #[test]
    fn parse_less_than_sign_eof() {
        check_parse(
            "downloads <",
            expect![[r#"
                ROOT@0..11
                  HTML_TEXT@0..11
                    TK_WORD@0..9 "downloads"
                    TK_WHITESPACE@9..10 " "
                    TK_LESS_THAN@10..11 "<""#]],
        );
    }

    #[test]
    fn parse_less_than_number() {
        check_parse(
            "downloads <100",
            expect![[r#"
                ROOT@0..14
                  HTML_TEXT@0..14
                    TK_WORD@0..9 "downloads"
                    TK_WHITESPACE@9..10 " "
                    TK_LESS_THAN@10..11 "<"
                    TK_NUMBER@11..14 "100""#]],
        );
    }

    #[test]
    fn parse_inline_style_tag() {
        check_parse(
            r"<style>
            body {
                font-family: Arial, sans-serif;
                line-height: 1.6;
                color: #333;
                max-width: 1200px;
                margin: 0 auto;
                padding: 20px;
            }
            h1, h2 {
                color: {{ myTwigColor }};
            }
            .issue {
                background-color: #f8f9fa;
                border-left: 4px solid #007bff;
                padding: 10px;
                margin-bottom: 20px;
            }
            code {
                white-space: pre;
            }
            </style>",
            expect![[r##"
                ROOT@0..608
                  HTML_TAG@0..608
                    HTML_STARTING_TAG@0..7
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..6 "style"
                      HTML_ATTRIBUTE_LIST@6..6
                      TK_GREATER_THAN@6..7 ">"
                    BODY@7..587
                      HTML_RAW_TEXT@7..587
                        TK_LINE_BREAK@7..8 "\n"
                        TK_WHITESPACE@8..20 "            "
                        TK_WORD@20..24 "body"
                        TK_WHITESPACE@24..25 " "
                        TK_OPEN_CURLY@25..26 "{"
                        TK_LINE_BREAK@26..27 "\n"
                        TK_WHITESPACE@27..43 "                "
                        TK_WORD@43..54 "font-family"
                        TK_COLON@54..55 ":"
                        TK_WHITESPACE@55..56 " "
                        TK_WORD@56..61 "Arial"
                        TK_COMMA@61..62 ","
                        TK_WHITESPACE@62..63 " "
                        TK_WORD@63..73 "sans-serif"
                        TK_SEMICOLON@73..74 ";"
                        TK_LINE_BREAK@74..75 "\n"
                        TK_WHITESPACE@75..91 "                "
                        TK_WORD@91..102 "line-height"
                        TK_COLON@102..103 ":"
                        TK_WHITESPACE@103..104 " "
                        TK_NUMBER@104..107 "1.6"
                        TK_SEMICOLON@107..108 ";"
                        TK_LINE_BREAK@108..109 "\n"
                        TK_WHITESPACE@109..125 "                "
                        TK_WORD@125..130 "color"
                        TK_COLON@130..131 ":"
                        TK_WHITESPACE@131..132 " "
                        TK_HASHTAG@132..133 "#"
                        TK_NUMBER@133..136 "333"
                        TK_SEMICOLON@136..137 ";"
                        TK_LINE_BREAK@137..138 "\n"
                        TK_WHITESPACE@138..154 "                "
                        TK_WORD@154..163 "max-width"
                        TK_COLON@163..164 ":"
                        TK_WHITESPACE@164..165 " "
                        TK_NUMBER@165..169 "1200"
                        TK_WORD@169..171 "px"
                        TK_SEMICOLON@171..172 ";"
                        TK_LINE_BREAK@172..173 "\n"
                        TK_WHITESPACE@173..189 "                "
                        TK_WORD@189..195 "margin"
                        TK_COLON@195..196 ":"
                        TK_WHITESPACE@196..197 " "
                        TK_NUMBER@197..198 "0"
                        TK_WHITESPACE@198..199 " "
                        TK_WORD@199..203 "auto"
                        TK_SEMICOLON@203..204 ";"
                        TK_LINE_BREAK@204..205 "\n"
                        TK_WHITESPACE@205..221 "                "
                        TK_WORD@221..228 "padding"
                        TK_COLON@228..229 ":"
                        TK_WHITESPACE@229..230 " "
                        TK_NUMBER@230..232 "20"
                        TK_WORD@232..234 "px"
                        TK_SEMICOLON@234..235 ";"
                        TK_LINE_BREAK@235..236 "\n"
                        TK_WHITESPACE@236..248 "            "
                        TK_CLOSE_CURLY@248..249 "}"
                        TK_LINE_BREAK@249..250 "\n"
                        TK_WHITESPACE@250..262 "            "
                        TK_WORD@262..264 "h1"
                        TK_COMMA@264..265 ","
                        TK_WHITESPACE@265..266 " "
                        TK_WORD@266..268 "h2"
                        TK_WHITESPACE@268..269 " "
                        TK_OPEN_CURLY@269..270 "{"
                        TK_LINE_BREAK@270..271 "\n"
                        TK_WHITESPACE@271..287 "                "
                        TK_WORD@287..292 "color"
                        TK_COLON@292..293 ":"
                        TWIG_VAR@293..311
                          TK_WHITESPACE@293..294 " "
                          TK_OPEN_CURLY_CURLY@294..296 "{{"
                          TWIG_EXPRESSION@296..308
                            TWIG_LITERAL_NAME@296..308
                              TK_WHITESPACE@296..297 " "
                              TK_WORD@297..308 "myTwigColor"
                          TK_WHITESPACE@308..309 " "
                          TK_CLOSE_CURLY_CURLY@309..311 "}}"
                        TK_SEMICOLON@311..312 ";"
                        TK_LINE_BREAK@312..313 "\n"
                        TK_WHITESPACE@313..325 "            "
                        TK_CLOSE_CURLY@325..326 "}"
                        TK_LINE_BREAK@326..327 "\n"
                        TK_WHITESPACE@327..339 "            "
                        TK_DOT@339..340 "."
                        TK_WORD@340..345 "issue"
                        TK_WHITESPACE@345..346 " "
                        TK_OPEN_CURLY@346..347 "{"
                        TK_LINE_BREAK@347..348 "\n"
                        TK_WHITESPACE@348..364 "                "
                        TK_WORD@364..380 "background-color"
                        TK_COLON@380..381 ":"
                        TK_WHITESPACE@381..382 " "
                        TK_WORD@382..389 "#f8f9fa"
                        TK_SEMICOLON@389..390 ";"
                        TK_LINE_BREAK@390..391 "\n"
                        TK_WHITESPACE@391..407 "                "
                        TK_WORD@407..418 "border-left"
                        TK_COLON@418..419 ":"
                        TK_WHITESPACE@419..420 " "
                        TK_NUMBER@420..421 "4"
                        TK_WORD@421..423 "px"
                        TK_WHITESPACE@423..424 " "
                        TK_WORD@424..429 "solid"
                        TK_WHITESPACE@429..430 " "
                        TK_HASHTAG@430..431 "#"
                        TK_NUMBER@431..434 "007"
                        TK_WORD@434..437 "bff"
                        TK_SEMICOLON@437..438 ";"
                        TK_LINE_BREAK@438..439 "\n"
                        TK_WHITESPACE@439..455 "                "
                        TK_WORD@455..462 "padding"
                        TK_COLON@462..463 ":"
                        TK_WHITESPACE@463..464 " "
                        TK_NUMBER@464..466 "10"
                        TK_WORD@466..468 "px"
                        TK_SEMICOLON@468..469 ";"
                        TK_LINE_BREAK@469..470 "\n"
                        TK_WHITESPACE@470..486 "                "
                        TK_WORD@486..499 "margin-bottom"
                        TK_COLON@499..500 ":"
                        TK_WHITESPACE@500..501 " "
                        TK_NUMBER@501..503 "20"
                        TK_WORD@503..505 "px"
                        TK_SEMICOLON@505..506 ";"
                        TK_LINE_BREAK@506..507 "\n"
                        TK_WHITESPACE@507..519 "            "
                        TK_CLOSE_CURLY@519..520 "}"
                        TK_LINE_BREAK@520..521 "\n"
                        TK_WHITESPACE@521..533 "            "
                        TK_WORD@533..537 "code"
                        TK_WHITESPACE@537..538 " "
                        TK_OPEN_CURLY@538..539 "{"
                        TK_LINE_BREAK@539..540 "\n"
                        TK_WHITESPACE@540..556 "                "
                        TK_WORD@556..567 "white-space"
                        TK_COLON@567..568 ":"
                        TK_WHITESPACE@568..569 " "
                        TK_WORD@569..572 "pre"
                        TK_SEMICOLON@572..573 ";"
                        TK_LINE_BREAK@573..574 "\n"
                        TK_WHITESPACE@574..586 "            "
                        TK_CLOSE_CURLY@586..587 "}"
                    HTML_ENDING_TAG@587..608
                      TK_LINE_BREAK@587..588 "\n"
                      TK_WHITESPACE@588..600 "            "
                      TK_LESS_THAN_SLASH@600..602 "</"
                      TK_WORD@602..607 "style"
                      TK_GREATER_THAN@607..608 ">""##]],
        );
    }

    #[test]
    fn parse_inline_script_tag() {
        // check if letter casing matters
        check_parse(
            r#"<ScrIpt type="text/javascript">
                document.getElementById("demo").innerHTML = "<h1>Hello JavaScript!</h1>";
            </ScrIpt>"#,
            expect![[r#"
                ROOT@0..143
                  HTML_TAG@0..143
                    HTML_STARTING_TAG@0..31
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..7 "ScrIpt"
                      HTML_ATTRIBUTE_LIST@7..30
                        HTML_ATTRIBUTE@7..30
                          TK_WHITESPACE@7..8 " "
                          TK_WORD@8..12 "type"
                          TK_EQUAL@12..13 "="
                          HTML_STRING@13..30
                            TK_DOUBLE_QUOTES@13..14 "\""
                            HTML_STRING_INNER@14..29
                              TK_WORD@14..18 "text"
                              TK_FORWARD_SLASH@18..19 "/"
                              TK_WORD@19..29 "javascript"
                            TK_DOUBLE_QUOTES@29..30 "\""
                      TK_GREATER_THAN@30..31 ">"
                    BODY@31..121
                      HTML_RAW_TEXT@31..121
                        TK_LINE_BREAK@31..32 "\n"
                        TK_WHITESPACE@32..48 "                "
                        TK_WORD@48..56 "document"
                        TK_DOT@56..57 "."
                        TK_WORD@57..71 "getElementById"
                        TK_OPEN_PARENTHESIS@71..72 "("
                        TK_DOUBLE_QUOTES@72..73 "\""
                        TK_WORD@73..77 "demo"
                        TK_DOUBLE_QUOTES@77..78 "\""
                        TK_CLOSE_PARENTHESIS@78..79 ")"
                        TK_DOT@79..80 "."
                        TK_WORD@80..89 "innerHTML"
                        TK_WHITESPACE@89..90 " "
                        TK_EQUAL@90..91 "="
                        TK_WHITESPACE@91..92 " "
                        TK_DOUBLE_QUOTES@92..93 "\""
                        TK_LESS_THAN@93..94 "<"
                        TK_WORD@94..96 "h1"
                        TK_GREATER_THAN@96..97 ">"
                        TK_WORD@97..102 "Hello"
                        TK_WHITESPACE@102..103 " "
                        TK_WORD@103..113 "JavaScript"
                        TK_EXCLAMATION_MARK@113..114 "!"
                        TK_LESS_THAN_SLASH@114..116 "</"
                        TK_WORD@116..118 "h1"
                        TK_GREATER_THAN@118..119 ">"
                        TK_DOUBLE_QUOTES@119..120 "\""
                        TK_SEMICOLON@120..121 ";"
                    HTML_ENDING_TAG@121..143
                      TK_LINE_BREAK@121..122 "\n"
                      TK_WHITESPACE@122..134 "            "
                      TK_LESS_THAN_SLASH@134..136 "</"
                      TK_WORD@136..142 "ScrIpt"
                      TK_GREATER_THAN@142..143 ">""#]],
        );
    }

    #[test]
    fn parse_inline_textarea_tag() {
        check_parse(
            r#"<textarea name="myTextarea" rows="4" cols="50">
                some text. {{ name }} was here. {% block changeme %}this can be overriden{% endblock %}
            </textarea>"#,
            expect![[r#"
                ROOT@0..175
                  HTML_TAG@0..175
                    HTML_STARTING_TAG@0..47
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..9 "textarea"
                      HTML_ATTRIBUTE_LIST@9..46
                        HTML_ATTRIBUTE@9..27
                          TK_WHITESPACE@9..10 " "
                          TK_WORD@10..14 "name"
                          TK_EQUAL@14..15 "="
                          HTML_STRING@15..27
                            TK_DOUBLE_QUOTES@15..16 "\""
                            HTML_STRING_INNER@16..26
                              TK_WORD@16..26 "myTextarea"
                            TK_DOUBLE_QUOTES@26..27 "\""
                        HTML_ATTRIBUTE@27..36
                          TK_WHITESPACE@27..28 " "
                          TK_WORD@28..32 "rows"
                          TK_EQUAL@32..33 "="
                          HTML_STRING@33..36
                            TK_DOUBLE_QUOTES@33..34 "\""
                            HTML_STRING_INNER@34..35
                              TK_NUMBER@34..35 "4"
                            TK_DOUBLE_QUOTES@35..36 "\""
                        HTML_ATTRIBUTE@36..46
                          TK_WHITESPACE@36..37 " "
                          TK_WORD@37..41 "cols"
                          TK_EQUAL@41..42 "="
                          HTML_STRING@42..46
                            TK_DOUBLE_QUOTES@42..43 "\""
                            HTML_STRING_INNER@43..45
                              TK_NUMBER@43..45 "50"
                            TK_DOUBLE_QUOTES@45..46 "\""
                      TK_GREATER_THAN@46..47 ">"
                    BODY@47..151
                      HTML_RAW_TEXT@47..151
                        TK_LINE_BREAK@47..48 "\n"
                        TK_WHITESPACE@48..64 "                "
                        TK_WORD@64..68 "some"
                        TK_WHITESPACE@68..69 " "
                        TK_WORD@69..73 "text"
                        TK_DOT@73..74 "."
                        TWIG_VAR@74..85
                          TK_WHITESPACE@74..75 " "
                          TK_OPEN_CURLY_CURLY@75..77 "{{"
                          TWIG_EXPRESSION@77..82
                            TWIG_LITERAL_NAME@77..82
                              TK_WHITESPACE@77..78 " "
                              TK_WORD@78..82 "name"
                          TK_WHITESPACE@82..83 " "
                          TK_CLOSE_CURLY_CURLY@83..85 "}}"
                        TK_WHITESPACE@85..86 " "
                        TK_WORD@86..89 "was"
                        TK_WHITESPACE@89..90 " "
                        TK_WORD@90..94 "here"
                        TK_DOT@94..95 "."
                        TWIG_BLOCK@95..151
                          TWIG_STARTING_BLOCK@95..116
                            TK_WHITESPACE@95..96 " "
                            TK_CURLY_PERCENT@96..98 "{%"
                            TK_WHITESPACE@98..99 " "
                            TK_BLOCK@99..104 "block"
                            TK_WHITESPACE@104..105 " "
                            TK_WORD@105..113 "changeme"
                            TK_WHITESPACE@113..114 " "
                            TK_PERCENT_CURLY@114..116 "%}"
                          BODY@116..137
                            HTML_RAW_TEXT@116..137
                              TK_WORD@116..120 "this"
                              TK_WHITESPACE@120..121 " "
                              TK_WORD@121..124 "can"
                              TK_WHITESPACE@124..125 " "
                              TK_WORD@125..127 "be"
                              TK_WHITESPACE@127..128 " "
                              TK_WORD@128..137 "overriden"
                          TWIG_ENDING_BLOCK@137..151
                            TK_CURLY_PERCENT@137..139 "{%"
                            TK_WHITESPACE@139..140 " "
                            TK_ENDBLOCK@140..148 "endblock"
                            TK_WHITESPACE@148..149 " "
                            TK_PERCENT_CURLY@149..151 "%}"
                    HTML_ENDING_TAG@151..175
                      TK_LINE_BREAK@151..152 "\n"
                      TK_WHITESPACE@152..164 "            "
                      TK_LESS_THAN_SLASH@164..166 "</"
                      TK_WORD@166..174 "textarea"
                      TK_GREATER_THAN@174..175 ">""#]],
        );
    }

    #[test]
    fn parse_inline_title_tag() {
        check_parse(
            "<title>{{ title }} is awesome</title>",
            expect![[r#"
                ROOT@0..37
                  HTML_TAG@0..37
                    HTML_STARTING_TAG@0..7
                      TK_LESS_THAN@0..1 "<"
                      TK_WORD@1..6 "title"
                      HTML_ATTRIBUTE_LIST@6..6
                      TK_GREATER_THAN@6..7 ">"
                    BODY@7..29
                      HTML_RAW_TEXT@7..29
                        TWIG_VAR@7..18
                          TK_OPEN_CURLY_CURLY@7..9 "{{"
                          TWIG_EXPRESSION@9..15
                            TWIG_LITERAL_NAME@9..15
                              TK_WHITESPACE@9..10 " "
                              TK_WORD@10..15 "title"
                          TK_WHITESPACE@15..16 " "
                          TK_CLOSE_CURLY_CURLY@16..18 "}}"
                        TK_WHITESPACE@18..19 " "
                        TK_IS@19..21 "is"
                        TK_WHITESPACE@21..22 " "
                        TK_WORD@22..29 "awesome"
                    HTML_ENDING_TAG@29..37
                      TK_LESS_THAN_SLASH@29..31 "</"
                      TK_WORD@31..36 "title"
                      TK_GREATER_THAN@36..37 ">""#]],
        );
    }
}

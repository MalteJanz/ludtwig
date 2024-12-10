use logos::Logos;

use crate::syntax::untyped::{SyntaxKind, TextRange, TextSize};

/// Lex the source code into a Vec of tokens with their corresponding span (position in source code).
/// These tokens are produced by a dumb lexer and don't have any meaning / semantic attached to them.
pub(crate) fn lex(source: &str) -> Vec<Token> {
    let mut lexer = SyntaxKind::lexer(source);
    let mut result = vec![];

    while let Some(kind) = lexer.next() {
        let range = {
            let span = lexer.span();
            let start = TextSize::try_from(span.start)
                .expect("lexer span range should fit into a u32 (file should be smaller than 4GB)");
            let end = TextSize::try_from(span.end)
                .expect("lexer span range should fit into a u32 (file should be smaller than 4GB)");
            TextRange::new(start, end)
        };

        let kind = kind.unwrap_or(SyntaxKind::TK_UNKNOWN);
        result.push(Token {
            kind,
            text: lexer.slice(),
            range,
        });
    }

    result
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Token<'source> {
    pub(crate) kind: SyntaxKind,
    pub(crate) text: &'source str,
    pub(crate) range: TextRange,
}

// ToDo: might be able to remove this annotation in future rust-analyzer version
#[allow(clippy::needless_lifetimes)]
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
    use crate::syntax::untyped::TextLen;
    use crate::T;

    use super::*;

    fn check_regex(input: &str, kind: SyntaxKind, display: &str) {
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

        // SyntaxKind display implementation should be there and match
        assert_eq!(display, format!("{}", lexer_results[0].kind));
    }

    fn check_token(input: &str, kind: SyntaxKind) {
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

        // compare SyntaxKind display implementation
        assert_eq!(input, format!("{}", lexer_results[0].kind));
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
    fn lex_simple_expression() {
        let results = lex("{{ not a }}");
        let syntax_kinds: Vec<SyntaxKind> = results.into_iter().map(|t| t.kind).collect();

        assert_eq!(
            syntax_kinds,
            vec![
                T!["{{"],
                T![ws],
                T!["not"],
                T![ws],
                T![word],
                T![ws],
                T!["}}"],
            ]
        );
    }

    #[test]
    fn lex_hashtag_curly_curly() {
        let results = lex("#{{");
        let syntax_kinds: Vec<SyntaxKind> = results.into_iter().map(|t| t.kind).collect();

        assert_eq!(syntax_kinds, vec![T!["#"], T!["{{"],]);
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn lex_all_tokens_chained_together() {
        use std::fmt::Write;

        let mut source = String::new();
        let mut expected_kinds: Vec<SyntaxKind> = vec![];
        let mut add = |text: &str, kind: SyntaxKind| {
            write!(source, "{text} ").unwrap();
            expected_kinds.push(kind);
            expected_kinds.push(T![ws]);
        };

        // add every token here (except ws)
        add("\n", T![lb]);
        add("word", T![word]);
        add("42.3", T![number]);
        add("&#10;", T![html escape character]);
        add(".", T!["."]);
        add("..", T![".."]);
        add(",", T![","]);
        add(":", T![":"]);
        add(";", T![";"]);
        add("!", T!["!"]);
        add("!=", T!["!="]);
        add("!==", T!["!=="]);
        add("?", T!["?"]);
        add("??", T!["??"]);
        add("%", T!["%"]);
        add("~", T!["~"]);
        add("|", T!["|"]);
        add("||", T!["||"]);
        add("&", T!["&"]);
        add("&&", T!["&&"]);
        add("/", T!["/"]);
        add("//", T!["//"]);
        add("\\", T!["\\"]);
        add("(", T!["("]);
        add(")", T![")"]);
        add("{", T!["{"]);
        add("}", T!["}"]);
        add("[", T!["["]);
        add("]", T!["]"]);
        add("<", T!["<"]);
        add("<=", T!["<="]);
        add("<=>", T!["<=>"]);
        add("</", T!["</"]);
        add("<!", T!["<!"]);
        add("doctype", T!["DOCTYPE"]);
        add(">", T![">"]);
        add(">=", T![">="]);
        add("=>", T!["=>"]);
        add("/>", T!["/>"]);
        add("<!--", T!["<!--"]);
        add("-->", T!["-->"]);
        add("=", T!["="]);
        add("==", T!["=="]);
        add("===", T!["==="]);
        add("+", T!["+"]);
        add("-", T!["-"]);
        add("*", T!["*"]);
        add("**", T!["**"]);
        add("\"", T!["\""]);
        add("'", T!["'"]);
        add("`", T!["`"]);
        add("{%", T!["{%"]);
        add("%}", T!["%}"]);
        add("{{", T!["{{"]);
        add("}}", T!["}}"]);
        add("{#", T!["{#"]);
        add("#", T!["#"]);
        add("#}", T!["#}"]);
        add("true", T!["true"]);
        add("false", T!["false"]);
        add("block", T!["block"]);
        add("endblock", T!["endblock"]);
        add("if", T!["if"]);
        add("elseif", T!["elseif"]);
        add("else", T!["else"]);
        add("endif", T!["endif"]);
        add("apply", T!["apply"]);
        add("endapply", T!["endapply"]);
        add("autoescape", T!["autoescape"]);
        add("endautoescape", T!["endautoescape"]);
        add("cache", T!["cache"]);
        add("endcache", T!["endcache"]);
        add("deprecated", T!["deprecated"]);
        add("do", T!["do"]);
        add("embed", T!["embed"]);
        add("endembed", T!["endembed"]);
        add("extends", T!["extends"]);
        add("flush", T!["flush"]);
        add("for", T!["for"]);
        add("endfor", T!["endfor"]);
        add("from", T!["from"]);
        add("import", T!["import"]);
        add("macro", T!["macro"]);
        add("endmacro", T!["endmacro"]);
        add("sandbox", T!["sandbox"]);
        add("endsandbox", T!["endsandbox"]);
        add("set", T!["set"]);
        add("endset", T!["endset"]);
        add("use", T!["use"]);
        add("verbatim", T!["verbatim"]);
        add("endverbatim", T!["endverbatim"]);
        add("only", T!["only"]);
        add("ignore missing", T!["ignore missing"]);
        add("with", T!["with"]);
        add("endwith", T!["endwith"]);
        add("ttl", T!["ttl"]);
        add("tags", T!["tags"]);
        add("not", T!["not"]);
        add("or", T!["or"]);
        add("and", T!["and"]);
        add("b-or", T!["b-or"]);
        add("b-xor", T!["b-xor"]);
        add("b-and", T!["b-and"]);
        add("in", T!["in"]);
        add("matches", T!["matches"]);
        add("starts with", T!["starts with"]);
        add("ends with", T!["ends with"]);
        add("is", T!["is"]);
        add("even", T!["even"]);
        add("odd", T!["odd"]);
        add("defined", T!["defined"]);
        add("same as", T!["same as"]);
        add("as", T!["as"]);
        add("none", T!["none"]);
        add("null", T!["null"]);
        add("divisible by", T!["divisible by"]);
        add("constant", T!["constant"]);
        add("empty", T!["empty"]);
        add("iterable", T!["iterable"]);
        add("max", T!["max"]);
        add("min", T!["min"]);
        add("range", T!["range"]);
        add("cycle", T!["cycle"]);
        add("random", T!["random"]);
        add("date", T!["date"]);
        add("include", T!["include"]);
        add("source", T!["source"]);
        add("sw_extends", T!["sw_extends"]);
        add("sw_silent_feature_call", T!["sw_silent_feature_call"]);
        add("endsw_silent_feature_call", T!["endsw_silent_feature_call"]);
        add("sw_include", T!["sw_include"]);
        add("return", T!["return"]);
        add("sw_icon", T!["sw_icon"]);
        add("sw_thumbnails", T!["sw_thumbnails"]);
        add("style", T!["style"]);
        add("ludtwig-ignore-file", T!["ludtwig-ignore-file"]);
        add("ludtwig-ignore", T!["ludtwig-ignore"]);
        add("€", T![unknown]);
        add("trans", T!["trans"]);
        add("endtrans", T!["endtrans"]);

        // lex and compare
        let results = lex(&source);
        let found_syntax_kinds: Vec<SyntaxKind> = results.into_iter().map(|t| t.kind).collect();
        assert_eq!(found_syntax_kinds, expected_kinds);
    }

    #[test]
    fn lex_whitespace() {
        check_regex("   ", T![ws], "whitespace");
        check_regex(" \t  ", T![ws], "whitespace");
        check_regex("\t", T![ws], "whitespace");
    }

    #[test]
    fn lex_line_break() {
        check_regex("\n", T![lb], "line break");
        check_regex("\n\n", T![lb], "line break");
        check_regex("\r\n", T![lb], "line break");
        check_regex("\r\n\r\n", T![lb], "line break");
        check_regex("\r\n\n\r\n", T![lb], "line break");
    }

    #[test]
    fn lex_word() {
        check_regex("hello", T![word], "word");
        check_regex("hello123", T![word], "word");
        check_regex("camelCase", T![word], "word");
        check_regex("kebab-case", T![word], "word");
        check_regex("snake_case", T![word], "word");
        check_regex("#hello123", T![word], "word");
        check_regex("@hello123", T![word], "word");
        check_regex("block1", T![word], "word");
        check_regex("block_", T![word], "word");
        check_regex("blocks", T![word], "word");
        check_regex("_blank", T![word], "word");
        check_regex("$special", T![word], "word");
    }

    #[test]
    fn lex_number() {
        check_regex("123", T![number], "number");
        check_regex("0.0", T![number], "number");
        check_regex("3.123456789", T![number], "number");
        check_regex("3e+2", T![number], "number");
        check_regex("3e-2", T![number], "number");
        check_regex("10E-7", T![number], "number");
        check_regex("10E+6", T![number], "number");
        check_regex("1.23E+10", T![number], "number");
    }

    #[test]
    fn lex_html_escape_character() {
        check_regex(
            "&NewLine;",
            T![html escape character],
            "html escape character",
        );
        check_regex("&nbsp;", T![html escape character], "html escape character");
        check_regex("&#39;", T![html escape character], "html escape character");
        check_regex(
            "&#8721;",
            T![html escape character],
            "html escape character",
        );
        check_regex("&sup3;", T![html escape character], "html escape character");
        check_regex(
            "&#x00B3;",
            T![html escape character],
            "html escape character",
        );
    }

    #[test]
    fn lex_dot() {
        check_token(".", T!["."]);
    }

    #[test]
    fn lex_double_dot() {
        check_token("..", T![".."]);
    }

    #[test]
    fn lex_comma() {
        check_token(",", T![","]);
    }

    #[test]
    fn lex_colon() {
        check_token(":", T![":"]);
    }

    #[test]
    fn lex_semicolon() {
        check_token(";", T![";"]);
    }

    #[test]
    fn lex_exclamation_mark() {
        check_token("!", T!["!"]);
    }

    #[test]
    fn lex_exclamation_mark_equals() {
        check_token("!=", T!["!="]);
    }

    #[test]
    fn lex_exclamation_mark_double_equals() {
        check_token("!==", T!["!=="]);
    }

    #[test]
    fn lex_question_mark() {
        check_token("?", T!["?"]);
    }

    #[test]
    fn lex_double_question_mark() {
        check_token("??", T!["??"]);
    }

    #[test]
    fn lex_percent() {
        check_token("%", T!["%"]);
    }

    #[test]
    fn lex_tilde() {
        check_token("~", T!["~"]);
    }

    #[test]
    fn lex_single_pipe() {
        check_token("|", T!["|"]);
    }

    #[test]
    fn lex_double_pipe() {
        check_token("||", T!["||"]);
    }

    #[test]
    fn lex_ampersand() {
        check_token("&", T!["&"]);
    }

    #[test]
    fn lex_double_ampersand() {
        check_token("&&", T!["&&"]);
    }

    #[test]
    fn lex_forward_slash() {
        check_token("/", T!["/"]);
    }

    #[test]
    fn lex_double_forward_slash() {
        check_token("//", T!["//"]);
    }

    #[test]
    fn lex_backward_slash() {
        check_token("\\", T!["\\"]);
    }

    #[test]
    fn lex_open_parenthesis() {
        check_token("(", T!["("]);
    }

    #[test]
    fn lex_close_parenthesis() {
        check_token(")", T![")"]);
    }

    #[test]
    fn lex_open_curly() {
        check_token("{", T!["{"]);
    }

    #[test]
    fn lex_close_curly() {
        check_token("}", T!["}"]);
    }

    #[test]
    fn lex_open_square() {
        check_token("[", T!["["]);
    }

    #[test]
    fn lex_close_square() {
        check_token("]", T!["]"]);
    }

    #[test]
    fn lex_less_than() {
        check_token("<", T!["<"]);
    }

    #[test]
    fn lex_less_than_equal() {
        check_token("<=", T!["<="]);
    }

    #[test]
    fn lex_less_than_equal_greater_than() {
        check_token("<=>", T!["<=>"]);
    }

    #[test]
    fn lex_less_than_slash() {
        check_token("</", T!["</"]);
    }

    #[test]
    fn lex_less_than_exclamation_mark() {
        check_token("<!", T!["<!"]);
    }

    #[test]
    fn lex_doctype() {
        check_token("DOCTYPE", T!["DOCTYPE"]);
    }

    #[test]
    fn lex_greater_than() {
        check_token(">", T![">"]);
    }

    #[test]
    fn lex_greater_than_equal() {
        check_token(">=", T![">="]);
    }

    #[test]
    fn lex_equal_greater_than() {
        check_token("=>", T!["=>"]);
    }

    #[test]
    fn lex_slash_greater_than() {
        check_token("/>", T!["/>"]);
    }

    #[test]
    fn lex_less_than_exclamation_mark_minus_minus() {
        check_token("<!--", T!["<!--"]);
    }

    #[test]
    fn lex_minus_minus_greater_than() {
        check_token("-->", T!["-->"]);
    }

    #[test]
    fn lex_equal() {
        check_token("=", T!["="]);
    }

    #[test]
    fn lex_double_equal() {
        check_token("==", T!["=="]);
    }

    #[test]
    fn lex_triple_equal() {
        check_token("===", T!["==="]);
    }

    #[test]
    fn lex_plus() {
        check_token("+", T!["+"]);
    }

    #[test]
    fn lex_minus() {
        check_token("-", T!["-"]);
    }

    #[test]
    fn lex_star() {
        check_token("*", T!["*"]);
    }

    #[test]
    fn lex_double_star() {
        check_token("**", T!["**"]);
    }

    #[test]
    fn lex_double_quotes() {
        check_token("\"", T!["\""]);
    }

    #[test]
    fn lex_single_quotes() {
        check_token("'", T!["'"]);
    }

    #[test]
    fn lex_grave_accent_quotes() {
        check_token("`", T!["`"]);
    }

    #[test]
    fn lex_curly_percent() {
        check_token("{%", T!["{%"]);
    }

    #[test]
    fn lex_percent_curly() {
        check_token("%}", T!["%}"]);
    }

    #[test]
    fn lex_open_curly_curly() {
        check_token("{{", T!["{{"]);
    }

    #[test]
    fn lex_close_curly_curly() {
        check_token("}}", T!["}}"]);
    }

    #[test]
    fn lex_open_curly_hashtag() {
        check_token("{#", T!["{#"]);
    }

    #[test]
    fn lex_hashtag_close_curly() {
        check_token("#}", T!["#}"]);
    }

    #[test]
    fn lex_hashtag() {
        check_token("#", T!["#"]);
    }

    #[test]
    fn lex_true() {
        check_token("true", T!["true"]);
    }

    #[test]
    fn lex_false() {
        check_token("false", T!["false"]);
    }

    #[test]
    fn lex_block() {
        check_token("block", T!["block"]);
    }

    #[test]
    fn lex_endblock() {
        check_token("endblock", T!["endblock"]);
    }

    #[test]
    fn lex_if() {
        check_token("if", T!["if"]);
    }

    #[test]
    fn lex_else_if() {
        check_token("elseif", T!["elseif"]);
    }

    #[test]
    fn lex_else() {
        check_token("else", T!["else"]);
    }

    #[test]
    fn lex_endif() {
        check_token("endif", T!["endif"]);
    }

    #[test]
    fn lex_apply() {
        check_token("apply", T!["apply"]);
    }

    #[test]
    fn lex_endapply() {
        check_token("endapply", T!["endapply"]);
    }

    #[test]
    fn lex_autoescape() {
        check_token("autoescape", T!["autoescape"]);
    }

    #[test]
    fn lex_endautoescape() {
        check_token("endautoescape", T!["endautoescape"]);
    }

    #[test]
    fn lex_cache() {
        check_token("cache", T!["cache"]);
    }

    #[test]
    fn lex_endcache() {
        check_token("endcache", T!["endcache"]);
    }

    #[test]
    fn lex_deprecated() {
        check_token("deprecated", T!["deprecated"]);
    }

    #[test]
    fn lex_do() {
        check_token("do", T!["do"]);
    }

    #[test]
    fn lex_embed() {
        check_token("embed", T!["embed"]);
    }

    #[test]
    fn lex_endembed() {
        check_token("endembed", T!["endembed"]);
    }

    #[test]
    fn lex_extends() {
        check_token("extends", T!["extends"]);
    }

    #[test]
    fn lex_flush() {
        check_token("flush", T!["flush"]);
    }

    #[test]
    fn lex_for() {
        check_token("for", T!["for"]);
    }

    #[test]
    fn lex_endfor() {
        check_token("endfor", T!["endfor"]);
    }

    #[test]
    fn lex_from() {
        check_token("from", T!["from"]);
    }

    #[test]
    fn lex_import() {
        check_token("import", T!["import"]);
    }

    #[test]
    fn lex_macro() {
        check_token("macro", T!["macro"]);
    }

    #[test]
    fn lex_endmacro() {
        check_token("endmacro", T!["endmacro"]);
    }

    #[test]
    fn lex_sandbox() {
        check_token("sandbox", T!["sandbox"]);
    }

    #[test]
    fn lex_endsandbox() {
        check_token("endsandbox", T!["endsandbox"]);
    }

    #[test]
    fn lex_set() {
        check_token("set", T!["set"]);
    }

    #[test]
    fn lex_endset() {
        check_token("endset", T!["endset"]);
    }

    #[test]
    fn lex_use() {
        check_token("use", T!["use"]);
    }

    #[test]
    fn lex_verbatim() {
        check_token("verbatim", T!["verbatim"]);
    }

    #[test]
    fn lex_endverbatim() {
        check_token("endverbatim", T!["endverbatim"]);
    }

    #[test]
    fn lex_only() {
        check_token("only", T!["only"]);
    }

    #[test]
    fn lex_ignore_missing() {
        check_token("ignore missing", T!["ignore missing"]);
    }

    #[test]
    fn lex_with() {
        check_token("with", T!["with"]);
    }

    #[test]
    fn lex_endwith() {
        check_token("endwith", T!["endwith"]);
    }

    #[test]
    fn lex_ttl() {
        check_token("ttl", T!["ttl"]);
    }

    #[test]
    fn lex_tags() {
        check_token("tags", T!["tags"]);
    }

    #[test]
    fn lex_not() {
        check_token("not", T!["not"]);
    }

    #[test]
    fn lex_or() {
        check_token("or", T!["or"]);
    }

    #[test]
    fn lex_and() {
        check_token("and", T!["and"]);
    }

    #[test]
    fn lex_binary_or() {
        check_token("b-or", T!["b-or"]);
    }

    #[test]
    fn lex_binary_xor() {
        check_token("b-xor", T!["b-xor"]);
    }

    #[test]
    fn lex_binary_and() {
        check_token("b-and", T!["b-and"]);
    }

    #[test]
    fn lex_in() {
        check_token("in", T!["in"]);
    }

    #[test]
    fn lex_matches() {
        check_token("matches", T!["matches"]);
    }

    #[test]
    fn lex_starts_with() {
        check_token("starts with", T!["starts with"]);
    }

    #[test]
    fn lex_ends_with() {
        check_token("ends with", T!["ends with"]);
    }

    #[test]
    fn lex_is() {
        check_token("is", T!["is"]);
    }

    #[test]
    fn lex_even() {
        check_token("even", T!["even"]);
    }

    #[test]
    fn lex_odd() {
        check_token("odd", T!["odd"]);
    }

    #[test]
    fn lex_defined() {
        check_token("defined", T!["defined"]);
    }

    #[test]
    fn lex_same_as() {
        check_token("same as", T!["same as"]);
    }

    #[test]
    fn lex_as() {
        check_token("as", T!["as"]);
    }

    #[test]
    fn lex_none() {
        check_token("none", T!["none"]);
    }

    #[test]
    fn lex_null() {
        check_token("null", T!["null"]);
    }

    #[test]
    fn lex_divisible_by() {
        check_token("divisible by", T!["divisible by"]);
    }

    #[test]
    fn lex_constant() {
        check_token("constant", T!["constant"]);
    }

    #[test]
    fn lex_empty() {
        check_token("empty", T!["empty"]);
    }

    #[test]
    fn lex_iterable() {
        check_token("iterable", T!["iterable"]);
    }

    #[test]
    fn lex_max() {
        check_token("max", T!["max"]);
    }

    #[test]
    fn lex_min() {
        check_token("min", T!["min"]);
    }

    #[test]
    fn lex_range() {
        check_token("range", T!["range"]);
    }

    #[test]
    fn lex_cycle() {
        check_token("cycle", T!["cycle"]);
    }

    #[test]
    fn lex_random() {
        check_token("random", T!["random"]);
    }

    #[test]
    fn lex_date() {
        check_token("date", T!["date"]);
    }

    #[test]
    fn lex_include() {
        check_token("include", T!["include"]);
    }

    #[test]
    fn lex_source() {
        check_token("source", T!["source"]);
    }

    #[test]
    fn lex_sw_extends() {
        check_token("sw_extends", T!["sw_extends"]);
    }

    #[test]
    fn lex_sw_silent_feature_call() {
        check_token("sw_silent_feature_call", T!["sw_silent_feature_call"]);
    }

    #[test]
    fn lex_endsw_silent_feature_call() {
        check_token("endsw_silent_feature_call", T!["endsw_silent_feature_call"]);
    }

    #[test]
    fn lex_sw_include() {
        check_token("sw_include", T!["sw_include"]);
    }

    #[test]
    fn lex_return() {
        check_token("return", T!["return"]);
    }

    #[test]
    fn lex_sw_icon() {
        check_token("sw_icon", T!["sw_icon"]);
    }

    #[test]
    fn lex_sw_thumbnails() {
        check_token("sw_thumbnails", T!["sw_thumbnails"]);
    }

    #[test]
    fn lex_style() {
        check_token("style", T!["style"]);
    }

    #[test]
    fn lex_ludtwig_ignore_file() {
        check_token("ludtwig-ignore-file", T!["ludtwig-ignore-file"]);
    }

    #[test]
    fn lex_ludtwig_ignore() {
        check_token("ludtwig-ignore", T!["ludtwig-ignore"]);
    }
}

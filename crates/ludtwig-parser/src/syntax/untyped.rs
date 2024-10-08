//! This module contains all the rowan types.
//!
//! All GreenNodes and consequently RedNodes as well as AST Nodes
//! contain a single [SyntaxKind] variant for each GreenNode.
//!
//! You can use the macro (T![])[crate::T] to quickly construct a [SyntaxKind]
//! and for example compare it to a different one. For example
//! ```
//! use ludtwig_parser::syntax::untyped::SyntaxKind;
//! use ludtwig_parser::T;
//! assert_eq!(T!["%}"], SyntaxKind::TK_PERCENT_CURLY);
//! ```
//!
//! An overview of the syntax tree concept can be found
//! at the [crate level documentation](crate#syntax-trees).

use logos::Logos;
pub use rowan::Direction;
// `GreenNode` is an immutable tree, which is cheap to change,
// but doesn't contain offsets and parent pointers.
pub use rowan::GreenNode;
// You can construct `GreenNodes` by hand, but a builder
// is helpful for top-down parsers: it maintains a stack
// of currently in-progress nodes
pub use rowan::GreenNodeBuilder;
pub use rowan::Language;
pub use rowan::SyntaxText;
pub use rowan::TextLen;
pub use rowan::TextRange;
pub use rowan::TextSize;
pub use rowan::WalkEvent;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    /*
    Tokens without any meaning / semantic attached to them.
    These are produced by the lexer and provide a small abstraction over plain text
     */
    #[regex(r"[ \t]+")]
    TK_WHITESPACE = 0,
    #[regex(r"((\n)|(\r\n))+")]
    TK_LINE_BREAK,
    /// a single word containing only characters, numbers or symbols
    /// must start with an alpha or one of the special starting characters followed by a normal alpha
    /// special case: allows a single underscore as a valid word
    #[regex(r"([a-zA-Z]|([@\#_\$][a-zA-Z])|_)[a-zA-Z0-9_\-]*")]
    TK_WORD,
    /// a valid twig number
    #[regex(r"[0-9]+(\.[0-9]+)?([Ee][\+\-][0-9]+)?")]
    TK_NUMBER,
    /// a html escape character like '&NewLine;' or '&#10;' or '&#xA;'
    #[regex(r"\&(([a-zA-Z][a-zA-Z0-9]*)|(\#[0-9]+)|(\#x[0-9a-fA-F]+));")]
    TK_HTML_ESCAPE_CHARACTER,
    #[token(".")]
    TK_DOT,
    #[token("..")]
    TK_DOUBLE_DOT,
    #[token(",")]
    TK_COMMA,
    #[token(":")]
    TK_COLON,
    #[token(";")]
    TK_SEMICOLON,
    #[token("!")]
    TK_EXCLAMATION_MARK,
    #[token("!=")]
    TK_EXCLAMATION_MARK_EQUALS,
    #[token("!==")]
    TK_EXCLAMATION_MARK_DOUBLE_EQUALS,
    #[token("?")]
    TK_QUESTION_MARK,
    #[token("??")]
    TK_DOUBLE_QUESTION_MARK,
    #[token("%")]
    TK_PERCENT,
    #[token("~")]
    TK_TILDE,
    #[token("|")]
    TK_SINGLE_PIPE,
    #[token("||")]
    TK_DOUBLE_PIPE,
    #[token("&")]
    TK_AMPERSAND,
    #[token("&&")]
    TK_DOUBLE_AMPERSAND,
    #[token("/")]
    TK_FORWARD_SLASH,
    #[token("//")]
    TK_DOUBLE_FORWARD_SLASH,
    #[token("\\")]
    TK_BACKWARD_SLASH,
    #[token("(")]
    TK_OPEN_PARENTHESIS,
    #[token(")")]
    TK_CLOSE_PARENTHESIS,
    #[token("{")]
    TK_OPEN_CURLY,
    #[token("}")]
    TK_CLOSE_CURLY,
    #[token("[")]
    TK_OPEN_SQUARE,
    #[token("]")]
    TK_CLOSE_SQUARE,
    #[token("<")]
    TK_LESS_THAN,
    #[token("<=")]
    TK_LESS_THAN_EQUAL,
    #[token("<=>")]
    TK_LESS_THAN_EQUAL_GREATER_THAN,
    #[token("</")]
    TK_LESS_THAN_SLASH,
    #[token("<!")]
    TK_LESS_THAN_EXCLAMATION_MARK,
    #[token("DOCTYPE", ignore(ascii_case))]
    TK_DOCTYPE,
    #[token(">")]
    TK_GREATER_THAN,
    #[token(">=")]
    TK_GREATER_THAN_EQUAL,
    #[token("=>")]
    TK_EQUAL_GREATER_THAN,
    #[token("/>")]
    TK_SLASH_GREATER_THAN,
    #[token("<!--")]
    TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS,
    #[token("-->")]
    TK_MINUS_MINUS_GREATER_THAN,
    #[token("=")]
    TK_EQUAL,
    #[token("==")]
    TK_DOUBLE_EQUAL,
    #[token("===")]
    TK_TRIPLE_EQUAL,
    #[token("+")]
    TK_PLUS,
    #[token("-")]
    TK_MINUS,
    #[token("*")]
    TK_STAR,
    #[token("**")]
    TK_DOUBLE_STAR,
    #[token("\"")]
    TK_DOUBLE_QUOTES,
    #[token("'")]
    TK_SINGLE_QUOTES,
    #[token("`")]
    TK_GRAVE_ACCENT_QUOTES,
    #[token("{%")]
    TK_CURLY_PERCENT,
    #[token("%}")]
    TK_PERCENT_CURLY,
    #[token("{{")]
    TK_OPEN_CURLY_CURLY,
    #[token("}}")]
    TK_CLOSE_CURLY_CURLY,
    #[token("{#")]
    TK_OPEN_CURLY_HASHTAG,
    #[token("#}")]
    TK_HASHTAG_CLOSE_CURLY,
    #[token("#")]
    TK_HASHTAG,

    #[token("true", ignore(ascii_case))]
    TK_TRUE,
    #[token("false", ignore(ascii_case))]
    TK_FALSE,

    /* twig tag tokens */
    #[token("block")]
    TK_BLOCK,
    #[token("endblock")]
    TK_ENDBLOCK,
    #[token("if")]
    TK_IF,
    #[token("elseif")]
    TK_ELSE_IF,
    #[token("else")]
    TK_ELSE,
    #[token("endif")]
    TK_ENDIF,
    #[token("apply")]
    TK_APPLY,
    #[token("endapply")]
    TK_ENDAPPLY,
    #[token("autoescape")]
    TK_AUTOESCAPE,
    #[token("endautoescape")]
    TK_ENDAUTOESCAPE,
    #[token("cache")]
    TK_CACHE,
    #[token("endcache")]
    TK_ENDCACHE,
    #[token("deprecated")]
    TK_DEPRECATED,
    #[token("do")]
    TK_DO,
    #[token("embed")]
    TK_EMBED,
    #[token("endembed")]
    TK_ENDEMBED,
    #[token("extends")]
    TK_EXTENDS,
    #[token("flush")]
    TK_FLUSH,
    #[token("for")]
    TK_FOR,
    #[token("endfor")]
    TK_ENDFOR,
    #[token("from")]
    TK_FROM,
    #[token("import")]
    TK_IMPORT,
    #[token("macro")]
    TK_MACRO,
    #[token("endmacro")]
    TK_ENDMACRO,
    #[token("sandbox")]
    TK_SANDBOX,
    #[token("endsandbox")]
    TK_ENDSANDBOX,
    #[token("set")]
    TK_SET,
    #[token("endset")]
    TK_ENDSET,
    #[token("use")]
    TK_USE,
    #[token("verbatim")]
    TK_VERBATIM,
    #[token("endverbatim")]
    TK_ENDVERBATIM,
    #[token("only")]
    TK_ONLY,
    #[token("ignore missing")]
    TK_IGNORE_MISSING,
    #[token("with")]
    TK_WITH,
    #[token("endwith")]
    TK_ENDWITH,
    #[token("ttl")]
    TK_TTL,
    #[token("tags")]
    TK_TAGS,
    /* twig operators */
    #[token("not")]
    TK_NOT,
    #[token("or")]
    TK_OR,
    #[token("and")]
    TK_AND,
    #[token("b-or")]
    TK_BINARY_OR,
    #[token("b-xor")]
    TK_BINARY_XOR,
    #[token("b-and")]
    TK_BINARY_AND,
    #[token("in")]
    TK_IN,
    #[token("matches")]
    TK_MATCHES,
    #[token("starts with")]
    TK_STARTS_WITH,
    #[token("ends with")]
    TK_ENDS_WITH,
    #[token("is")]
    TK_IS,
    /* twig tests */
    #[token("even")]
    TK_EVEN,
    #[token("odd")]
    TK_ODD,
    #[token("defined")]
    TK_DEFINED,
    #[token("same as")]
    TK_SAME_AS,
    #[token("as")]
    TK_AS,
    #[token("none", ignore(ascii_case))]
    TK_NONE,
    #[token("null", ignore(ascii_case))]
    TK_NULL,
    #[token("divisible by")]
    TK_DIVISIBLE_BY,
    #[token("constant")]
    TK_CONSTANT,
    #[token("empty")]
    TK_EMPTY,
    #[token("iterable")]
    TK_ITERABLE,
    /* twig functions */
    #[token("max")]
    TK_MAX,
    #[token("min")]
    TK_MIN,
    #[token("range")]
    TK_RANGE,
    #[token("cycle")]
    TK_CYCLE,
    #[token("random")]
    TK_RANDOM,
    #[token("date")]
    TK_DATE,
    #[token("include")]
    TK_INCLUDE,
    #[token("source")]
    TK_SOURCE,

    /* Drupal Trans */
    #[token("trans")]
    TK_TRANS,
    #[token("endtrans")]
    TK_ENDTRANS,

    /* shopware specific */
    #[token("sw_extends")]
    TK_SW_EXTENDS,
    #[token("sw_silent_feature_call")]
    TK_SW_SILENT_FEATURE_CALL,
    #[token("endsw_silent_feature_call")]
    TK_ENDSW_SILENT_FEATURE_CALL,
    #[token("sw_include")]
    TK_SW_INCLUDE,
    #[token("return")]
    TK_RETURN,
    #[token("sw_icon")]
    TK_SW_ICON,
    #[token("sw_thumbnails")]
    TK_SW_THUMBNAILS,
    #[token("style")]
    TK_STYLE,

    /* special tokens */
    #[token("ludtwig-ignore-file", ignore(ascii_case))]
    TK_LUDTWIG_IGNORE_FILE,
    #[token("ludtwig-ignore", ignore(ascii_case))]
    TK_LUDTWIG_IGNORE,
    TK_UNKNOWN, // contains invalid / unrecognized syntax (used for error recovery).

    /*
    Composite nodes (which can have children and ast / typed counterparts)
    These do have a meaning and are constructed by the parser
    */
    BODY,
    TWIG_VAR,
    TWIG_EXPRESSION, // covers every expression (binary / unary) or literals (where expressions are allowed)
    TWIG_BINARY_EXPRESSION,
    TWIG_UNARY_EXPRESSION,
    TWIG_PARENTHESES_EXPRESSION,
    TWIG_CONDITIONAL_EXPRESSION,

    TWIG_OPERAND, // covers the operands in TWIG_ACCESSOR, TWIG_INDEX_LOOKUP, TWIG_PIPE and TWIG_FUNCTION_CALL
    TWIG_ACCESSOR, // accessor node like 'product.price'
    TWIG_FILTER,  // filter node like 'name|title'

    TWIG_INDEX_LOOKUP, // indexer node like 'products[0]'
    TWIG_INDEX,        // covers a array index '5' inside []
    TWIG_INDEX_RANGE,  // covers a array index range like '0:10' inside []

    TWIG_FUNCTION_CALL,
    TWIG_ARROW_FUNCTION, // like 'i => i % 2' or '(a, b) => a >= b'
    TWIG_ARGUMENTS,
    TWIG_NAMED_ARGUMENT,

    // twig literals
    TWIG_LITERAL_STRING,
    TWIG_LITERAL_STRING_INNER,
    TWIG_LITERAL_STRING_INTERPOLATION,
    TWIG_LITERAL_NUMBER,
    TWIG_LITERAL_ARRAY,
    TWIG_LITERAL_ARRAY_INNER,
    TWIG_LITERAL_NULL,
    TWIG_LITERAL_BOOLEAN,
    TWIG_LITERAL_HASH,
    TWIG_LITERAL_HASH_ITEMS,
    TWIG_LITERAL_HASH_PAIR,
    TWIG_LITERAL_HASH_KEY,
    TWIG_LITERAL_HASH_VALUE,
    TWIG_LITERAL_NAME,

    // twig block like structures
    TWIG_COMMENT,
    // twig block
    TWIG_BLOCK,
    TWIG_STARTING_BLOCK,
    TWIG_ENDING_BLOCK,
    // twig if
    TWIG_IF,
    TWIG_IF_BLOCK,
    TWIG_ELSE_IF_BLOCK,
    TWIG_ELSE_BLOCK,
    TWIG_ENDIF_BLOCK,
    // twig set
    TWIG_SET,
    TWIG_SET_BLOCK,
    TWIG_ENDSET_BLOCK,
    TWIG_ASSIGNMENT,
    // twig for
    TWIG_FOR,
    TWIG_FOR_BLOCK,
    TWIG_FOR_ELSE_BLOCK,
    TWIG_ENDFOR_BLOCK,
    // twig extends
    TWIG_EXTENDS,
    // twig include
    TWIG_INCLUDE,
    TWIG_INCLUDE_WITH,
    // twig use
    TWIG_USE,
    TWIG_OVERRIDE,
    // twig apply
    TWIG_APPLY,
    TWIG_APPLY_STARTING_BLOCK,
    TWIG_APPLY_ENDING_BLOCK,
    // twig autoescape
    TWIG_AUTOESCAPE,
    TWIG_AUTOESCAPE_STARTING_BLOCK,
    TWIG_AUTOESCAPE_ENDING_BLOCK,
    // twig deprecated
    TWIG_DEPRECATED,
    // twig do
    TWIG_DO,
    // twig embed
    TWIG_EMBED,
    TWIG_EMBED_STARTING_BLOCK,
    TWIG_EMBED_ENDING_BLOCK,
    // twig flush
    TWIG_FLUSH,
    // twig from
    TWIG_FROM, // shares TWIG_OVERRIDE with twig use tag
    // twig import
    TWIG_IMPORT,
    // twig sandbox
    TWIG_SANDBOX,
    TWIG_SANDBOX_STARTING_BLOCK,
    TWIG_SANDBOX_ENDING_BLOCK,
    // twig verbatim
    TWIG_VERBATIM,
    TWIG_VERBATIM_STARTING_BLOCK,
    TWIG_VERBATIM_ENDING_BLOCK,
    // twig macro
    TWIG_MACRO,
    TWIG_MACRO_STARTING_BLOCK,
    TWIG_MACRO_ENDING_BLOCK,
    // twig with
    TWIG_WITH,
    TWIG_WITH_STARTING_BLOCK,
    TWIG_WITH_ENDING_BLOCK,
    // twig cache
    TWIG_CACHE,
    TWIG_CACHE_TTL,
    TWIG_CACHE_TAGS,
    TWIG_CACHE_STARTING_BLOCK,
    TWIG_CACHE_ENDING_BLOCK,

    // Drupal Trans
    TWIG_TRANS,
    TWIG_TRANS_BLOCK,
    TWIG_ENDTRANS_BLOCK,

    // shopware specific
    SHOPWARE_TWIG_SW_EXTENDS,
    SHOPWARE_TWIG_SW_INCLUDE,
    SHOPWARE_SILENT_FEATURE_CALL,
    SHOPWARE_SILENT_FEATURE_CALL_STARTING_BLOCK,
    SHOPWARE_SILENT_FEATURE_CALL_ENDING_BLOCK,
    SHOPWARE_RETURN,
    SHOPWARE_ICON,
    SHOPWARE_ICON_STYLE,
    SHOPWARE_THUMBNAILS,
    SHOPWARE_THUMBNAILS_WITH,

    // html
    HTML_DOCTYPE,
    HTML_ATTRIBUTE_LIST,
    HTML_ATTRIBUTE,
    HTML_STRING,       // used as attribute values
    HTML_STRING_INNER, // content inside the quotes of html attribute values
    HTML_TEXT,         // used as plain text between html tags / twig blocks
    HTML_RAW_TEXT, // used as raw text between "raw text" html elements like inline script and style
    HTML_COMMENT,
    HTML_TAG,
    HTML_STARTING_TAG,
    HTML_ENDING_TAG,

    // special ludtwig directive
    LUDTWIG_DIRECTIVE_FILE_IGNORE,
    LUDTWIG_DIRECTIVE_IGNORE,
    LUDTWIG_DIRECTIVE_RULE_LIST,
    /*
    Special Nodes
     */
    ERROR, // syntax node which wraps invalid syntax
    /// SAFETY: this must be the last enum element for u16 conversion!
    ROOT, // top-level node: list of elements inside the template (must be last item of enum for safety check!)
}

#[macro_export]
macro_rules! T {
    [ws] => { $crate::syntax::untyped::SyntaxKind::TK_WHITESPACE };
    [lb] => { $crate::syntax::untyped::SyntaxKind::TK_LINE_BREAK };
    [word] => { $crate::syntax::untyped::SyntaxKind::TK_WORD };
    [number] => { $crate::syntax::untyped::SyntaxKind::TK_NUMBER };
    [html escape character] => { $crate::syntax::untyped::SyntaxKind::TK_HTML_ESCAPE_CHARACTER };
    [unknown] => { $crate::syntax::untyped::SyntaxKind::TK_UNKNOWN };
    ["."] => { $crate::syntax::untyped::SyntaxKind::TK_DOT };
    [".."] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_DOT };
    [","] => { $crate::syntax::untyped::SyntaxKind::TK_COMMA };
    [":"] => { $crate::syntax::untyped::SyntaxKind::TK_COLON };
    [";"] => { $crate::syntax::untyped::SyntaxKind::TK_SEMICOLON };
    ["!"] => { $crate::syntax::untyped::SyntaxKind::TK_EXCLAMATION_MARK };
    ["!="] => { $crate::syntax::untyped::SyntaxKind::TK_EXCLAMATION_MARK_EQUALS };
    ["!=="] => { $crate::syntax::untyped::SyntaxKind::TK_EXCLAMATION_MARK_DOUBLE_EQUALS };
    ["?"] => { $crate::syntax::untyped::SyntaxKind::TK_QUESTION_MARK };
    ["??"] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_QUESTION_MARK };
    ["%"] => { $crate::syntax::untyped::SyntaxKind::TK_PERCENT };
    ["~"] => { $crate::syntax::untyped::SyntaxKind::TK_TILDE };
    ["|"] => { $crate::syntax::untyped::SyntaxKind::TK_SINGLE_PIPE };
    ["||"] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_PIPE };
    ["&"] => { $crate::syntax::untyped::SyntaxKind::TK_AMPERSAND };
    ["&&"] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_AMPERSAND };
    ["/"] => { $crate::syntax::untyped::SyntaxKind::TK_FORWARD_SLASH };
    ["//"] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_FORWARD_SLASH };
    ["\\"] => { $crate::syntax::untyped::SyntaxKind::TK_BACKWARD_SLASH };
    ["("] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_PARENTHESIS };
    [")"] => { $crate::syntax::untyped::SyntaxKind::TK_CLOSE_PARENTHESIS };
    ["{"] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_CURLY };
    ["}"] => { $crate::syntax::untyped::SyntaxKind::TK_CLOSE_CURLY };
    ["["] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_SQUARE };
    ["]"] => { $crate::syntax::untyped::SyntaxKind::TK_CLOSE_SQUARE };
    ["<"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN };
    ["<="] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_EQUAL };
    ["<=>"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_EQUAL_GREATER_THAN };
    ["</"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_SLASH };
    ["<!"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_EXCLAMATION_MARK };
    ["DOCTYPE"] => { $crate::syntax::untyped::SyntaxKind::TK_DOCTYPE };
    [">"] => { $crate::syntax::untyped::SyntaxKind::TK_GREATER_THAN };
    [">="] => { $crate::syntax::untyped::SyntaxKind::TK_GREATER_THAN_EQUAL };
    ["=>"] => { $crate::syntax::untyped::SyntaxKind::TK_EQUAL_GREATER_THAN };
    ["/>"] => { $crate::syntax::untyped::SyntaxKind::TK_SLASH_GREATER_THAN };
    ["<!--"] => { $crate::syntax::untyped::SyntaxKind::TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS };
    ["-->"] => { $crate::syntax::untyped::SyntaxKind::TK_MINUS_MINUS_GREATER_THAN };
    ["="] => { $crate::syntax::untyped::SyntaxKind::TK_EQUAL };
    ["=="] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_EQUAL };
    ["==="] => { $crate::syntax::untyped::SyntaxKind::TK_TRIPLE_EQUAL };
    ["+"] => { $crate::syntax::untyped::SyntaxKind::TK_PLUS };
    ["-"] => { $crate::syntax::untyped::SyntaxKind::TK_MINUS };
    ["*"] => { $crate::syntax::untyped::SyntaxKind::TK_STAR };
    ["**"] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_STAR };
    ["\""] => { $crate::syntax::untyped::SyntaxKind::TK_DOUBLE_QUOTES };
    ["'"] => { $crate::syntax::untyped::SyntaxKind::TK_SINGLE_QUOTES };
    ["`"] => { $crate::syntax::untyped::SyntaxKind::TK_GRAVE_ACCENT_QUOTES };
    ["{%"] => { $crate::syntax::untyped::SyntaxKind::TK_CURLY_PERCENT };
    ["%}"] => { $crate::syntax::untyped::SyntaxKind::TK_PERCENT_CURLY };
    ["{{"] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_CURLY_CURLY };
    ["}}"] => { $crate::syntax::untyped::SyntaxKind::TK_CLOSE_CURLY_CURLY };
    ["{#"] => { $crate::syntax::untyped::SyntaxKind::TK_OPEN_CURLY_HASHTAG };
    ["#}"] => { $crate::syntax::untyped::SyntaxKind::TK_HASHTAG_CLOSE_CURLY };
    ["#"] => { $crate::syntax::untyped::SyntaxKind::TK_HASHTAG };
    ["true"] => { $crate::syntax::untyped::SyntaxKind::TK_TRUE };
    ["false"] => { $crate::syntax::untyped::SyntaxKind::TK_FALSE };
    ["block"] => { $crate::syntax::untyped::SyntaxKind::TK_BLOCK };
    ["endblock"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDBLOCK };
    ["if"] => { $crate::syntax::untyped::SyntaxKind::TK_IF };
    ["elseif"] => { $crate::syntax::untyped::SyntaxKind::TK_ELSE_IF };
    ["else"] => { $crate::syntax::untyped::SyntaxKind::TK_ELSE };
    ["endif"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDIF };
    ["apply"] => { $crate::syntax::untyped::SyntaxKind::TK_APPLY };
    ["endapply"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDAPPLY };
    ["autoescape"] => { $crate::syntax::untyped::SyntaxKind::TK_AUTOESCAPE };
    ["endautoescape"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDAUTOESCAPE };
    ["cache"] => { $crate::syntax::untyped::SyntaxKind::TK_CACHE };
    ["endcache"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDCACHE };
    ["deprecated"] => { $crate::syntax::untyped::SyntaxKind::TK_DEPRECATED };
    ["do"] => { $crate::syntax::untyped::SyntaxKind::TK_DO };
    ["embed"] => { $crate::syntax::untyped::SyntaxKind::TK_EMBED };
    ["endembed"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDEMBED };
    ["extends"] => { $crate::syntax::untyped::SyntaxKind::TK_EXTENDS };
    ["flush"] => { $crate::syntax::untyped::SyntaxKind::TK_FLUSH };
    ["for"] => { $crate::syntax::untyped::SyntaxKind::TK_FOR };
    ["endfor"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDFOR };
    ["from"] => { $crate::syntax::untyped::SyntaxKind::TK_FROM };
    ["import"] => { $crate::syntax::untyped::SyntaxKind::TK_IMPORT };
    ["macro"] => { $crate::syntax::untyped::SyntaxKind::TK_MACRO };
    ["endmacro"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDMACRO };
    ["sandbox"] => { $crate::syntax::untyped::SyntaxKind::TK_SANDBOX };
    ["endsandbox"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDSANDBOX };
    ["set"] => { $crate::syntax::untyped::SyntaxKind::TK_SET };
    ["endset"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDSET };
    ["use"] => { $crate::syntax::untyped::SyntaxKind::TK_USE };
    ["verbatim"] => { $crate::syntax::untyped::SyntaxKind::TK_VERBATIM };
    ["endverbatim"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDVERBATIM };
    ["only"] => { $crate::syntax::untyped::SyntaxKind::TK_ONLY };
    ["ignore missing"] => { $crate::syntax::untyped::SyntaxKind::TK_IGNORE_MISSING };
    ["with"] => { $crate::syntax::untyped::SyntaxKind::TK_WITH };
    ["endwith"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDWITH };
    ["ttl"] => { $crate::syntax::untyped::SyntaxKind::TK_TTL };
    ["tags"] => { $crate::syntax::untyped::SyntaxKind::TK_TAGS };
    ["not"] => { $crate::syntax::untyped::SyntaxKind::TK_NOT };
    ["or"] => { $crate::syntax::untyped::SyntaxKind::TK_OR };
    ["and"] => { $crate::syntax::untyped::SyntaxKind::TK_AND };
    ["b-or"] => { $crate::syntax::untyped::SyntaxKind::TK_BINARY_OR };
    ["b-xor"] => { $crate::syntax::untyped::SyntaxKind::TK_BINARY_XOR };
    ["b-and"] => { $crate::syntax::untyped::SyntaxKind::TK_BINARY_AND };
    ["in"] => { $crate::syntax::untyped::SyntaxKind::TK_IN };
    ["matches"] => { $crate::syntax::untyped::SyntaxKind::TK_MATCHES };
    ["starts with"] => { $crate::syntax::untyped::SyntaxKind::TK_STARTS_WITH };
    ["ends with"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDS_WITH };
    ["is"] => { $crate::syntax::untyped::SyntaxKind::TK_IS };
    ["even"] => { $crate::syntax::untyped::SyntaxKind::TK_EVEN };
    ["odd"] => { $crate::syntax::untyped::SyntaxKind::TK_ODD };
    ["defined"] => { $crate::syntax::untyped::SyntaxKind::TK_DEFINED };
    ["same as"] => { $crate::syntax::untyped::SyntaxKind::TK_SAME_AS };
    ["as"] => { $crate::syntax::untyped::SyntaxKind::TK_AS };
    ["none"] => { $crate::syntax::untyped::SyntaxKind::TK_NONE };
    ["null"] => { $crate::syntax::untyped::SyntaxKind::TK_NULL };
    ["divisible by"] => { $crate::syntax::untyped::SyntaxKind::TK_DIVISIBLE_BY };
    ["constant"] => { $crate::syntax::untyped::SyntaxKind::TK_CONSTANT };
    ["empty"] => { $crate::syntax::untyped::SyntaxKind::TK_EMPTY };
    ["iterable"] => { $crate::syntax::untyped::SyntaxKind::TK_ITERABLE };
    ["max"] => { $crate::syntax::untyped::SyntaxKind::TK_MAX };
    ["min"] => { $crate::syntax::untyped::SyntaxKind::TK_MIN };
    ["range"] => { $crate::syntax::untyped::SyntaxKind::TK_RANGE };
    ["cycle"] => { $crate::syntax::untyped::SyntaxKind::TK_CYCLE };
    ["random"] => { $crate::syntax::untyped::SyntaxKind::TK_RANDOM };
    ["date"] => { $crate::syntax::untyped::SyntaxKind::TK_DATE };
    ["include"] => { $crate::syntax::untyped::SyntaxKind::TK_INCLUDE };
    ["source"] => { $crate::syntax::untyped::SyntaxKind::TK_SOURCE };
    ["trans"] => { $crate::syntax::untyped::SyntaxKind::TK_TRANS };
    ["endtrans"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDTRANS };
    ["sw_extends"] => { $crate::syntax::untyped::SyntaxKind::TK_SW_EXTENDS };
    ["sw_silent_feature_call"] => { $crate::syntax::untyped::SyntaxKind::TK_SW_SILENT_FEATURE_CALL };
    ["endsw_silent_feature_call"] => { $crate::syntax::untyped::SyntaxKind::TK_ENDSW_SILENT_FEATURE_CALL };
    ["sw_include"] => { $crate::syntax::untyped::SyntaxKind::TK_SW_INCLUDE };
    ["return"] => { $crate::syntax::untyped::SyntaxKind::TK_RETURN };
    ["sw_icon"] => { $crate::syntax::untyped::SyntaxKind::TK_SW_ICON };
    ["sw_thumbnails"] => { $crate::syntax::untyped::SyntaxKind::TK_SW_THUMBNAILS };
    ["style"] => { $crate::syntax::untyped::SyntaxKind::TK_STYLE };
    ["ludtwig-ignore-file"] => { $crate::syntax::untyped::SyntaxKind::TK_LUDTWIG_IGNORE_FILE };
    ["ludtwig-ignore"] => { $crate::syntax::untyped::SyntaxKind::TK_LUDTWIG_IGNORE };
}

impl SyntaxKind {
    #[must_use]
    pub fn is_trivia(self) -> bool {
        // Add comments and other non interesting things for the parser here in the future
        matches!(self, T![ws] | T![lb])
    }
}

#[allow(clippy::too_many_lines)]
impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            SyntaxKind::TK_WHITESPACE => "whitespace",
            SyntaxKind::TK_LINE_BREAK => "line break",
            SyntaxKind::TK_WORD => "word",
            SyntaxKind::TK_NUMBER => "number",
            SyntaxKind::TK_HTML_ESCAPE_CHARACTER => "html escape character",
            SyntaxKind::TK_DOT => ".",
            SyntaxKind::TK_DOUBLE_DOT => "..",
            SyntaxKind::TK_COMMA => ",",
            SyntaxKind::TK_COLON => ":",
            SyntaxKind::TK_SEMICOLON => ";",
            SyntaxKind::TK_EXCLAMATION_MARK => "!",
            SyntaxKind::TK_EXCLAMATION_MARK_EQUALS => "!=",
            SyntaxKind::TK_EXCLAMATION_MARK_DOUBLE_EQUALS => "!==",
            SyntaxKind::TK_QUESTION_MARK => "?",
            SyntaxKind::TK_DOUBLE_QUESTION_MARK => "??",
            SyntaxKind::TK_PERCENT => "%",
            SyntaxKind::TK_TILDE => "~",
            SyntaxKind::TK_SINGLE_PIPE => "|",
            SyntaxKind::TK_DOUBLE_PIPE => "||",
            SyntaxKind::TK_AMPERSAND => "&",
            SyntaxKind::TK_DOUBLE_AMPERSAND => "&&",
            SyntaxKind::TK_FORWARD_SLASH => "/",
            SyntaxKind::TK_DOUBLE_FORWARD_SLASH => "//",
            SyntaxKind::TK_BACKWARD_SLASH => "\\",
            SyntaxKind::TK_OPEN_PARENTHESIS => "(",
            SyntaxKind::TK_CLOSE_PARENTHESIS => ")",
            SyntaxKind::TK_OPEN_CURLY => "{",
            SyntaxKind::TK_CLOSE_CURLY => "}",
            SyntaxKind::TK_OPEN_SQUARE => "[",
            SyntaxKind::TK_CLOSE_SQUARE => "]",
            SyntaxKind::TK_LESS_THAN => "<",
            SyntaxKind::TK_LESS_THAN_EQUAL => "<=",
            SyntaxKind::TK_LESS_THAN_EQUAL_GREATER_THAN => "<=>",
            SyntaxKind::TK_LESS_THAN_SLASH => "</",
            SyntaxKind::TK_LESS_THAN_EXCLAMATION_MARK => "<!",
            SyntaxKind::TK_DOCTYPE => "DOCTYPE",
            SyntaxKind::TK_GREATER_THAN => ">",
            SyntaxKind::TK_GREATER_THAN_EQUAL => ">=",
            SyntaxKind::TK_EQUAL_GREATER_THAN => "=>",
            SyntaxKind::TK_SLASH_GREATER_THAN => "/>",
            SyntaxKind::TK_LESS_THAN_EXCLAMATION_MARK_MINUS_MINUS => "<!--",
            SyntaxKind::TK_MINUS_MINUS_GREATER_THAN => "-->",
            SyntaxKind::TK_EQUAL => "=",
            SyntaxKind::TK_DOUBLE_EQUAL => "==",
            SyntaxKind::TK_TRIPLE_EQUAL => "===",
            SyntaxKind::TK_PLUS => "+",
            SyntaxKind::TK_MINUS => "-",
            SyntaxKind::TK_STAR => "*",
            SyntaxKind::TK_DOUBLE_STAR => "**",
            SyntaxKind::TK_DOUBLE_QUOTES => "\"",
            SyntaxKind::TK_SINGLE_QUOTES => "'",
            SyntaxKind::TK_GRAVE_ACCENT_QUOTES => "`",
            SyntaxKind::TK_CURLY_PERCENT => "{%",
            SyntaxKind::TK_PERCENT_CURLY => "%}",
            SyntaxKind::TK_OPEN_CURLY_CURLY => "{{",
            SyntaxKind::TK_CLOSE_CURLY_CURLY => "}}",
            SyntaxKind::TK_OPEN_CURLY_HASHTAG => "{#",
            SyntaxKind::TK_HASHTAG_CLOSE_CURLY => "#}",
            SyntaxKind::TK_HASHTAG => "#",
            SyntaxKind::TK_TRUE => "true",
            SyntaxKind::TK_FALSE => "false",
            SyntaxKind::TK_BLOCK => "block",
            SyntaxKind::TK_ENDBLOCK => "endblock",
            SyntaxKind::TK_IF => "if",
            SyntaxKind::TK_ELSE_IF => "elseif",
            SyntaxKind::TK_ELSE => "else",
            SyntaxKind::TK_ENDIF => "endif",
            SyntaxKind::TK_APPLY => "apply",
            SyntaxKind::TK_ENDAPPLY => "endapply",
            SyntaxKind::TK_AUTOESCAPE => "autoescape",
            SyntaxKind::TK_ENDAUTOESCAPE => "endautoescape",
            SyntaxKind::TK_CACHE => "cache",
            SyntaxKind::TK_ENDCACHE => "endcache",
            SyntaxKind::TK_DEPRECATED => "deprecated",
            SyntaxKind::TK_DO => "do",
            SyntaxKind::TK_EMBED => "embed",
            SyntaxKind::TK_ENDEMBED => "endembed",
            SyntaxKind::TK_EXTENDS => "extends",
            SyntaxKind::TK_FLUSH => "flush",
            SyntaxKind::TK_FOR => "for",
            SyntaxKind::TK_ENDFOR => "endfor",
            SyntaxKind::TK_FROM => "from",
            SyntaxKind::TK_IMPORT => "import",
            SyntaxKind::TK_MACRO => "macro",
            SyntaxKind::TK_ENDMACRO => "endmacro",
            SyntaxKind::TK_SANDBOX => "sandbox",
            SyntaxKind::TK_ENDSANDBOX => "endsandbox",
            SyntaxKind::TK_SET => "set",
            SyntaxKind::TK_ENDSET => "endset",
            SyntaxKind::TK_USE => "use",
            SyntaxKind::TK_VERBATIM => "verbatim",
            SyntaxKind::TK_ENDVERBATIM => "endverbatim",
            SyntaxKind::TK_ONLY => "only",
            SyntaxKind::TK_IGNORE_MISSING => "ignore missing",
            SyntaxKind::TK_WITH => "with",
            SyntaxKind::TK_ENDWITH => "endwith",
            SyntaxKind::TK_TTL => "ttl",
            SyntaxKind::TK_TAGS => "tags",
            SyntaxKind::TK_NOT => "not",
            SyntaxKind::TK_OR => "or",
            SyntaxKind::TK_AND => "and",
            SyntaxKind::TK_BINARY_OR => "b-or",
            SyntaxKind::TK_BINARY_XOR => "b-xor",
            SyntaxKind::TK_BINARY_AND => "b-and",
            SyntaxKind::TK_IN => "in",
            SyntaxKind::TK_MATCHES => "matches",
            SyntaxKind::TK_STARTS_WITH => "starts with",
            SyntaxKind::TK_ENDS_WITH => "ends with",
            SyntaxKind::TK_IS => "is",
            SyntaxKind::TK_EVEN => "even",
            SyntaxKind::TK_ODD => "odd",
            SyntaxKind::TK_DEFINED => "defined",
            SyntaxKind::TK_SAME_AS => "same as",
            SyntaxKind::TK_AS => "as",
            SyntaxKind::TK_NONE => "none",
            SyntaxKind::TK_NULL => "null",
            SyntaxKind::TK_DIVISIBLE_BY => "divisible by",
            SyntaxKind::TK_CONSTANT => "constant",
            SyntaxKind::TK_EMPTY => "empty",
            SyntaxKind::TK_ITERABLE => "iterable",
            SyntaxKind::TK_MAX => "max",
            SyntaxKind::TK_MIN => "min",
            SyntaxKind::TK_RANGE => "range",
            SyntaxKind::TK_CYCLE => "cycle",
            SyntaxKind::TK_RANDOM => "random",
            SyntaxKind::TK_DATE => "date",
            SyntaxKind::TK_INCLUDE => "include",
            SyntaxKind::TK_SOURCE => "source",
            SyntaxKind::TK_TRANS => "trans",
            SyntaxKind::TK_ENDTRANS => "endtrans",
            SyntaxKind::TK_SW_EXTENDS => "sw_extends",
            SyntaxKind::TK_SW_SILENT_FEATURE_CALL => "sw_silent_feature_call",
            SyntaxKind::TK_ENDSW_SILENT_FEATURE_CALL => "endsw_silent_feature_call",
            SyntaxKind::TK_SW_INCLUDE => "sw_include",
            SyntaxKind::TK_RETURN => "return",
            SyntaxKind::TK_SW_ICON => "sw_icon",
            SyntaxKind::TK_SW_THUMBNAILS => "sw_thumbnails",
            SyntaxKind::TK_STYLE => "style",
            SyntaxKind::TK_LUDTWIG_IGNORE_FILE => "ludtwig-ignore-file",
            SyntaxKind::TK_LUDTWIG_IGNORE => "ludtwig-ignore",
            SyntaxKind::TK_UNKNOWN => "unknown",
            SyntaxKind::ERROR => "error",
            t => unreachable!("Display not implemented for {:?}", t),
        })
    }
}

// Some boilerplate is needed, as rowan settled on using its own
// `struct SyntaxKind(u16)` internally, instead of accepting the
// user's `enum SyntaxKind` as a type parameter.
//
// First, to easily pass the enum variants into rowan via `.into()`:
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

// Second, implementing the `Language` trait teaches rowan to convert between
// these two `SyntaxKind` types, allowing for a nicer `SyntaxNode` API where
// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TemplateLanguage {}
impl Language for TemplateLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

// To work with the parse results we need a view into the
// green tree - the Syntax tree.
// It is also immutable, like a `GreenNode`,
// but it contains parent pointers, offsets, and
// has identity semantics.
pub type SyntaxNode = rowan::SyntaxNode<TemplateLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<TemplateLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<TemplateLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<TemplateLanguage>;
pub type Preorder = rowan::api::Preorder<TemplateLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<TemplateLanguage>;

#[must_use]
pub fn debug_tree(syntax_node: &SyntaxNode) -> String {
    let formatted = format!("{syntax_node:#?}");
    // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
    formatted[0..formatted.len() - 1].to_string()
}

pub trait SyntaxNodeExt {
    fn text_range_trimmed_trivia(&self) -> TextRange;
}

impl SyntaxNodeExt for SyntaxNode {
    /// Trims leading trivia from the original `text_range`
    fn text_range_trimmed_trivia(&self) -> TextRange {
        let mut range = self.text_range();

        for element in self.children_with_tokens() {
            match element {
                SyntaxElement::Token(t) if t.kind().is_trivia() => {
                    let new_start = range.start() + t.text_range().len();
                    if new_start < range.end() {
                        range = TextRange::new(new_start, range.end());
                    }
                }
                _ => return range,
            }
        }

        range
    }
}

//! Twig Tag parsing (anything between {% ... %})

use crate::grammar::twig::expression::parse_twig_expression;
use crate::grammar::twig::literal::{
    parse_twig_filter, parse_twig_function_argument, parse_twig_name, parse_twig_string,
};
use crate::grammar::twig::shopware::{parse_shopware_twig_block_statement, BlockParseResult};
use crate::grammar::{parse_many, ParseFunction};
use crate::parser::event::{CompletedMarker, Marker};
use crate::parser::{ParseErrorBuilder, Parser};
use crate::syntax::untyped::SyntaxKind;
use crate::T;

/// Checks if the parser is at an twig ending / delimiter tag like
/// `endblock` or `elseif` which should be caught by html body parsers to stop parsing early
/// (this is helpful to spot for example missing closing html tags)
///
/// Important:
/// Every ending twig tag or delimiter tag must be added to this function for now!
pub(crate) fn at_twig_termination_tag(p: &mut Parser) -> bool {
    p.at_following(&[T!["{%"], T!["endblock"]])
        || p.at_following(&[T!["{%"], T!["endif"]])
        || p.at_following(&[T!["{%"], T!["elseif"]])
        || p.at_following(&[T!["{%"], T!["else"]])
        || p.at_following(&[T!["{%"], T!["endset"]])
        || p.at_following(&[T!["{%"], T!["endfor"]])
        || p.at_following(&[T!["{%"], T!["endembed"]])
        || p.at_following(&[T!["{%"], T!["endapply"]])
        || p.at_following(&[T!["{%"], T!["endautoescape"]])
        || p.at_following(&[T!["{%"], T!["endsandbox"]])
        || p.at_following(&[T!["{%"], T!["endverbatim"]])
        || p.at_following(&[T!["{%"], T!["endmacro"]])
        || p.at_following(&[T!["{%"], T!["endwith"]])
        || p.at_following(&[T!["{%"], T!["endcache"]])
        || p.at_following(&[T!["{%"], T!["endsw_silent_feature_call"]])
        || p.at_following(&[T!["{%"], T!["endtrans"]]) // Drupal Trans / Endtrans
}

pub(crate) fn parse_twig_block_statement(
    parser: &mut Parser,
    child_parser: ParseFunction,
) -> Option<CompletedMarker> {
    debug_assert!(parser.at(T!["{%"]));
    let m = parser.start();
    parser.bump();

    if parser.at(T!["block"]) {
        Some(parse_twig_block(parser, m, child_parser))
    } else if parser.at(T!["if"]) {
        Some(parse_twig_if(parser, m, child_parser))
    } else if parser.at(T!["set"]) {
        Some(parse_twig_set(parser, m, child_parser))
    } else if parser.at(T!["for"]) {
        Some(parse_twig_for(parser, m, child_parser))
    } else if parser.at(T!["extends"]) {
        Some(parse_twig_extends(parser, m))
    } else if parser.at(T!["include"]) {
        Some(parse_twig_include(parser, m))
    } else if parser.at(T!["embed"]) {
        Some(parse_twig_embed(parser, m, child_parser))
    } else if parser.at(T!["use"]) {
        Some(parse_twig_use(parser, m))
    } else if parser.at(T!["from"]) {
        Some(parse_twig_from(parser, m))
    } else if parser.at(T!["import"]) {
        Some(parse_twig_import(parser, m))
    } else if parser.at(T!["apply"]) {
        Some(parse_twig_apply(parser, m, child_parser))
    } else if parser.at(T!["autoescape"]) {
        Some(parse_twig_autoescape(parser, m, child_parser))
    } else if parser.at(T!["deprecated"]) {
        Some(parse_twig_deprecated(parser, m))
    } else if parser.at(T!["do"]) {
        Some(parse_twig_do(parser, m))
    } else if parser.at(T!["flush"]) {
        Some(parse_twig_flush(parser, m))
    } else if parser.at(T!["sandbox"]) {
        Some(parse_twig_sandbox(parser, m, child_parser))
    } else if parser.at(T!["verbatim"]) {
        Some(parse_twig_verbatim(parser, m, child_parser))
    } else if parser.at(T!["macro"]) {
        Some(parse_twig_macro(parser, m, child_parser))
    } else if parser.at(T!["with"]) {
        Some(parse_twig_with(parser, m, child_parser))
    } else if parser.at(T!["cache"]) {
        Some(parse_twig_cache(parser, m, child_parser))
    } else if parser.at(T!["trans"]) {
        Some(parse_twig_trans(parser, m, child_parser))
    } else {
        match parse_shopware_twig_block_statement(parser, m, child_parser) {
            BlockParseResult::NothingFound(m) => {
                parser.add_error(ParseErrorBuilder::new("twig tag".to_string()));
                parser.complete(m, SyntaxKind::ERROR);
                None
            }
            BlockParseResult::Successful(completed_m) => Some(completed_m),
        }
    }
}

fn parse_twig_cache(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["cache"]));
    parser.bump();
    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as cache key"));
        parser.recover(&[
            T!["ttl"],
            T!["tags"],
            T!["endcache"],
            T!["("],
            T![")"],
            T!["%}"],
            T!["</"],
        ]);
    }

    if parser.at(T!["ttl"]) {
        let ttl_m = parser.start();
        parser.bump();
        parser.expect(
            T!["("],
            &[T![")"], T!["tags"], T!["endcache"], T!["%}"], T!["</"]],
        );
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new(
                "twig expression as cache time to live",
            ));
            parser.recover(&[T![")"], T!["tags"], T!["endcache"], T!["%}"], T!["</"]]);
        }
        parser.expect(T![")"], &[T!["tags"], T!["endcache"], T!["%}"], T!["</"]]);
        parser.complete(ttl_m, SyntaxKind::TWIG_CACHE_TTL);
    }
    if parser.at(T!["tags"]) {
        let tags_m = parser.start();
        parser.bump();
        parser.expect(T!["("], &[T![")"], T!["endcache"], T!["%}"], T!["</"]]);
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression as cache tags"));
            parser.recover(&[T![")"], T!["endcache"], T!["%}"], T!["</"]]);
        }
        parser.expect(T![")"], &[T!["endcache"], T!["%}"], T!["</"]]);
        parser.complete(tags_m, SyntaxKind::TWIG_CACHE_TAGS);
    }
    parser.expect(T!["%}"], &[T!["endcache"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_CACHE_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endcache
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endcache"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endcache"], T!["%}"], T!["</"]]);
    parser.expect(T!["endcache"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_CACHE_ENDING_BLOCK);

    // close overall twig with
    parser.complete(wrapper_m, SyntaxKind::TWIG_CACHE)
}

fn parse_twig_with(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["with"]));
    parser.bump();
    // optional expression which should resolve to a hash with variable names as keys
    parse_twig_expression(parser);
    if parser.at(T!["only"]) {
        parser.bump();
    }
    parser.expect(T!["%}"], &[T!["endwith"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_WITH_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endwith
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endwith"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endwith"], T!["%}"], T!["</"]]);
    parser.expect(T!["endwith"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_WITH_ENDING_BLOCK);

    // close overall twig with
    parser.complete(wrapper_m, SyntaxKind::TWIG_WITH)
}

fn parse_twig_macro(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["macro"]));
    parser.bump();
    let macro_name = parser
        .expect(
            T![word],
            &[T!["("], T![")"], T!["endmacro"], T!["%}"], T!["</"]],
        )
        .map(|t| t.text.to_owned());

    // macro must have parentheses (arguments can be zero)
    let arguments_m = parser.start();
    parser.expect(T!["("], &[T![")"], T!["endmacro"], T!["%}"], T!["</"]]);
    parse_many(
        parser,
        |p| p.at_set(&[T!["%}"], T![")"]]),
        |p| {
            parse_twig_function_argument(p);
            if p.at(T![","]) {
                p.bump();
            } else if !p.at_set(&[T!["%}"], T![")"]]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );
    parser.expect(T![")"], &[T!["endmacro"], T!["%}"], T!["</"]]);
    parser.complete(arguments_m, SyntaxKind::TWIG_ARGUMENTS);
    parser.expect(T!["%}"], &[T!["endmacro"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_MACRO_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endblock
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endmacro"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endmacro"], T!["%}"], T!["</"]]);
    parser.expect(T!["endmacro"], &[T!["%}"], T!["</"]]);
    // check for optional name behind endmacro
    if parser.at(T![word]) {
        let end_macro_name_token = parser.bump();
        if let Some(macro_name) = macro_name {
            if end_macro_name_token.text != macro_name {
                let parser_err = ParseErrorBuilder::new(format!(
                    "nothing or same twig macro name as opening ({macro_name})"
                ))
                .at_token(end_macro_name_token);

                parser.add_error(parser_err);
                parser.recover(&[T!["%}"], T!["</"]]);
            }
        }
    }
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_MACRO_ENDING_BLOCK);

    // close overall twig macro
    parser.complete(wrapper_m, SyntaxKind::TWIG_MACRO)
}

fn parse_twig_verbatim(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["verbatim"]));
    parser.bump();
    parser.expect(T!["%}"], &[T!["endverbatim"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_VERBATIM_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endverbatim
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endverbatim"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endverbatim"], T!["%}"], T!["</"]]);
    parser.expect(T!["endverbatim"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_VERBATIM_ENDING_BLOCK);

    // close overall twig sandbox
    parser.complete(wrapper_m, SyntaxKind::TWIG_VERBATIM)
}

fn parse_twig_sandbox(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["sandbox"]));
    parser.bump();
    parser.expect(T!["%}"], &[T!["endsandbox"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_SANDBOX_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endsandbox
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endsandbox"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endsandbox"], T!["%}"], T!["</"]]);
    parser.expect(T!["endsandbox"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_SANDBOX_ENDING_BLOCK);

    // close overall twig sandbox
    parser.complete(wrapper_m, SyntaxKind::TWIG_SANDBOX)
}

fn parse_twig_flush(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["flush"]));
    parser.bump();
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(outer, SyntaxKind::TWIG_FLUSH)
}

fn parse_twig_do(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["do"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        parser.recover(&[T!["%}"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(outer, SyntaxKind::TWIG_DO)
}

fn parse_twig_deprecated(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["deprecated"]));
    parser.bump();

    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else {
        parser.add_error(ParseErrorBuilder::new("twig deprecation message as string"));
        parser.recover(&[T!["%}"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(outer, SyntaxKind::TWIG_DEPRECATED)
}

fn parse_twig_autoescape(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["autoescape"]));
    parser.bump();

    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else if parser.at(T!["false"]) {
        parser.bump();
    } else if !parser.at(T!["%}"]) {
        parser.add_error(ParseErrorBuilder::new(
            "twig escape strategy as string or 'false'",
        ));
        parser.recover(&[T!["%}"], T!["endautoescape"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["endautoescape"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_AUTOESCAPE_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endautoescape
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endautoescape"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endautoescape"], T!["%}"], T!["</"]]);
    parser.expect(T!["endautoescape"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_AUTOESCAPE_ENDING_BLOCK);

    // close overall twig autoescape
    parser.complete(wrapper_m, SyntaxKind::TWIG_AUTOESCAPE)
}

fn parse_twig_apply(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["apply"]));
    parser.bump();

    // parse any amount of filters
    if let Some(mut node) = parse_twig_name(parser) {
        // parse optional arguments
        if parser.at(T!["("]) {
            // parse any amount of arguments
            let arguments_m = parser.start();
            parser.bump();
            parse_many(
                parser,
                |p| p.at_set(&[T!["%}"], T![")"]]),
                |p| {
                    parse_twig_function_argument(p);
                    if p.at(T![","]) {
                        p.bump();
                    } else if !p.at_set(&[T!["%}"], T![")"]]) {
                        p.add_error(ParseErrorBuilder::new(","));
                    }
                },
            );
            parser.expect(T![")"], &[T!["endapply"], T!["%}"], T!["</"]]);
            parser.complete(arguments_m, SyntaxKind::TWIG_ARGUMENTS);
        }

        // parse any amount of piped filters
        parse_many(
            parser,
            |p| p.at(T!["%}"]),
            |p| {
                if p.at(T!["|"]) {
                    node = parse_twig_filter(p, node.clone());
                }
            },
        );
    } else {
        parser.add_error(ParseErrorBuilder::new("twig filter"));
        parser.recover(&[T!["%}"], T!["endapply"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["endapply"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_APPLY_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endapply
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endapply"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endapply"], T!["%}"], T!["</"]]);
    parser.expect(T!["endapply"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_APPLY_ENDING_BLOCK);

    // close overall twig apply
    parser.complete(wrapper_m, SyntaxKind::TWIG_APPLY)
}

fn parse_twig_import(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["import"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template"));
        parser.recover(&[T!["as"], T!["%}"], T![word], T!["</"]]);
    }

    parser.expect(T!["as"], &[T!["%}"], T![word], T!["</"]]);

    if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("name for twig macro"));
        parser.recover(&[T!["%}"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["</"]]);

    parser.complete(outer, SyntaxKind::TWIG_IMPORT)
}

fn parse_twig_from(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["from"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template"));
        parser.recover(&[T!["import"], T!["%}"], T!["</"]]);
    }

    parser.expect(T!["import"], &[T!["%}"], T!["</"]]);

    let mut override_count = 0;
    parse_many(
        parser,
        |p| p.at(T!["%}"]),
        |p| {
            override_count += 1;
            parse_name_as_name_override(p, "macro name");
            if p.at(T![","]) {
                // consume optional comma
                p.bump();
            } else if !p.at(T!["%}"]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );

    if override_count < 1 {
        parser.add_error(ParseErrorBuilder::new(
            "at least one macro name as macro name",
        ));
    }

    parser.expect(T!["%}"], &[T!["</"]]);

    parser.complete(outer, SyntaxKind::TWIG_FROM)
}

fn parse_name_as_name_override(parser: &mut Parser, expected_description: &str) -> CompletedMarker {
    let override_m = parser.start();
    if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new(expected_description));
        parser.recover(&[T!["as"], T!["%}"], T!["</"]]);
    }
    if parser.at(T!["as"]) {
        parser.bump();
        if parse_twig_name(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new(expected_description));
            parser.recover(&[T!["%}"], T!["</"]]);
        }
    }
    parser.complete(override_m, SyntaxKind::TWIG_OVERRIDE)
}

fn parse_twig_use(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["use"]));
    parser.bump();

    if parser.at_set(&[T!["\""], T!["'"]]) {
        parse_twig_string(parser, false);
    } else {
        parser.add_error(ParseErrorBuilder::new("twig string as template"));
        parser.recover(&[T!["with"], T![word], T!["%}"], T!["</"]]);
    }

    if parser.at(T!["with"]) {
        parser.bump();

        let mut override_count = 0;
        parse_many(
            parser,
            |p| p.at(T!["%}"]),
            |p| {
                override_count += 1;
                parse_name_as_name_override(p, "block name");
                if p.at(T![","]) {
                    // consume optional comma
                    p.bump();
                } else if !p.at(T!["%}"]) {
                    p.add_error(ParseErrorBuilder::new(","));
                }
            },
        );

        if override_count < 1 {
            parser.add_error(ParseErrorBuilder::new(
                "at least one block name as block name",
            ));
        }
    }

    parser.expect(T!["%}"], &[T!["</"]]);

    parser.complete(outer, SyntaxKind::TWIG_USE)
}

fn parse_twig_embed(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["embed"]));
    parser.bump();

    // same arguments as include tag
    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template name"));
        parser.recover(&[
            T!["ignore missing"],
            T!["with"],
            T!["only"],
            T!["endembed"],
            T!["%}"],
            T!["</"],
        ]);
    }

    if parser.at(T!["ignore missing"]) {
        parser.bump();
    }

    if parser.at(T!["with"]) {
        let with_value_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression as with value"));
            parser.recover(&[T!["only"], T!["endembed"], T!["%}"], T!["</"]]);
        }
        parser.complete(with_value_m, SyntaxKind::TWIG_INCLUDE_WITH);
    }

    if parser.at(T!["only"]) {
        parser.bump();
    }

    parser.expect(T!["%}"], &[T!["endembed"], T!["%}"], T!["</"]]);

    // but embed has a body
    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_EMBED_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except endembed
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endembed"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endembed"], T!["%}"], T!["</"]]);
    parser.expect(T!["endembed"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_EMBED_ENDING_BLOCK);

    // close overall twig embed
    parser.complete(wrapper_m, SyntaxKind::TWIG_EMBED)
}

fn parse_twig_include(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["include"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression as template name"));
        parser.recover(&[
            T!["ignore missing"],
            T!["with"],
            T!["only"],
            T!["%}"],
            T!["</"],
        ]);
    }

    if parser.at(T!["ignore missing"]) {
        parser.bump();
    }

    if parser.at(T!["with"]) {
        let with_value_m = parser.start();
        parser.bump();
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression as with value"));
            parser.recover(&[T!["only"], T!["%}"], T!["</"]]);
        }
        parser.complete(with_value_m, SyntaxKind::TWIG_INCLUDE_WITH);
    }

    if parser.at(T!["only"]) {
        parser.bump();
    }

    parser.expect(T!["%}"], &[T!["</"]]);

    parser.complete(outer, SyntaxKind::TWIG_INCLUDE)
}

fn parse_twig_extends(parser: &mut Parser, outer: Marker) -> CompletedMarker {
    debug_assert!(parser.at(T!["extends"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        parser.recover(&[T!["%}"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["</"]]);

    parser.complete(outer, SyntaxKind::TWIG_EXTENDS)
}

fn parse_twig_for(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["for"]));
    parser.bump();

    // parse key, value identifiers
    if parse_twig_name(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("variable name"));
        parser.recover(&[
            T![","],
            T!["in"],
            T!["else"],
            T!["endfor"],
            T!["%}"],
            T!["</"],
        ]);
    }
    if parser.at(T![","]) {
        parser.bump();
        if parse_twig_name(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("variable name"));
            parser.recover(&[T!["in"], T!["else"], T!["endfor"], T!["%}"], T!["</"]]);
        }
    }

    parser.expect(T!["in"], &[T!["else"], T!["endfor"], T!["%}"], T!["</"]]);

    // parse expression after in
    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        parser.recover(&[T!["%}"], T!["else"], T!["endfor"], T!["</"]]);
    }

    parser.expect(T!["%}"], &[T!["else"], T!["endfor"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_FOR_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse all the children except else or endfor
    let body_m = parser.start();
    parse_many(
        parser,
        |p| p.at_following(&[T!["{%"], T!["endfor"]]) || p.at_following(&[T!["{%"], T!["else"]]),
        |p| {
            child_parser(p);
        },
    );
    parser.complete(body_m, SyntaxKind::BODY);

    // check for else block
    if parser.at_following(&[T!["{%"], T!["else"]]) {
        let else_m = parser.start();
        parser.bump();
        parser.bump();
        parser.expect(T!["%}"], &[T!["endfor"], T!["%}"], T!["</"]]);
        parser.complete(else_m, SyntaxKind::TWIG_FOR_ELSE_BLOCK);

        // parse all the children except endfor
        let body_m = parser.start();
        parse_many(
            parser,
            |p| p.at_following(&[T!["{%"], T!["endfor"]]),
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);
    }

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endfor"], T!["%}"], T!["</"]]);
    parser.expect(T!["endfor"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_ENDFOR_BLOCK);

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::TWIG_FOR)
}

fn parse_twig_set(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["set"]));
    parser.bump();

    // parse any amount of words seperated by comma
    let assignment_m = parser.start();
    let mut declaration_count = 0;
    parse_many(
        parser,
        |p| p.at_set(&[T!["="], T!["%}"]]),
        |p| {
            if parse_twig_name(p).is_some() {
                declaration_count += 1;
            } else {
                p.add_error(ParseErrorBuilder::new("twig variable name"));
            }

            if p.at(T![","]) {
                p.bump();
            } else if !p.at_set(&[T!["="], T!["%}"]]) {
                p.add_error(ParseErrorBuilder::new(","));
            }
        },
    );
    if declaration_count == 0 {
        parser.add_error(ParseErrorBuilder::new("twig variable name"));
    }

    // check for equal assignment
    let mut is_assigned_by_equal = false;
    if parser.at(T!["="]) {
        parser.bump();
        is_assigned_by_equal = true;

        let mut assignment_count = 0;
        parse_many(
            parser,
            |p| p.at(T!["%}"]),
            |p| {
                if parse_twig_expression(p).is_some() {
                    assignment_count += 1;
                } else {
                    p.add_error(ParseErrorBuilder::new("twig expression"));
                }

                if p.at(T![","]) {
                    p.bump();
                } else if !p.at(T!["%}"]) {
                    p.add_error(ParseErrorBuilder::new(","));
                }
            },
        );

        if declaration_count != assignment_count {
            parser.add_error(ParseErrorBuilder::new(format!(
                "a total of {declaration_count} twig expressions (same amount as declarations) instead of {assignment_count}"
            )));
        }
    } else if declaration_count > 1 {
        parser.add_error(ParseErrorBuilder::new(format!(
            "= followed by {declaration_count} twig expressions"
        )));
    }

    parser.complete(assignment_m, SyntaxKind::TWIG_ASSIGNMENT);
    parser.expect(T!["%}"], &[T!["endset"], T!["%}"], T!["</"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_SET_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    if !is_assigned_by_equal {
        // children and a closing endset should be there

        // parse all the children except endset
        let body_m = parser.start();
        parse_many(
            parser,
            |p| p.at_following(&[T!["{%"], T!["endset"]]),
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);

        let end_block_m = parser.start();
        parser.expect(T!["{%"], &[T!["endset"], T!["%}"], T!["</"]]);
        parser.expect(T!["endset"], &[T!["%}"], T!["</"]]);
        parser.expect(T!["%}"], &[T!["</"]]);
        parser.complete(end_block_m, SyntaxKind::TWIG_ENDSET_BLOCK);
    }

    // close overall twig set
    parser.complete(wrapper_m, SyntaxKind::TWIG_SET)
}

fn parse_twig_block(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["block"]));
    parser.bump();
    let block_name = parser
        .expect(T![word], &[T!["</"], T!["%}"], T!["endblock"]])
        .map(|t| t.text.to_owned());
    // look for optional shortcut
    let mut found_shortcut = false;
    if !parser.at(T!["%}"]) {
        if parse_twig_expression(parser).is_none() {
            parser.add_error(ParseErrorBuilder::new("twig expression or '%}'"));
            parser.recover(&[T!["%}"], T!["endblock"], T!["</"]]);
        } else {
            found_shortcut = true;
        }
    }
    parser.expect(T!["%}"], &[T!["</"], T!["endblock"], T!["%}"]]);

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_STARTING_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    if !found_shortcut {
        // parse all the children except endblock
        let body_m = parser.start();
        parse_many(
            parser,
            |p| p.at_following(&[T!["{%"], T!["endblock"]]),
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);

        let end_block_m = parser.start();
        parser.expect(T!["{%"], &[T!["</"], T!["endblock"], T!["%}"]]);
        parser.expect(T!["endblock"], &[T!["</"], T!["%}"]]);
        // check for optional name behind endblock
        if parser.at(T![word]) {
            let end_block_name_token = parser.bump();
            if let Some(block_name) = block_name {
                if end_block_name_token.text != block_name {
                    let parser_err = ParseErrorBuilder::new(format!(
                        "nothing or same twig block name as opening ({block_name})"
                    ))
                    .at_token(end_block_name_token);

                    parser.add_error(parser_err);
                    parser.recover(&[T!["%}"], T!["</"]]);
                }
            }
        }
        parser.expect(T!["%}"], &[T!["</"]]);
        parser.complete(end_block_m, SyntaxKind::TWIG_ENDING_BLOCK);
    }

    // close overall twig block
    parser.complete(wrapper_m, SyntaxKind::TWIG_BLOCK)
}

fn parse_twig_if(
    parser: &mut Parser,
    outer: Marker,
    child_parser: ParseFunction,
) -> CompletedMarker {
    debug_assert!(parser.at(T!["if"]));
    parser.bump();

    if parse_twig_expression(parser).is_none() {
        parser.add_error(ParseErrorBuilder::new("twig expression"));
        parser.recover(&[T!["%}"], T!["else"], T!["elseif"], T!["endif"], T!["</"]]);
    }
    parser.expect(
        T!["%}"],
        &[T!["else"], T!["elseif"], T!["endif"], T!["%}"], T!["</"]],
    );

    let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_IF_BLOCK);
    let wrapper_m = parser.precede(wrapper_m);

    // parse branches
    loop {
        // parse body (all the children)
        let body_m = parser.start();
        parse_many(
            parser,
            |p| {
                p.at_following(&[T!["{%"], T!["endif"]])
                    || p.at_following(&[T!["{%"], T!["elseif"]])
                    || p.at_following(&[T!["{%"], T!["else"]])
            },
            |p| {
                child_parser(p);
            },
        );
        parser.complete(body_m, SyntaxKind::BODY);

        if parser.at_following(&[T!["{%"], T!["endif"]]) {
            break; // no more branches
        }

        // parse next branch header
        if parser.at_following(&[T!["{%"], T!["elseif"]]) {
            let branch_m = parser.start();
            parser.bump();
            parser.bump();
            if parse_twig_expression(parser).is_none() {
                parser.add_error(ParseErrorBuilder::new("twig expression"));
                parser.recover(&[T!["%}"], T!["endif"], T!["</"]]);
            }
            parser.expect(T!["%}"], &[T!["endif"], T!["%}"], T!["</"]]);
            parser.complete(branch_m, SyntaxKind::TWIG_ELSE_IF_BLOCK);
        } else if parser.at_following(&[T!["{%"], T!["else"]]) {
            let branch_m = parser.start();
            parser.bump();
            parser.bump();
            parser.expect(T!["%}"], &[T!["endif"], T!["%}"], T!["</"]]);
            parser.complete(branch_m, SyntaxKind::TWIG_ELSE_BLOCK);
        } else {
            // not an actual branch found, the child parser has ended
            break;
        }
    }

    let end_block_m = parser.start();
    parser.expect(T!["{%"], &[T!["endif"], T!["%}"], T!["</"]]);
    parser.expect(T!["endif"], &[T!["%}"], T!["</"]]);
    parser.expect(T!["%}"], &[T!["</"]]);
    parser.complete(end_block_m, SyntaxKind::TWIG_ENDIF_BLOCK);

    parser.complete(wrapper_m, SyntaxKind::TWIG_IF)
}

fn parse_twig_trans(
  parser: &mut Parser,
  outer: Marker,
  child_parser: ParseFunction,
) -> CompletedMarker {
  debug_assert!(parser.at(T!["trans"]));
  parser.bump();

  parser.expect(
      T!["%}"],
      &[T!["endtrans"], T!["%}"], T!["</"]],
  );

  let wrapper_m = parser.complete(outer, SyntaxKind::TWIG_TRANS_STARTING_BLOCK);
  let wrapper_m = parser.precede(wrapper_m);

  // parse all the children except endtrans
  let body_m = parser.start();
  parse_many(
    parser,
    |p| {
        p.at_following(&[T!["{%"], T!["endtrans"]])
    },
    |p| {
        child_parser(p);
    },
  );
  parser.complete(body_m, SyntaxKind::BODY);

  let end_block_m = parser.start();
  parser.expect(T!["{%"], &[T!["endtrans"], T!["%}"], T!["</"]]);
  parser.expect(T!["endtrans"], &[T!["%}"], T!["</"]]);
  parser.expect(T!["%}"], &[T!["</"]]);
  parser.complete(end_block_m, SyntaxKind::TWIG_TRANS_ENDING_BLOCK);

  parser.complete(wrapper_m, SyntaxKind::TWIG_TRANS)
}

#[cfg(test)]
mod tests {
    use crate::parser::check_parse;
    use expect_test::expect;

    #[test]
    fn parse_error() {
        check_parse(
            "{% asdf",
            expect![[r#"
                ROOT@0..7
                  ERROR@0..2
                    TK_CURLY_PERCENT@0..2 "{%"
                  HTML_TEXT@2..7
                    TK_WHITESPACE@2..3 " "
                    TK_WORD@3..7 "asdf"
                error at 3..7: expected twig tag but found word"#]],
        );
    }

    #[test]
    fn parse_twig_block() {
        check_parse(
            "{% block block_name %} hello world {% endblock %}",
            expect![[r#"
            ROOT@0..49
              TWIG_BLOCK@0..49
                TWIG_STARTING_BLOCK@0..22
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..19 "block_name"
                  TK_WHITESPACE@19..20 " "
                  TK_PERCENT_CURLY@20..22 "%}"
                BODY@22..34
                  HTML_TEXT@22..34
                    TK_WHITESPACE@22..23 " "
                    TK_WORD@23..28 "hello"
                    TK_WHITESPACE@28..29 " "
                    TK_WORD@29..34 "world"
                TWIG_ENDING_BLOCK@34..49
                  TK_WHITESPACE@34..35 " "
                  TK_CURLY_PERCENT@35..37 "{%"
                  TK_WHITESPACE@37..38 " "
                  TK_ENDBLOCK@38..46 "endblock"
                  TK_WHITESPACE@46..47 " "
                  TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_nested_twig_blocks() {
        check_parse(
            "{% block outer %}\
                out\
                {% block middle %}\
                    mid\
                    {% block inner %}\
                    in\
                    {% endblock %}\
                    mid\
                {% endblock %}\
                out\
                {% endblock %}",
            expect![[r#"
            ROOT@0..108
              TWIG_BLOCK@0..108
                TWIG_STARTING_BLOCK@0..17
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..14 "outer"
                  TK_WHITESPACE@14..15 " "
                  TK_PERCENT_CURLY@15..17 "%}"
                BODY@17..94
                  HTML_TEXT@17..20
                    TK_WORD@17..20 "out"
                  TWIG_BLOCK@20..91
                    TWIG_STARTING_BLOCK@20..38
                      TK_CURLY_PERCENT@20..22 "{%"
                      TK_WHITESPACE@22..23 " "
                      TK_BLOCK@23..28 "block"
                      TK_WHITESPACE@28..29 " "
                      TK_WORD@29..35 "middle"
                      TK_WHITESPACE@35..36 " "
                      TK_PERCENT_CURLY@36..38 "%}"
                    BODY@38..77
                      HTML_TEXT@38..41
                        TK_WORD@38..41 "mid"
                      TWIG_BLOCK@41..74
                        TWIG_STARTING_BLOCK@41..58
                          TK_CURLY_PERCENT@41..43 "{%"
                          TK_WHITESPACE@43..44 " "
                          TK_BLOCK@44..49 "block"
                          TK_WHITESPACE@49..50 " "
                          TK_WORD@50..55 "inner"
                          TK_WHITESPACE@55..56 " "
                          TK_PERCENT_CURLY@56..58 "%}"
                        BODY@58..60
                          HTML_TEXT@58..60
                            TK_IN@58..60 "in"
                        TWIG_ENDING_BLOCK@60..74
                          TK_CURLY_PERCENT@60..62 "{%"
                          TK_WHITESPACE@62..63 " "
                          TK_ENDBLOCK@63..71 "endblock"
                          TK_WHITESPACE@71..72 " "
                          TK_PERCENT_CURLY@72..74 "%}"
                      HTML_TEXT@74..77
                        TK_WORD@74..77 "mid"
                    TWIG_ENDING_BLOCK@77..91
                      TK_CURLY_PERCENT@77..79 "{%"
                      TK_WHITESPACE@79..80 " "
                      TK_ENDBLOCK@80..88 "endblock"
                      TK_WHITESPACE@88..89 " "
                      TK_PERCENT_CURLY@89..91 "%}"
                  HTML_TEXT@91..94
                    TK_WORD@91..94 "out"
                TWIG_ENDING_BLOCK@94..108
                  TK_CURLY_PERCENT@94..96 "{%"
                  TK_WHITESPACE@96..97 " "
                  TK_ENDBLOCK@97..105 "endblock"
                  TK_WHITESPACE@105..106 " "
                  TK_PERCENT_CURLY@106..108 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if() {
        check_parse(
            "{% if isTrue %} true {% endif %}",
            expect![[r#"
            ROOT@0..32
              TWIG_IF@0..32
                TWIG_IF_BLOCK@0..15
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..12
                    TWIG_LITERAL_NAME@5..12
                      TK_WHITESPACE@5..6 " "
                      TK_WORD@6..12 "isTrue"
                  TK_WHITESPACE@12..13 " "
                  TK_PERCENT_CURLY@13..15 "%}"
                BODY@15..20
                  HTML_TEXT@15..20
                    TK_WHITESPACE@15..16 " "
                    TK_TRUE@16..20 "true"
                TWIG_ENDIF_BLOCK@20..32
                  TK_WHITESPACE@20..21 " "
                  TK_CURLY_PERCENT@21..23 "{%"
                  TK_WHITESPACE@23..24 " "
                  TK_ENDIF@24..29 "endif"
                  TK_WHITESPACE@29..30 " "
                  TK_PERCENT_CURLY@30..32 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if_condition_expression() {
        check_parse(
            "{% if temperature > 18 and temperature < 27 %} true {% endif %}",
            expect![[r#"
            ROOT@0..63
              TWIG_IF@0..63
                TWIG_IF_BLOCK@0..46
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..43
                    TWIG_BINARY_EXPRESSION@5..43
                      TWIG_BINARY_EXPRESSION@5..22
                        TWIG_EXPRESSION@5..17
                          TWIG_LITERAL_NAME@5..17
                            TK_WHITESPACE@5..6 " "
                            TK_WORD@6..17 "temperature"
                        TK_WHITESPACE@17..18 " "
                        TK_GREATER_THAN@18..19 ">"
                        TWIG_EXPRESSION@19..22
                          TWIG_LITERAL_NUMBER@19..22
                            TK_WHITESPACE@19..20 " "
                            TK_NUMBER@20..22 "18"
                      TK_WHITESPACE@22..23 " "
                      TK_AND@23..26 "and"
                      TWIG_EXPRESSION@26..43
                        TWIG_BINARY_EXPRESSION@26..43
                          TWIG_EXPRESSION@26..38
                            TWIG_LITERAL_NAME@26..38
                              TK_WHITESPACE@26..27 " "
                              TK_WORD@27..38 "temperature"
                          TK_WHITESPACE@38..39 " "
                          TK_LESS_THAN@39..40 "<"
                          TWIG_EXPRESSION@40..43
                            TWIG_LITERAL_NUMBER@40..43
                              TK_WHITESPACE@40..41 " "
                              TK_NUMBER@41..43 "27"
                  TK_WHITESPACE@43..44 " "
                  TK_PERCENT_CURLY@44..46 "%}"
                BODY@46..51
                  HTML_TEXT@46..51
                    TK_WHITESPACE@46..47 " "
                    TK_TRUE@47..51 "true"
                TWIG_ENDIF_BLOCK@51..63
                  TK_WHITESPACE@51..52 " "
                  TK_CURLY_PERCENT@52..54 "{%"
                  TK_WHITESPACE@54..55 " "
                  TK_ENDIF@55..60 "endif"
                  TK_WHITESPACE@60..61 " "
                  TK_PERCENT_CURLY@61..63 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if_else() {
        check_parse(
            "{% if isTrue %} true {% else %} false {% endif %}",
            expect![[r#"
            ROOT@0..49
              TWIG_IF@0..49
                TWIG_IF_BLOCK@0..15
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..12
                    TWIG_LITERAL_NAME@5..12
                      TK_WHITESPACE@5..6 " "
                      TK_WORD@6..12 "isTrue"
                  TK_WHITESPACE@12..13 " "
                  TK_PERCENT_CURLY@13..15 "%}"
                BODY@15..20
                  HTML_TEXT@15..20
                    TK_WHITESPACE@15..16 " "
                    TK_TRUE@16..20 "true"
                TWIG_ELSE_BLOCK@20..31
                  TK_WHITESPACE@20..21 " "
                  TK_CURLY_PERCENT@21..23 "{%"
                  TK_WHITESPACE@23..24 " "
                  TK_ELSE@24..28 "else"
                  TK_WHITESPACE@28..29 " "
                  TK_PERCENT_CURLY@29..31 "%}"
                BODY@31..37
                  HTML_TEXT@31..37
                    TK_WHITESPACE@31..32 " "
                    TK_FALSE@32..37 "false"
                TWIG_ENDIF_BLOCK@37..49
                  TK_WHITESPACE@37..38 " "
                  TK_CURLY_PERCENT@38..40 "{%"
                  TK_WHITESPACE@40..41 " "
                  TK_ENDIF@41..46 "endif"
                  TK_WHITESPACE@46..47 " "
                  TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if_elseif() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% endif %}",
            expect![[r#"
            ROOT@0..45
              TWIG_IF@0..45
                TWIG_IF_BLOCK@0..12
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..9
                    TWIG_LITERAL_NAME@5..9
                      TK_WHITESPACE@5..6 " "
                      TK_WORD@6..9 "isA"
                  TK_WHITESPACE@9..10 " "
                  TK_PERCENT_CURLY@10..12 "%}"
                BODY@12..14
                  HTML_TEXT@12..14
                    TK_WHITESPACE@12..13 " "
                    TK_WORD@13..14 "A"
                TWIG_ELSE_IF_BLOCK@14..31
                  TK_WHITESPACE@14..15 " "
                  TK_CURLY_PERCENT@15..17 "{%"
                  TK_WHITESPACE@17..18 " "
                  TK_ELSE_IF@18..24 "elseif"
                  TWIG_EXPRESSION@24..28
                    TWIG_LITERAL_NAME@24..28
                      TK_WHITESPACE@24..25 " "
                      TK_WORD@25..28 "isB"
                  TK_WHITESPACE@28..29 " "
                  TK_PERCENT_CURLY@29..31 "%}"
                BODY@31..33
                  HTML_TEXT@31..33
                    TK_WHITESPACE@31..32 " "
                    TK_WORD@32..33 "B"
                TWIG_ENDIF_BLOCK@33..45
                  TK_WHITESPACE@33..34 " "
                  TK_CURLY_PERCENT@34..36 "{%"
                  TK_WHITESPACE@36..37 " "
                  TK_ENDIF@37..42 "endif"
                  TK_WHITESPACE@42..43 " "
                  TK_PERCENT_CURLY@43..45 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if_elseif_else() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% else %} other {% endif %}",
            expect![[r#"
            ROOT@0..62
              TWIG_IF@0..62
                TWIG_IF_BLOCK@0..12
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..9
                    TWIG_LITERAL_NAME@5..9
                      TK_WHITESPACE@5..6 " "
                      TK_WORD@6..9 "isA"
                  TK_WHITESPACE@9..10 " "
                  TK_PERCENT_CURLY@10..12 "%}"
                BODY@12..14
                  HTML_TEXT@12..14
                    TK_WHITESPACE@12..13 " "
                    TK_WORD@13..14 "A"
                TWIG_ELSE_IF_BLOCK@14..31
                  TK_WHITESPACE@14..15 " "
                  TK_CURLY_PERCENT@15..17 "{%"
                  TK_WHITESPACE@17..18 " "
                  TK_ELSE_IF@18..24 "elseif"
                  TWIG_EXPRESSION@24..28
                    TWIG_LITERAL_NAME@24..28
                      TK_WHITESPACE@24..25 " "
                      TK_WORD@25..28 "isB"
                  TK_WHITESPACE@28..29 " "
                  TK_PERCENT_CURLY@29..31 "%}"
                BODY@31..33
                  HTML_TEXT@31..33
                    TK_WHITESPACE@31..32 " "
                    TK_WORD@32..33 "B"
                TWIG_ELSE_BLOCK@33..44
                  TK_WHITESPACE@33..34 " "
                  TK_CURLY_PERCENT@34..36 "{%"
                  TK_WHITESPACE@36..37 " "
                  TK_ELSE@37..41 "else"
                  TK_WHITESPACE@41..42 " "
                  TK_PERCENT_CURLY@42..44 "%}"
                BODY@44..50
                  HTML_TEXT@44..50
                    TK_WHITESPACE@44..45 " "
                    TK_WORD@45..50 "other"
                TWIG_ENDIF_BLOCK@50..62
                  TK_WHITESPACE@50..51 " "
                  TK_CURLY_PERCENT@51..53 "{%"
                  TK_WHITESPACE@53..54 " "
                  TK_ENDIF@54..59 "endif"
                  TK_WHITESPACE@59..60 " "
                  TK_PERCENT_CURLY@60..62 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_if_elseif_elseif_else() {
        check_parse(
            "{% if isA %} A {% elseif isB %} B {% elseif isC %} C {% else %} other {% endif %}",
            expect![[r#"
            ROOT@0..81
              TWIG_IF@0..81
                TWIG_IF_BLOCK@0..12
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_IF@3..5 "if"
                  TWIG_EXPRESSION@5..9
                    TWIG_LITERAL_NAME@5..9
                      TK_WHITESPACE@5..6 " "
                      TK_WORD@6..9 "isA"
                  TK_WHITESPACE@9..10 " "
                  TK_PERCENT_CURLY@10..12 "%}"
                BODY@12..14
                  HTML_TEXT@12..14
                    TK_WHITESPACE@12..13 " "
                    TK_WORD@13..14 "A"
                TWIG_ELSE_IF_BLOCK@14..31
                  TK_WHITESPACE@14..15 " "
                  TK_CURLY_PERCENT@15..17 "{%"
                  TK_WHITESPACE@17..18 " "
                  TK_ELSE_IF@18..24 "elseif"
                  TWIG_EXPRESSION@24..28
                    TWIG_LITERAL_NAME@24..28
                      TK_WHITESPACE@24..25 " "
                      TK_WORD@25..28 "isB"
                  TK_WHITESPACE@28..29 " "
                  TK_PERCENT_CURLY@29..31 "%}"
                BODY@31..33
                  HTML_TEXT@31..33
                    TK_WHITESPACE@31..32 " "
                    TK_WORD@32..33 "B"
                TWIG_ELSE_IF_BLOCK@33..50
                  TK_WHITESPACE@33..34 " "
                  TK_CURLY_PERCENT@34..36 "{%"
                  TK_WHITESPACE@36..37 " "
                  TK_ELSE_IF@37..43 "elseif"
                  TWIG_EXPRESSION@43..47
                    TWIG_LITERAL_NAME@43..47
                      TK_WHITESPACE@43..44 " "
                      TK_WORD@44..47 "isC"
                  TK_WHITESPACE@47..48 " "
                  TK_PERCENT_CURLY@48..50 "%}"
                BODY@50..52
                  HTML_TEXT@50..52
                    TK_WHITESPACE@50..51 " "
                    TK_WORD@51..52 "C"
                TWIG_ELSE_BLOCK@52..63
                  TK_WHITESPACE@52..53 " "
                  TK_CURLY_PERCENT@53..55 "{%"
                  TK_WHITESPACE@55..56 " "
                  TK_ELSE@56..60 "else"
                  TK_WHITESPACE@60..61 " "
                  TK_PERCENT_CURLY@61..63 "%}"
                BODY@63..69
                  HTML_TEXT@63..69
                    TK_WHITESPACE@63..64 " "
                    TK_WORD@64..69 "other"
                TWIG_ENDIF_BLOCK@69..81
                  TK_WHITESPACE@69..70 " "
                  TK_CURLY_PERCENT@70..72 "{%"
                  TK_WHITESPACE@72..73 " "
                  TK_ENDIF@73..78 "endif"
                  TK_WHITESPACE@78..79 " "
                  TK_PERCENT_CURLY@79..81 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_block_with_unknown_body() {
        check_parse(
            "{% block my_block %} \\t unknown token {% endblock %}",
            expect![[r#"
            ROOT@0..52
              TWIG_BLOCK@0..52
                TWIG_STARTING_BLOCK@0..20
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..17 "my_block"
                  TK_WHITESPACE@17..18 " "
                  TK_PERCENT_CURLY@18..20 "%}"
                BODY@20..37
                  HTML_TEXT@20..37
                    TK_WHITESPACE@20..21 " "
                    TK_BACKWARD_SLASH@21..22 "\\"
                    TK_WORD@22..23 "t"
                    TK_WHITESPACE@23..24 " "
                    TK_WORD@24..31 "unknown"
                    TK_WHITESPACE@31..32 " "
                    TK_WORD@32..37 "token"
                TWIG_ENDING_BLOCK@37..52
                  TK_WHITESPACE@37..38 " "
                  TK_CURLY_PERCENT@38..40 "{%"
                  TK_WHITESPACE@40..41 " "
                  TK_ENDBLOCK@41..49 "endblock"
                  TK_WHITESPACE@49..50 " "
                  TK_PERCENT_CURLY@50..52 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_block_without_endblock() {
        check_parse(
            "{% block my_block %}",
            expect![[r#"
            ROOT@0..20
              TWIG_BLOCK@0..20
                TWIG_STARTING_BLOCK@0..20
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..17 "my_block"
                  TK_WHITESPACE@17..18 " "
                  TK_PERCENT_CURLY@18..20 "%}"
                BODY@20..20
                TWIG_ENDING_BLOCK@20..20
            error at 18..20: expected {% but reached end of file
            error at 18..20: expected endblock but reached end of file
            error at 18..20: expected %} but reached end of file"#]],
        );
    }

    #[test]
    fn parse_twig_block_with_named_endlbock() {
        check_parse(
            "{% block sidebar %}
    {% block inner_sidebar %}
        ...
    {% endblock inner_sidebar %}
{% endblock sidebar %}",
            expect![[r#"
            ROOT@0..117
              TWIG_BLOCK@0..117
                TWIG_STARTING_BLOCK@0..19
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..16 "sidebar"
                  TK_WHITESPACE@16..17 " "
                  TK_PERCENT_CURLY@17..19 "%}"
                BODY@19..94
                  TWIG_BLOCK@19..94
                    TWIG_STARTING_BLOCK@19..49
                      TK_LINE_BREAK@19..20 "\n"
                      TK_WHITESPACE@20..24 "    "
                      TK_CURLY_PERCENT@24..26 "{%"
                      TK_WHITESPACE@26..27 " "
                      TK_BLOCK@27..32 "block"
                      TK_WHITESPACE@32..33 " "
                      TK_WORD@33..46 "inner_sidebar"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}"
                    BODY@49..61
                      HTML_TEXT@49..61
                        TK_LINE_BREAK@49..50 "\n"
                        TK_WHITESPACE@50..58 "        "
                        TK_DOUBLE_DOT@58..60 ".."
                        TK_DOT@60..61 "."
                    TWIG_ENDING_BLOCK@61..94
                      TK_LINE_BREAK@61..62 "\n"
                      TK_WHITESPACE@62..66 "    "
                      TK_CURLY_PERCENT@66..68 "{%"
                      TK_WHITESPACE@68..69 " "
                      TK_ENDBLOCK@69..77 "endblock"
                      TK_WHITESPACE@77..78 " "
                      TK_WORD@78..91 "inner_sidebar"
                      TK_WHITESPACE@91..92 " "
                      TK_PERCENT_CURLY@92..94 "%}"
                TWIG_ENDING_BLOCK@94..117
                  TK_LINE_BREAK@94..95 "\n"
                  TK_CURLY_PERCENT@95..97 "{%"
                  TK_WHITESPACE@97..98 " "
                  TK_ENDBLOCK@98..106 "endblock"
                  TK_WHITESPACE@106..107 " "
                  TK_WORD@107..114 "sidebar"
                  TK_WHITESPACE@114..115 " "
                  TK_PERCENT_CURLY@115..117 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_block_with_named_endlbock_mismatch() {
        check_parse(
            "{% block sidebar %}
    {% block inner_sidebar %}
        ...
    {% endblock sidebar %}
{% endblock sidebar %}",
            expect![[r#"
            ROOT@0..111
              TWIG_BLOCK@0..111
                TWIG_STARTING_BLOCK@0..19
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_BLOCK@3..8 "block"
                  TK_WHITESPACE@8..9 " "
                  TK_WORD@9..16 "sidebar"
                  TK_WHITESPACE@16..17 " "
                  TK_PERCENT_CURLY@17..19 "%}"
                BODY@19..88
                  TWIG_BLOCK@19..88
                    TWIG_STARTING_BLOCK@19..49
                      TK_LINE_BREAK@19..20 "\n"
                      TK_WHITESPACE@20..24 "    "
                      TK_CURLY_PERCENT@24..26 "{%"
                      TK_WHITESPACE@26..27 " "
                      TK_BLOCK@27..32 "block"
                      TK_WHITESPACE@32..33 " "
                      TK_WORD@33..46 "inner_sidebar"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}"
                    BODY@49..61
                      HTML_TEXT@49..61
                        TK_LINE_BREAK@49..50 "\n"
                        TK_WHITESPACE@50..58 "        "
                        TK_DOUBLE_DOT@58..60 ".."
                        TK_DOT@60..61 "."
                    TWIG_ENDING_BLOCK@61..88
                      TK_LINE_BREAK@61..62 "\n"
                      TK_WHITESPACE@62..66 "    "
                      TK_CURLY_PERCENT@66..68 "{%"
                      TK_WHITESPACE@68..69 " "
                      TK_ENDBLOCK@69..77 "endblock"
                      TK_WHITESPACE@77..78 " "
                      TK_WORD@78..85 "sidebar"
                      TK_WHITESPACE@85..86 " "
                      TK_PERCENT_CURLY@86..88 "%}"
                TWIG_ENDING_BLOCK@88..111
                  TK_LINE_BREAK@88..89 "\n"
                  TK_CURLY_PERCENT@89..91 "{%"
                  TK_WHITESPACE@91..92 " "
                  TK_ENDBLOCK@92..100 "endblock"
                  TK_WHITESPACE@100..101 " "
                  TK_WORD@101..108 "sidebar"
                  TK_WHITESPACE@108..109 " "
                  TK_PERCENT_CURLY@109..111 "%}"
            error at 78..85: expected nothing or same twig block name as opening (inner_sidebar) but found word"#]],
        );
    }

    #[test]
    fn parse_twig_block_shortcut() {
        check_parse(
            "{% block title page_title|title %}",
            expect![[r#"
                ROOT@0..34
                  TWIG_BLOCK@0..34
                    TWIG_STARTING_BLOCK@0..34
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_BLOCK@3..8 "block"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "title"
                      TWIG_EXPRESSION@14..31
                        TWIG_FILTER@14..31
                          TWIG_OPERAND@14..25
                            TWIG_LITERAL_NAME@14..25
                              TK_WHITESPACE@14..15 " "
                              TK_WORD@15..25 "page_title"
                          TK_SINGLE_PIPE@25..26 "|"
                          TWIG_OPERAND@26..31
                            TWIG_LITERAL_NAME@26..31
                              TK_WORD@26..31 "title"
                      TK_WHITESPACE@31..32 " "
                      TK_PERCENT_CURLY@32..34 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_single_set() {
        check_parse(
            "{% set foo = 'bar' %}",
            expect![[r#"
        ROOT@0..21
          TWIG_SET@0..21
            TWIG_SET_BLOCK@0..21
              TK_CURLY_PERCENT@0..2 "{%"
              TK_WHITESPACE@2..3 " "
              TK_SET@3..6 "set"
              TWIG_ASSIGNMENT@6..18
                TWIG_LITERAL_NAME@6..10
                  TK_WHITESPACE@6..7 " "
                  TK_WORD@7..10 "foo"
                TK_WHITESPACE@10..11 " "
                TK_EQUAL@11..12 "="
                TWIG_EXPRESSION@12..18
                  TWIG_LITERAL_STRING@12..18
                    TK_WHITESPACE@12..13 " "
                    TK_SINGLE_QUOTES@13..14 "'"
                    TWIG_LITERAL_STRING_INNER@14..17
                      TK_WORD@14..17 "bar"
                    TK_SINGLE_QUOTES@17..18 "'"
              TK_WHITESPACE@18..19 " "
              TK_PERCENT_CURLY@19..21 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_capturing_set() {
        check_parse(
            r#"{% set foo %}
    hello world
{% endset %}"#,
            expect![[r#"
                ROOT@0..42
                  TWIG_SET@0..42
                    TWIG_SET_BLOCK@0..13
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..10
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                      TK_WHITESPACE@10..11 " "
                      TK_PERCENT_CURLY@11..13 "%}"
                    BODY@13..29
                      HTML_TEXT@13..29
                        TK_LINE_BREAK@13..14 "\n"
                        TK_WHITESPACE@14..18 "    "
                        TK_WORD@18..23 "hello"
                        TK_WHITESPACE@23..24 " "
                        TK_WORD@24..29 "world"
                    TWIG_ENDSET_BLOCK@29..42
                      TK_LINE_BREAK@29..30 "\n"
                      TK_CURLY_PERCENT@30..32 "{%"
                      TK_WHITESPACE@32..33 " "
                      TK_ENDSET@33..39 "endset"
                      TK_WHITESPACE@39..40 " "
                      TK_PERCENT_CURLY@40..42 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_multi_set() {
        check_parse(
            r#"{% set foo, bar, baz = 'foo', 'bar', 'baz' %}"#,
            expect![[r#"
            ROOT@0..45
              TWIG_SET@0..45
                TWIG_SET_BLOCK@0..45
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_SET@3..6 "set"
                  TWIG_ASSIGNMENT@6..42
                    TWIG_LITERAL_NAME@6..10
                      TK_WHITESPACE@6..7 " "
                      TK_WORD@7..10 "foo"
                    TK_COMMA@10..11 ","
                    TWIG_LITERAL_NAME@11..15
                      TK_WHITESPACE@11..12 " "
                      TK_WORD@12..15 "bar"
                    TK_COMMA@15..16 ","
                    TWIG_LITERAL_NAME@16..20
                      TK_WHITESPACE@16..17 " "
                      TK_WORD@17..20 "baz"
                    TK_WHITESPACE@20..21 " "
                    TK_EQUAL@21..22 "="
                    TWIG_EXPRESSION@22..28
                      TWIG_LITERAL_STRING@22..28
                        TK_WHITESPACE@22..23 " "
                        TK_SINGLE_QUOTES@23..24 "'"
                        TWIG_LITERAL_STRING_INNER@24..27
                          TK_WORD@24..27 "foo"
                        TK_SINGLE_QUOTES@27..28 "'"
                    TK_COMMA@28..29 ","
                    TWIG_EXPRESSION@29..35
                      TWIG_LITERAL_STRING@29..35
                        TK_WHITESPACE@29..30 " "
                        TK_SINGLE_QUOTES@30..31 "'"
                        TWIG_LITERAL_STRING_INNER@31..34
                          TK_WORD@31..34 "bar"
                        TK_SINGLE_QUOTES@34..35 "'"
                    TK_COMMA@35..36 ","
                    TWIG_EXPRESSION@36..42
                      TWIG_LITERAL_STRING@36..42
                        TK_WHITESPACE@36..37 " "
                        TK_SINGLE_QUOTES@37..38 "'"
                        TWIG_LITERAL_STRING_INNER@38..41
                          TK_WORD@38..41 "baz"
                        TK_SINGLE_QUOTES@41..42 "'"
                  TK_WHITESPACE@42..43 " "
                  TK_PERCENT_CURLY@43..45 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_multi_set_non_equal_declarations() {
        check_parse(
            r#"{% set foo, bar, baz = 'foo', 'bar' %}"#,
            expect![[r#"
            ROOT@0..38
              TWIG_SET@0..38
                TWIG_SET_BLOCK@0..38
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_SET@3..6 "set"
                  TWIG_ASSIGNMENT@6..35
                    TWIG_LITERAL_NAME@6..10
                      TK_WHITESPACE@6..7 " "
                      TK_WORD@7..10 "foo"
                    TK_COMMA@10..11 ","
                    TWIG_LITERAL_NAME@11..15
                      TK_WHITESPACE@11..12 " "
                      TK_WORD@12..15 "bar"
                    TK_COMMA@15..16 ","
                    TWIG_LITERAL_NAME@16..20
                      TK_WHITESPACE@16..17 " "
                      TK_WORD@17..20 "baz"
                    TK_WHITESPACE@20..21 " "
                    TK_EQUAL@21..22 "="
                    TWIG_EXPRESSION@22..28
                      TWIG_LITERAL_STRING@22..28
                        TK_WHITESPACE@22..23 " "
                        TK_SINGLE_QUOTES@23..24 "'"
                        TWIG_LITERAL_STRING_INNER@24..27
                          TK_WORD@24..27 "foo"
                        TK_SINGLE_QUOTES@27..28 "'"
                    TK_COMMA@28..29 ","
                    TWIG_EXPRESSION@29..35
                      TWIG_LITERAL_STRING@29..35
                        TK_WHITESPACE@29..30 " "
                        TK_SINGLE_QUOTES@30..31 "'"
                        TWIG_LITERAL_STRING_INNER@31..34
                          TK_WORD@31..34 "bar"
                        TK_SINGLE_QUOTES@34..35 "'"
                  TK_WHITESPACE@35..36 " "
                  TK_PERCENT_CURLY@36..38 "%}"
            error at 36..38: expected a total of 3 twig expressions (same amount as declarations) instead of 2 but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_multi_with_capturing() {
        check_parse(
            r#"{% set foo, bar, baz %}
    hello world
{% endset %}"#,
            expect![[r#"
                ROOT@0..52
                  TWIG_SET@0..52
                    TWIG_SET_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..20
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..39
                      HTML_TEXT@23..39
                        TK_LINE_BREAK@23..24 "\n"
                        TK_WHITESPACE@24..28 "    "
                        TK_WORD@28..33 "hello"
                        TK_WHITESPACE@33..34 " "
                        TK_WORD@34..39 "world"
                    TWIG_ENDSET_BLOCK@39..52
                      TK_LINE_BREAK@39..40 "\n"
                      TK_CURLY_PERCENT@40..42 "{%"
                      TK_WHITESPACE@42..43 " "
                      TK_ENDSET@43..49 "endset"
                      TK_WHITESPACE@49..50 " "
                      TK_PERCENT_CURLY@50..52 "%}"
                error at 21..23: expected = followed by 3 twig expressions but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_set_missing_declaration() {
        check_parse(
            r#"{% set = 'foo' %}"#,
            expect![[r#"
                ROOT@0..17
                  TWIG_SET@0..17
                    TWIG_SET_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..14
                        TK_WHITESPACE@6..7 " "
                        TK_EQUAL@7..8 "="
                        TWIG_EXPRESSION@8..14
                          TWIG_LITERAL_STRING@8..14
                            TK_WHITESPACE@8..9 " "
                            TK_SINGLE_QUOTES@9..10 "'"
                            TWIG_LITERAL_STRING_INNER@10..13
                              TK_WORD@10..13 "foo"
                            TK_SINGLE_QUOTES@13..14 "'"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                error at 7..8: expected twig variable name but found =
                error at 15..17: expected a total of 0 twig expressions (same amount as declarations) instead of 1 but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_set_missing_assignment() {
        check_parse(
            r#"{% set foo = %}"#,
            expect![[r#"
                ROOT@0..15
                  TWIG_SET@0..15
                    TWIG_SET_BLOCK@0..15
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..12
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_WHITESPACE@10..11 " "
                        TK_EQUAL@11..12 "="
                      TK_WHITESPACE@12..13 " "
                      TK_PERCENT_CURLY@13..15 "%}"
                error at 13..15: expected a total of 1 twig expressions (same amount as declarations) instead of 0 but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_set_missing_equal() {
        check_parse(
            r#"{% set foo, bar, baz %}"#,
            expect![[r#"
                ROOT@0..23
                  TWIG_SET@0..23
                    TWIG_SET_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..20
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "foo"
                        TK_COMMA@10..11 ","
                        TWIG_LITERAL_NAME@11..15
                          TK_WHITESPACE@11..12 " "
                          TK_WORD@12..15 "bar"
                        TK_COMMA@15..16 ","
                        TWIG_LITERAL_NAME@16..20
                          TK_WHITESPACE@16..17 " "
                          TK_WORD@17..20 "baz"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..23
                    TWIG_ENDSET_BLOCK@23..23
                error at 21..23: expected = followed by 3 twig expressions but found %}
                error at 21..23: expected {% but reached end of file
                error at 21..23: expected endset but reached end of file
                error at 21..23: expected %} but reached end of file"#]],
        );
    }

    #[test]
    fn parse_twig_for_in_users() {
        check_parse(
            r#"{% for user in users %}
    <li>{{ user.username }}</li>
{% endfor %}"#,
            expect![[r#"
                ROOT@0..69
                  TWIG_FOR@0..69
                    TWIG_FOR_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TWIG_EXPRESSION@14..20
                        TWIG_LITERAL_NAME@14..20
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..20 "users"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..56
                      HTML_TAG@23..56
                        HTML_STARTING_TAG@23..32
                          TK_LINE_BREAK@23..24 "\n"
                          TK_WHITESPACE@24..28 "    "
                          TK_LESS_THAN@28..29 "<"
                          TK_WORD@29..31 "li"
                          HTML_ATTRIBUTE_LIST@31..31
                          TK_GREATER_THAN@31..32 ">"
                        BODY@32..51
                          TWIG_VAR@32..51
                            TK_OPEN_CURLY_CURLY@32..34 "{{"
                            TWIG_EXPRESSION@34..48
                              TWIG_ACCESSOR@34..48
                                TWIG_OPERAND@34..39
                                  TWIG_LITERAL_NAME@34..39
                                    TK_WHITESPACE@34..35 " "
                                    TK_WORD@35..39 "user"
                                TK_DOT@39..40 "."
                                TWIG_OPERAND@40..48
                                  TWIG_LITERAL_NAME@40..48
                                    TK_WORD@40..48 "username"
                            TK_WHITESPACE@48..49 " "
                            TK_CLOSE_CURLY_CURLY@49..51 "}}"
                        HTML_ENDING_TAG@51..56
                          TK_LESS_THAN_SLASH@51..53 "</"
                          TK_WORD@53..55 "li"
                          TK_GREATER_THAN@55..56 ">"
                    TWIG_ENDFOR_BLOCK@56..69
                      TK_LINE_BREAK@56..57 "\n"
                      TK_CURLY_PERCENT@57..59 "{%"
                      TK_WHITESPACE@59..60 " "
                      TK_ENDFOR@60..66 "endfor"
                      TK_WHITESPACE@66..67 " "
                      TK_PERCENT_CURLY@67..69 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_for_in_number_range() {
        check_parse(
            r#"{% for i in 0..10 %}
    * {{ i }}
{% endfor %}"#,
            expect![[r#"
            ROOT@0..47
              TWIG_FOR@0..47
                TWIG_FOR_BLOCK@0..20
                  TK_CURLY_PERCENT@0..2 "{%"
                  TK_WHITESPACE@2..3 " "
                  TK_FOR@3..6 "for"
                  TWIG_LITERAL_NAME@6..8
                    TK_WHITESPACE@6..7 " "
                    TK_WORD@7..8 "i"
                  TK_WHITESPACE@8..9 " "
                  TK_IN@9..11 "in"
                  TWIG_EXPRESSION@11..17
                    TWIG_BINARY_EXPRESSION@11..17
                      TWIG_EXPRESSION@11..13
                        TWIG_LITERAL_NUMBER@11..13
                          TK_WHITESPACE@11..12 " "
                          TK_NUMBER@12..13 "0"
                      TK_DOUBLE_DOT@13..15 ".."
                      TWIG_EXPRESSION@15..17
                        TWIG_LITERAL_NUMBER@15..17
                          TK_NUMBER@15..17 "10"
                  TK_WHITESPACE@17..18 " "
                  TK_PERCENT_CURLY@18..20 "%}"
                BODY@20..34
                  HTML_TEXT@20..26
                    TK_LINE_BREAK@20..21 "\n"
                    TK_WHITESPACE@21..25 "    "
                    TK_STAR@25..26 "*"
                  TWIG_VAR@26..34
                    TK_WHITESPACE@26..27 " "
                    TK_OPEN_CURLY_CURLY@27..29 "{{"
                    TWIG_EXPRESSION@29..31
                      TWIG_LITERAL_NAME@29..31
                        TK_WHITESPACE@29..30 " "
                        TK_WORD@30..31 "i"
                    TK_WHITESPACE@31..32 " "
                    TK_CLOSE_CURLY_CURLY@32..34 "}}"
                TWIG_ENDFOR_BLOCK@34..47
                  TK_LINE_BREAK@34..35 "\n"
                  TK_CURLY_PERCENT@35..37 "{%"
                  TK_WHITESPACE@37..38 " "
                  TK_ENDFOR@38..44 "endfor"
                  TK_WHITESPACE@44..45 " "
                  TK_PERCENT_CURLY@45..47 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_for_in_letter_range_with_filters() {
        check_parse(
            r#"{% for letter in 'a'|upper..'z'|upper %}
    * {{ letter }}
{% endfor %}"#,
            expect![[r#"
                ROOT@0..72
                  TWIG_FOR@0..72
                    TWIG_FOR_BLOCK@0..40
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..13
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..13 "letter"
                      TK_WHITESPACE@13..14 " "
                      TK_IN@14..16 "in"
                      TWIG_EXPRESSION@16..37
                        TWIG_BINARY_EXPRESSION@16..37
                          TWIG_EXPRESSION@16..26
                            TWIG_FILTER@16..26
                              TWIG_OPERAND@16..20
                                TWIG_LITERAL_STRING@16..20
                                  TK_WHITESPACE@16..17 " "
                                  TK_SINGLE_QUOTES@17..18 "'"
                                  TWIG_LITERAL_STRING_INNER@18..19
                                    TK_WORD@18..19 "a"
                                  TK_SINGLE_QUOTES@19..20 "'"
                              TK_SINGLE_PIPE@20..21 "|"
                              TWIG_OPERAND@21..26
                                TWIG_LITERAL_NAME@21..26
                                  TK_WORD@21..26 "upper"
                          TK_DOUBLE_DOT@26..28 ".."
                          TWIG_EXPRESSION@28..37
                            TWIG_FILTER@28..37
                              TWIG_OPERAND@28..31
                                TWIG_LITERAL_STRING@28..31
                                  TK_SINGLE_QUOTES@28..29 "'"
                                  TWIG_LITERAL_STRING_INNER@29..30
                                    TK_WORD@29..30 "z"
                                  TK_SINGLE_QUOTES@30..31 "'"
                              TK_SINGLE_PIPE@31..32 "|"
                              TWIG_OPERAND@32..37
                                TWIG_LITERAL_NAME@32..37
                                  TK_WORD@32..37 "upper"
                      TK_WHITESPACE@37..38 " "
                      TK_PERCENT_CURLY@38..40 "%}"
                    BODY@40..59
                      HTML_TEXT@40..46
                        TK_LINE_BREAK@40..41 "\n"
                        TK_WHITESPACE@41..45 "    "
                        TK_STAR@45..46 "*"
                      TWIG_VAR@46..59
                        TK_WHITESPACE@46..47 " "
                        TK_OPEN_CURLY_CURLY@47..49 "{{"
                        TWIG_EXPRESSION@49..56
                          TWIG_LITERAL_NAME@49..56
                            TK_WHITESPACE@49..50 " "
                            TK_WORD@50..56 "letter"
                        TK_WHITESPACE@56..57 " "
                        TK_CLOSE_CURLY_CURLY@57..59 "}}"
                    TWIG_ENDFOR_BLOCK@59..72
                      TK_LINE_BREAK@59..60 "\n"
                      TK_CURLY_PERCENT@60..62 "{%"
                      TK_WHITESPACE@62..63 " "
                      TK_ENDFOR@63..69 "endfor"
                      TK_WHITESPACE@69..70 " "
                      TK_PERCENT_CURLY@70..72 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_for_key_value_in_users() {
        check_parse(
            r#"{% for key, user in users %}
    <li>{{ key }}: {{ user.username|e }}</li>
{% endfor %}"#,
            expect![[r#"
                ROOT@0..87
                  TWIG_FOR@0..87
                    TWIG_FOR_BLOCK@0..28
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..10
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..10 "key"
                      TK_COMMA@10..11 ","
                      TWIG_LITERAL_NAME@11..16
                        TK_WHITESPACE@11..12 " "
                        TK_WORD@12..16 "user"
                      TK_WHITESPACE@16..17 " "
                      TK_IN@17..19 "in"
                      TWIG_EXPRESSION@19..25
                        TWIG_LITERAL_NAME@19..25
                          TK_WHITESPACE@19..20 " "
                          TK_WORD@20..25 "users"
                      TK_WHITESPACE@25..26 " "
                      TK_PERCENT_CURLY@26..28 "%}"
                    BODY@28..74
                      HTML_TAG@28..74
                        HTML_STARTING_TAG@28..37
                          TK_LINE_BREAK@28..29 "\n"
                          TK_WHITESPACE@29..33 "    "
                          TK_LESS_THAN@33..34 "<"
                          TK_WORD@34..36 "li"
                          HTML_ATTRIBUTE_LIST@36..36
                          TK_GREATER_THAN@36..37 ">"
                        BODY@37..69
                          TWIG_VAR@37..46
                            TK_OPEN_CURLY_CURLY@37..39 "{{"
                            TWIG_EXPRESSION@39..43
                              TWIG_LITERAL_NAME@39..43
                                TK_WHITESPACE@39..40 " "
                                TK_WORD@40..43 "key"
                            TK_WHITESPACE@43..44 " "
                            TK_CLOSE_CURLY_CURLY@44..46 "}}"
                          HTML_TEXT@46..47
                            TK_COLON@46..47 ":"
                          TWIG_VAR@47..69
                            TK_WHITESPACE@47..48 " "
                            TK_OPEN_CURLY_CURLY@48..50 "{{"
                            TWIG_EXPRESSION@50..66
                              TWIG_FILTER@50..66
                                TWIG_OPERAND@50..64
                                  TWIG_ACCESSOR@50..64
                                    TWIG_OPERAND@50..55
                                      TWIG_LITERAL_NAME@50..55
                                        TK_WHITESPACE@50..51 " "
                                        TK_WORD@51..55 "user"
                                    TK_DOT@55..56 "."
                                    TWIG_OPERAND@56..64
                                      TWIG_LITERAL_NAME@56..64
                                        TK_WORD@56..64 "username"
                                TK_SINGLE_PIPE@64..65 "|"
                                TWIG_OPERAND@65..66
                                  TWIG_LITERAL_NAME@65..66
                                    TK_WORD@65..66 "e"
                            TK_WHITESPACE@66..67 " "
                            TK_CLOSE_CURLY_CURLY@67..69 "}}"
                        HTML_ENDING_TAG@69..74
                          TK_LESS_THAN_SLASH@69..71 "</"
                          TK_WORD@71..73 "li"
                          TK_GREATER_THAN@73..74 ">"
                    TWIG_ENDFOR_BLOCK@74..87
                      TK_LINE_BREAK@74..75 "\n"
                      TK_CURLY_PERCENT@75..77 "{%"
                      TK_WHITESPACE@77..78 " "
                      TK_ENDFOR@78..84 "endfor"
                      TK_WHITESPACE@84..85 " "
                      TK_PERCENT_CURLY@85..87 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_for_with_else() {
        check_parse(
            r#"{% for user in users %}
    <li>{{ user.username }}</li>
{% else %}
    <li><em>no user found</em></li>
{% endfor %}"#,
            expect![[r#"
                ROOT@0..116
                  TWIG_FOR@0..116
                    TWIG_FOR_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TWIG_EXPRESSION@14..20
                        TWIG_LITERAL_NAME@14..20
                          TK_WHITESPACE@14..15 " "
                          TK_WORD@15..20 "users"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..56
                      HTML_TAG@23..56
                        HTML_STARTING_TAG@23..32
                          TK_LINE_BREAK@23..24 "\n"
                          TK_WHITESPACE@24..28 "    "
                          TK_LESS_THAN@28..29 "<"
                          TK_WORD@29..31 "li"
                          HTML_ATTRIBUTE_LIST@31..31
                          TK_GREATER_THAN@31..32 ">"
                        BODY@32..51
                          TWIG_VAR@32..51
                            TK_OPEN_CURLY_CURLY@32..34 "{{"
                            TWIG_EXPRESSION@34..48
                              TWIG_ACCESSOR@34..48
                                TWIG_OPERAND@34..39
                                  TWIG_LITERAL_NAME@34..39
                                    TK_WHITESPACE@34..35 " "
                                    TK_WORD@35..39 "user"
                                TK_DOT@39..40 "."
                                TWIG_OPERAND@40..48
                                  TWIG_LITERAL_NAME@40..48
                                    TK_WORD@40..48 "username"
                            TK_WHITESPACE@48..49 " "
                            TK_CLOSE_CURLY_CURLY@49..51 "}}"
                        HTML_ENDING_TAG@51..56
                          TK_LESS_THAN_SLASH@51..53 "</"
                          TK_WORD@53..55 "li"
                          TK_GREATER_THAN@55..56 ">"
                    TWIG_FOR_ELSE_BLOCK@56..67
                      TK_LINE_BREAK@56..57 "\n"
                      TK_CURLY_PERCENT@57..59 "{%"
                      TK_WHITESPACE@59..60 " "
                      TK_ELSE@60..64 "else"
                      TK_WHITESPACE@64..65 " "
                      TK_PERCENT_CURLY@65..67 "%}"
                    BODY@67..103
                      HTML_TAG@67..103
                        HTML_STARTING_TAG@67..76
                          TK_LINE_BREAK@67..68 "\n"
                          TK_WHITESPACE@68..72 "    "
                          TK_LESS_THAN@72..73 "<"
                          TK_WORD@73..75 "li"
                          HTML_ATTRIBUTE_LIST@75..75
                          TK_GREATER_THAN@75..76 ">"
                        BODY@76..98
                          HTML_TAG@76..98
                            HTML_STARTING_TAG@76..80
                              TK_LESS_THAN@76..77 "<"
                              TK_WORD@77..79 "em"
                              HTML_ATTRIBUTE_LIST@79..79
                              TK_GREATER_THAN@79..80 ">"
                            BODY@80..93
                              HTML_TEXT@80..93
                                TK_WORD@80..82 "no"
                                TK_WHITESPACE@82..83 " "
                                TK_WORD@83..87 "user"
                                TK_WHITESPACE@87..88 " "
                                TK_WORD@88..93 "found"
                            HTML_ENDING_TAG@93..98
                              TK_LESS_THAN_SLASH@93..95 "</"
                              TK_WORD@95..97 "em"
                              TK_GREATER_THAN@97..98 ">"
                        HTML_ENDING_TAG@98..103
                          TK_LESS_THAN_SLASH@98..100 "</"
                          TK_WORD@100..102 "li"
                          TK_GREATER_THAN@102..103 ">"
                    TWIG_ENDFOR_BLOCK@103..116
                      TK_LINE_BREAK@103..104 "\n"
                      TK_CURLY_PERCENT@104..106 "{%"
                      TK_WHITESPACE@106..107 " "
                      TK_ENDFOR@107..113 "endfor"
                      TK_WHITESPACE@113..114 " "
                      TK_PERCENT_CURLY@114..116 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_for_with_missing_variable() {
        check_parse(
            r#"{% for in users %}
    <li>{{ user.username }}</li>
{% endfor %}"#,
            expect![[r#"
                ROOT@0..64
                  TWIG_FOR@0..64
                    TWIG_FOR_BLOCK@0..18
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..9
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..9 "in"
                      ERROR@9..15
                        TK_WHITESPACE@9..10 " "
                        TK_WORD@10..15 "users"
                      TK_WHITESPACE@15..16 " "
                      TK_PERCENT_CURLY@16..18 "%}"
                    BODY@18..51
                      HTML_TAG@18..51
                        HTML_STARTING_TAG@18..27
                          TK_LINE_BREAK@18..19 "\n"
                          TK_WHITESPACE@19..23 "    "
                          TK_LESS_THAN@23..24 "<"
                          TK_WORD@24..26 "li"
                          HTML_ATTRIBUTE_LIST@26..26
                          TK_GREATER_THAN@26..27 ">"
                        BODY@27..46
                          TWIG_VAR@27..46
                            TK_OPEN_CURLY_CURLY@27..29 "{{"
                            TWIG_EXPRESSION@29..43
                              TWIG_ACCESSOR@29..43
                                TWIG_OPERAND@29..34
                                  TWIG_LITERAL_NAME@29..34
                                    TK_WHITESPACE@29..30 " "
                                    TK_WORD@30..34 "user"
                                TK_DOT@34..35 "."
                                TWIG_OPERAND@35..43
                                  TWIG_LITERAL_NAME@35..43
                                    TK_WORD@35..43 "username"
                            TK_WHITESPACE@43..44 " "
                            TK_CLOSE_CURLY_CURLY@44..46 "}}"
                        HTML_ENDING_TAG@46..51
                          TK_LESS_THAN_SLASH@46..48 "</"
                          TK_WORD@48..50 "li"
                          TK_GREATER_THAN@50..51 ">"
                    TWIG_ENDFOR_BLOCK@51..64
                      TK_LINE_BREAK@51..52 "\n"
                      TK_CURLY_PERCENT@52..54 "{%"
                      TK_WHITESPACE@54..55 " "
                      TK_ENDFOR@55..61 "endfor"
                      TK_WHITESPACE@61..62 " "
                      TK_PERCENT_CURLY@62..64 "%}"
                error at 10..15: expected in but found word
                error at 16..18: expected twig expression but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_for_with_missing_expression() {
        check_parse(
            r#"{% for user in %}
    <li>{{ user.username }}</li>
{% endfor %}"#,
            expect![[r#"
                ROOT@0..63
                  TWIG_FOR@0..63
                    TWIG_FOR_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_FOR@3..6 "for"
                      TWIG_LITERAL_NAME@6..11
                        TK_WHITESPACE@6..7 " "
                        TK_WORD@7..11 "user"
                      TK_WHITESPACE@11..12 " "
                      TK_IN@12..14 "in"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                    BODY@17..50
                      HTML_TAG@17..50
                        HTML_STARTING_TAG@17..26
                          TK_LINE_BREAK@17..18 "\n"
                          TK_WHITESPACE@18..22 "    "
                          TK_LESS_THAN@22..23 "<"
                          TK_WORD@23..25 "li"
                          HTML_ATTRIBUTE_LIST@25..25
                          TK_GREATER_THAN@25..26 ">"
                        BODY@26..45
                          TWIG_VAR@26..45
                            TK_OPEN_CURLY_CURLY@26..28 "{{"
                            TWIG_EXPRESSION@28..42
                              TWIG_ACCESSOR@28..42
                                TWIG_OPERAND@28..33
                                  TWIG_LITERAL_NAME@28..33
                                    TK_WHITESPACE@28..29 " "
                                    TK_WORD@29..33 "user"
                                TK_DOT@33..34 "."
                                TWIG_OPERAND@34..42
                                  TWIG_LITERAL_NAME@34..42
                                    TK_WORD@34..42 "username"
                            TK_WHITESPACE@42..43 " "
                            TK_CLOSE_CURLY_CURLY@43..45 "}}"
                        HTML_ENDING_TAG@45..50
                          TK_LESS_THAN_SLASH@45..47 "</"
                          TK_WORD@47..49 "li"
                          TK_GREATER_THAN@49..50 ">"
                    TWIG_ENDFOR_BLOCK@50..63
                      TK_LINE_BREAK@50..51 "\n"
                      TK_CURLY_PERCENT@51..53 "{%"
                      TK_WHITESPACE@53..54 " "
                      TK_ENDFOR@54..60 "endfor"
                      TK_WHITESPACE@60..61 " "
                      TK_PERCENT_CURLY@61..63 "%}"
                error at 15..17: expected twig expression but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_extends_with_string() {
        check_parse(
            r#"{% extends "base.html" %}"#,
            expect![[r#"
        ROOT@0..25
          TWIG_EXTENDS@0..25
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_EXTENDS@3..10 "extends"
            TWIG_EXPRESSION@10..22
              TWIG_LITERAL_STRING@10..22
                TK_WHITESPACE@10..11 " "
                TK_DOUBLE_QUOTES@11..12 "\""
                TWIG_LITERAL_STRING_INNER@12..21
                  TK_WORD@12..16 "base"
                  TK_DOT@16..17 "."
                  TK_WORD@17..21 "html"
                TK_DOUBLE_QUOTES@21..22 "\""
            TK_WHITESPACE@22..23 " "
            TK_PERCENT_CURLY@23..25 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_extends_with_variable() {
        check_parse(
            r#"{% extends some_var %}"#,
            expect![[r#"
        ROOT@0..22
          TWIG_EXTENDS@0..22
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_EXTENDS@3..10 "extends"
            TWIG_EXPRESSION@10..19
              TWIG_LITERAL_NAME@10..19
                TK_WHITESPACE@10..11 " "
                TK_WORD@11..19 "some_var"
            TK_WHITESPACE@19..20 " "
            TK_PERCENT_CURLY@20..22 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_extends_with_array() {
        check_parse(
            r#"{% extends ['layout.html', 'base_layout.html'] %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_EXTENDS@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_EXTENDS@3..10 "extends"
                    TWIG_EXPRESSION@10..46
                      TWIG_LITERAL_ARRAY@10..46
                        TK_WHITESPACE@10..11 " "
                        TK_OPEN_SQUARE@11..12 "["
                        TWIG_LITERAL_ARRAY_INNER@12..45
                          TWIG_EXPRESSION@12..25
                            TWIG_LITERAL_STRING@12..25
                              TK_SINGLE_QUOTES@12..13 "'"
                              TWIG_LITERAL_STRING_INNER@13..24
                                TK_WORD@13..19 "layout"
                                TK_DOT@19..20 "."
                                TK_WORD@20..24 "html"
                              TK_SINGLE_QUOTES@24..25 "'"
                          TK_COMMA@25..26 ","
                          TWIG_EXPRESSION@26..45
                            TWIG_LITERAL_STRING@26..45
                              TK_WHITESPACE@26..27 " "
                              TK_SINGLE_QUOTES@27..28 "'"
                              TWIG_LITERAL_STRING_INNER@28..44
                                TK_WORD@28..39 "base_layout"
                                TK_DOT@39..40 "."
                                TK_WORD@40..44 "html"
                              TK_SINGLE_QUOTES@44..45 "'"
                        TK_CLOSE_SQUARE@45..46 "]"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_extends_with_conditional() {
        check_parse(
            r#"{% extends standalone ? "minimum.html" : "base.html" %}"#,
            expect![[r#"
            ROOT@0..55
              TWIG_EXTENDS@0..55
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_EXTENDS@3..10 "extends"
                TWIG_EXPRESSION@10..52
                  TWIG_CONDITIONAL_EXPRESSION@10..52
                    TWIG_EXPRESSION@10..21
                      TWIG_LITERAL_NAME@10..21
                        TK_WHITESPACE@10..11 " "
                        TK_WORD@11..21 "standalone"
                    TK_WHITESPACE@21..22 " "
                    TK_QUESTION_MARK@22..23 "?"
                    TWIG_EXPRESSION@23..38
                      TWIG_LITERAL_STRING@23..38
                        TK_WHITESPACE@23..24 " "
                        TK_DOUBLE_QUOTES@24..25 "\""
                        TWIG_LITERAL_STRING_INNER@25..37
                          TK_WORD@25..32 "minimum"
                          TK_DOT@32..33 "."
                          TK_WORD@33..37 "html"
                        TK_DOUBLE_QUOTES@37..38 "\""
                    TK_WHITESPACE@38..39 " "
                    TK_COLON@39..40 ":"
                    TWIG_EXPRESSION@40..52
                      TWIG_LITERAL_STRING@40..52
                        TK_WHITESPACE@40..41 " "
                        TK_DOUBLE_QUOTES@41..42 "\""
                        TWIG_LITERAL_STRING_INNER@42..51
                          TK_WORD@42..46 "base"
                          TK_DOT@46..47 "."
                          TK_WORD@47..51 "html"
                        TK_DOUBLE_QUOTES@51..52 "\""
                TK_WHITESPACE@52..53 " "
                TK_PERCENT_CURLY@53..55 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_extends_missing_expression() {
        check_parse(
            r#"{% extends %}"#,
            expect![[r#"
        ROOT@0..13
          TWIG_EXTENDS@0..13
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_EXTENDS@3..10 "extends"
            TK_WHITESPACE@10..11 " "
            TK_PERCENT_CURLY@11..13 "%}"
        error at 11..13: expected twig expression but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_include_string() {
        check_parse(
            r#"{% include 'header.html' %}"#,
            expect![[r#"
        ROOT@0..27
          TWIG_INCLUDE@0..27
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_INCLUDE@3..10 "include"
            TWIG_EXPRESSION@10..24
              TWIG_LITERAL_STRING@10..24
                TK_WHITESPACE@10..11 " "
                TK_SINGLE_QUOTES@11..12 "'"
                TWIG_LITERAL_STRING_INNER@12..23
                  TK_WORD@12..18 "header"
                  TK_DOT@18..19 "."
                  TK_WORD@19..23 "html"
                TK_SINGLE_QUOTES@23..24 "'"
            TK_WHITESPACE@24..25 " "
            TK_PERCENT_CURLY@25..27 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_with_variable() {
        check_parse(
            r#"{% include 'template.html' with vars %}"#,
            expect![[r#"
            ROOT@0..39
              TWIG_INCLUDE@0..39
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TWIG_EXPRESSION@10..26
                  TWIG_LITERAL_STRING@10..26
                    TK_WHITESPACE@10..11 " "
                    TK_SINGLE_QUOTES@11..12 "'"
                    TWIG_LITERAL_STRING_INNER@12..25
                      TK_WORD@12..20 "template"
                      TK_DOT@20..21 "."
                      TK_WORD@21..25 "html"
                    TK_SINGLE_QUOTES@25..26 "'"
                TWIG_INCLUDE_WITH@26..36
                  TK_WHITESPACE@26..27 " "
                  TK_WITH@27..31 "with"
                  TWIG_EXPRESSION@31..36
                    TWIG_LITERAL_NAME@31..36
                      TK_WHITESPACE@31..32 " "
                      TK_WORD@32..36 "vars"
                TK_WHITESPACE@36..37 " "
                TK_PERCENT_CURLY@37..39 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_with_hash() {
        check_parse(
            r#"{% include 'template.html' with {'foo': 'bar'} %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_INCLUDE@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..46
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                      TWIG_EXPRESSION@31..46
                        TWIG_LITERAL_HASH@31..46
                          TK_WHITESPACE@31..32 " "
                          TK_OPEN_CURLY@32..33 "{"
                          TWIG_LITERAL_HASH_ITEMS@33..45
                            TWIG_LITERAL_HASH_PAIR@33..45
                              TWIG_LITERAL_HASH_KEY@33..38
                                TWIG_LITERAL_STRING@33..38
                                  TK_SINGLE_QUOTES@33..34 "'"
                                  TWIG_LITERAL_STRING_INNER@34..37
                                    TK_WORD@34..37 "foo"
                                  TK_SINGLE_QUOTES@37..38 "'"
                              TK_COLON@38..39 ":"
                              TWIG_EXPRESSION@39..45
                                TWIG_LITERAL_STRING@39..45
                                  TK_WHITESPACE@39..40 " "
                                  TK_SINGLE_QUOTES@40..41 "'"
                                  TWIG_LITERAL_STRING_INNER@41..44
                                    TK_WORD@41..44 "bar"
                                  TK_SINGLE_QUOTES@44..45 "'"
                          TK_CLOSE_CURLY@45..46 "}"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_with_hash_only() {
        check_parse(
            r#"{% include 'template.html' with {'foo': 'bar'} only %}"#,
            expect![[r#"
                ROOT@0..54
                  TWIG_INCLUDE@0..54
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..26
                      TWIG_LITERAL_STRING@10..26
                        TK_WHITESPACE@10..11 " "
                        TK_SINGLE_QUOTES@11..12 "'"
                        TWIG_LITERAL_STRING_INNER@12..25
                          TK_WORD@12..20 "template"
                          TK_DOT@20..21 "."
                          TK_WORD@21..25 "html"
                        TK_SINGLE_QUOTES@25..26 "'"
                    TWIG_INCLUDE_WITH@26..46
                      TK_WHITESPACE@26..27 " "
                      TK_WITH@27..31 "with"
                      TWIG_EXPRESSION@31..46
                        TWIG_LITERAL_HASH@31..46
                          TK_WHITESPACE@31..32 " "
                          TK_OPEN_CURLY@32..33 "{"
                          TWIG_LITERAL_HASH_ITEMS@33..45
                            TWIG_LITERAL_HASH_PAIR@33..45
                              TWIG_LITERAL_HASH_KEY@33..38
                                TWIG_LITERAL_STRING@33..38
                                  TK_SINGLE_QUOTES@33..34 "'"
                                  TWIG_LITERAL_STRING_INNER@34..37
                                    TK_WORD@34..37 "foo"
                                  TK_SINGLE_QUOTES@37..38 "'"
                              TK_COLON@38..39 ":"
                              TWIG_EXPRESSION@39..45
                                TWIG_LITERAL_STRING@39..45
                                  TK_WHITESPACE@39..40 " "
                                  TK_SINGLE_QUOTES@40..41 "'"
                                  TWIG_LITERAL_STRING_INNER@41..44
                                    TK_WORD@41..44 "bar"
                                  TK_SINGLE_QUOTES@44..45 "'"
                          TK_CLOSE_CURLY@45..46 "}"
                    TK_WHITESPACE@46..47 " "
                    TK_ONLY@47..51 "only"
                    TK_WHITESPACE@51..52 " "
                    TK_PERCENT_CURLY@52..54 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_only() {
        check_parse(
            r#"{% include 'template.html' only %}"#,
            expect![[r#"
        ROOT@0..34
          TWIG_INCLUDE@0..34
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_INCLUDE@3..10 "include"
            TWIG_EXPRESSION@10..26
              TWIG_LITERAL_STRING@10..26
                TK_WHITESPACE@10..11 " "
                TK_SINGLE_QUOTES@11..12 "'"
                TWIG_LITERAL_STRING_INNER@12..25
                  TK_WORD@12..20 "template"
                  TK_DOT@20..21 "."
                  TK_WORD@21..25 "html"
                TK_SINGLE_QUOTES@25..26 "'"
            TK_WHITESPACE@26..27 " "
            TK_ONLY@27..31 "only"
            TK_WHITESPACE@31..32 " "
            TK_PERCENT_CURLY@32..34 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_expression() {
        check_parse(
            r#"{% include ajax ? 'ajax.html' : 'not_ajax.html' %}"#,
            expect![[r#"
            ROOT@0..50
              TWIG_INCLUDE@0..50
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TWIG_EXPRESSION@10..47
                  TWIG_CONDITIONAL_EXPRESSION@10..47
                    TWIG_EXPRESSION@10..15
                      TWIG_LITERAL_NAME@10..15
                        TK_WHITESPACE@10..11 " "
                        TK_WORD@11..15 "ajax"
                    TK_WHITESPACE@15..16 " "
                    TK_QUESTION_MARK@16..17 "?"
                    TWIG_EXPRESSION@17..29
                      TWIG_LITERAL_STRING@17..29
                        TK_WHITESPACE@17..18 " "
                        TK_SINGLE_QUOTES@18..19 "'"
                        TWIG_LITERAL_STRING_INNER@19..28
                          TK_WORD@19..23 "ajax"
                          TK_DOT@23..24 "."
                          TK_WORD@24..28 "html"
                        TK_SINGLE_QUOTES@28..29 "'"
                    TK_WHITESPACE@29..30 " "
                    TK_COLON@30..31 ":"
                    TWIG_EXPRESSION@31..47
                      TWIG_LITERAL_STRING@31..47
                        TK_WHITESPACE@31..32 " "
                        TK_SINGLE_QUOTES@32..33 "'"
                        TWIG_LITERAL_STRING_INNER@33..46
                          TK_WORD@33..41 "not_ajax"
                          TK_DOT@41..42 "."
                          TK_WORD@42..46 "html"
                        TK_SINGLE_QUOTES@46..47 "'"
                TK_WHITESPACE@47..48 " "
                TK_PERCENT_CURLY@48..50 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_variable_ignore_missing_with_hash_only() {
        check_parse(
            r#"{% include some_var ignore missing with {'foo': 'bar'} only %}"#,
            expect![[r#"
                ROOT@0..62
                  TWIG_INCLUDE@0..62
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..19
                      TWIG_LITERAL_NAME@10..19
                        TK_WHITESPACE@10..11 " "
                        TK_WORD@11..19 "some_var"
                    TK_WHITESPACE@19..20 " "
                    TK_IGNORE_MISSING@20..34 "ignore missing"
                    TWIG_INCLUDE_WITH@34..54
                      TK_WHITESPACE@34..35 " "
                      TK_WITH@35..39 "with"
                      TWIG_EXPRESSION@39..54
                        TWIG_LITERAL_HASH@39..54
                          TK_WHITESPACE@39..40 " "
                          TK_OPEN_CURLY@40..41 "{"
                          TWIG_LITERAL_HASH_ITEMS@41..53
                            TWIG_LITERAL_HASH_PAIR@41..53
                              TWIG_LITERAL_HASH_KEY@41..46
                                TWIG_LITERAL_STRING@41..46
                                  TK_SINGLE_QUOTES@41..42 "'"
                                  TWIG_LITERAL_STRING_INNER@42..45
                                    TK_WORD@42..45 "foo"
                                  TK_SINGLE_QUOTES@45..46 "'"
                              TK_COLON@46..47 ":"
                              TWIG_EXPRESSION@47..53
                                TWIG_LITERAL_STRING@47..53
                                  TK_WHITESPACE@47..48 " "
                                  TK_SINGLE_QUOTES@48..49 "'"
                                  TWIG_LITERAL_STRING_INNER@49..52
                                    TK_WORD@49..52 "bar"
                                  TK_SINGLE_QUOTES@52..53 "'"
                          TK_CLOSE_CURLY@53..54 "}"
                    TK_WHITESPACE@54..55 " "
                    TK_ONLY@55..59 "only"
                    TK_WHITESPACE@59..60 " "
                    TK_PERCENT_CURLY@60..62 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_include_missing_template() {
        check_parse(
            r#"{% include %}"#,
            expect![[r#"
        ROOT@0..13
          TWIG_INCLUDE@0..13
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_INCLUDE@3..10 "include"
            TK_WHITESPACE@10..11 " "
            TK_PERCENT_CURLY@11..13 "%}"
        error at 11..13: expected twig expression as template name but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_include_missing_with_value() {
        check_parse(
            r#"{% include 'template.html' with %}"#,
            expect![[r#"
            ROOT@0..34
              TWIG_INCLUDE@0..34
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_INCLUDE@3..10 "include"
                TWIG_EXPRESSION@10..26
                  TWIG_LITERAL_STRING@10..26
                    TK_WHITESPACE@10..11 " "
                    TK_SINGLE_QUOTES@11..12 "'"
                    TWIG_LITERAL_STRING_INNER@12..25
                      TK_WORD@12..20 "template"
                      TK_DOT@20..21 "."
                      TK_WORD@21..25 "html"
                    TK_SINGLE_QUOTES@25..26 "'"
                TWIG_INCLUDE_WITH@26..31
                  TK_WHITESPACE@26..27 " "
                  TK_WITH@27..31 "with"
                TK_WHITESPACE@31..32 " "
                TK_PERCENT_CURLY@32..34 "%}"
            error at 32..34: expected twig expression as with value but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_include_array() {
        check_parse(
            r#"{% include ['page_detailed.html', 'page.html'] %}"#,
            expect![[r#"
                ROOT@0..49
                  TWIG_INCLUDE@0..49
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_INCLUDE@3..10 "include"
                    TWIG_EXPRESSION@10..46
                      TWIG_LITERAL_ARRAY@10..46
                        TK_WHITESPACE@10..11 " "
                        TK_OPEN_SQUARE@11..12 "["
                        TWIG_LITERAL_ARRAY_INNER@12..45
                          TWIG_EXPRESSION@12..32
                            TWIG_LITERAL_STRING@12..32
                              TK_SINGLE_QUOTES@12..13 "'"
                              TWIG_LITERAL_STRING_INNER@13..31
                                TK_WORD@13..26 "page_detailed"
                                TK_DOT@26..27 "."
                                TK_WORD@27..31 "html"
                              TK_SINGLE_QUOTES@31..32 "'"
                          TK_COMMA@32..33 ","
                          TWIG_EXPRESSION@33..45
                            TWIG_LITERAL_STRING@33..45
                              TK_WHITESPACE@33..34 " "
                              TK_SINGLE_QUOTES@34..35 "'"
                              TWIG_LITERAL_STRING_INNER@35..44
                                TK_WORD@35..39 "page"
                                TK_DOT@39..40 "."
                                TK_WORD@40..44 "html"
                              TK_SINGLE_QUOTES@44..45 "'"
                        TK_CLOSE_SQUARE@45..46 "]"
                    TK_WHITESPACE@46..47 " "
                    TK_PERCENT_CURLY@47..49 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_use_string() {
        check_parse(
            r#"{% use "blocks.html" %}"#,
            expect![[r#"
        ROOT@0..23
          TWIG_USE@0..23
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_USE@3..6 "use"
            TWIG_LITERAL_STRING@6..20
              TK_WHITESPACE@6..7 " "
              TK_DOUBLE_QUOTES@7..8 "\""
              TWIG_LITERAL_STRING_INNER@8..19
                TK_WORD@8..14 "blocks"
                TK_DOT@14..15 "."
                TK_WORD@15..19 "html"
              TK_DOUBLE_QUOTES@19..20 "\""
            TK_WHITESPACE@20..21 " "
            TK_PERCENT_CURLY@21..23 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_use_interpolated_string() {
        // should not! be parsed as an interpolated string, because here only plain strings are allowed
        // also should add a parser error that this is not supported
        check_parse(
            r#"{% use "blocks#{1+1}.html" %}"#,
            expect![[r##"
                ROOT@0..29
                  TWIG_USE@0..29
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_USE@3..6 "use"
                    TWIG_LITERAL_STRING@6..26
                      TK_WHITESPACE@6..7 " "
                      TK_DOUBLE_QUOTES@7..8 "\""
                      TWIG_LITERAL_STRING_INNER@8..25
                        TK_WORD@8..14 "blocks"
                        TK_HASHTAG@14..15 "#"
                        TK_OPEN_CURLY@15..16 "{"
                        TK_NUMBER@16..17 "1"
                        TK_PLUS@17..18 "+"
                        TK_NUMBER@18..19 "1"
                        TK_CLOSE_CURLY@19..20 "}"
                        TK_DOT@20..21 "."
                        TK_WORD@21..25 "html"
                      TK_DOUBLE_QUOTES@25..26 "\""
                    TK_WHITESPACE@26..27 " "
                    TK_PERCENT_CURLY@27..29 "%}"
                error at 14..15: expected no string interpolation, because it isn't allowed here but found #"##]],
        );
    }

    #[test]
    fn parse_twig_use_string_with_as() {
        check_parse(
            r#"{% use "blocks.html" with sidebar as base_sidebar, title as base_title %}"#,
            expect![[r#"
                ROOT@0..73
                  TWIG_USE@0..73
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_USE@3..6 "use"
                    TWIG_LITERAL_STRING@6..20
                      TK_WHITESPACE@6..7 " "
                      TK_DOUBLE_QUOTES@7..8 "\""
                      TWIG_LITERAL_STRING_INNER@8..19
                        TK_WORD@8..14 "blocks"
                        TK_DOT@14..15 "."
                        TK_WORD@15..19 "html"
                      TK_DOUBLE_QUOTES@19..20 "\""
                    TK_WHITESPACE@20..21 " "
                    TK_WITH@21..25 "with"
                    TWIG_OVERRIDE@25..49
                      TWIG_LITERAL_NAME@25..33
                        TK_WHITESPACE@25..26 " "
                        TK_WORD@26..33 "sidebar"
                      TK_WHITESPACE@33..34 " "
                      TK_AS@34..36 "as"
                      TWIG_LITERAL_NAME@36..49
                        TK_WHITESPACE@36..37 " "
                        TK_WORD@37..49 "base_sidebar"
                    TK_COMMA@49..50 ","
                    TWIG_OVERRIDE@50..70
                      TWIG_LITERAL_NAME@50..56
                        TK_WHITESPACE@50..51 " "
                        TK_WORD@51..56 "title"
                      TK_WHITESPACE@56..57 " "
                      TK_AS@57..59 "as"
                      TWIG_LITERAL_NAME@59..70
                        TK_WHITESPACE@59..60 " "
                        TK_WORD@60..70 "base_title"
                    TK_WHITESPACE@70..71 " "
                    TK_PERCENT_CURLY@71..73 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_use_string_with_missing() {
        check_parse(
            r#"{% use "blocks.html" with %}"#,
            expect![[r#"
        ROOT@0..28
          TWIG_USE@0..28
            TK_CURLY_PERCENT@0..2 "{%"
            TK_WHITESPACE@2..3 " "
            TK_USE@3..6 "use"
            TWIG_LITERAL_STRING@6..20
              TK_WHITESPACE@6..7 " "
              TK_DOUBLE_QUOTES@7..8 "\""
              TWIG_LITERAL_STRING_INNER@8..19
                TK_WORD@8..14 "blocks"
                TK_DOT@14..15 "."
                TK_WORD@15..19 "html"
              TK_DOUBLE_QUOTES@19..20 "\""
            TK_WHITESPACE@20..21 " "
            TK_WITH@21..25 "with"
            TK_WHITESPACE@25..26 " "
            TK_PERCENT_CURLY@26..28 "%}"
        error at 26..28: expected at least one block name as block name but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_use_string_with_name_as_missing() {
        check_parse(
            r#"{% use "blocks.html" with a %}"#,
            expect![[r#"
                ROOT@0..30
                  TWIG_USE@0..30
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_USE@3..6 "use"
                    TWIG_LITERAL_STRING@6..20
                      TK_WHITESPACE@6..7 " "
                      TK_DOUBLE_QUOTES@7..8 "\""
                      TWIG_LITERAL_STRING_INNER@8..19
                        TK_WORD@8..14 "blocks"
                        TK_DOT@14..15 "."
                        TK_WORD@15..19 "html"
                      TK_DOUBLE_QUOTES@19..20 "\""
                    TK_WHITESPACE@20..21 " "
                    TK_WITH@21..25 "with"
                    TWIG_OVERRIDE@25..27
                      TWIG_LITERAL_NAME@25..27
                        TK_WHITESPACE@25..26 " "
                        TK_WORD@26..27 "a"
                    TK_WHITESPACE@27..28 " "
                    TK_PERCENT_CURLY@28..30 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_apply_filter() {
        check_parse(
            r#"{% apply upper %}
    This text becomes uppercase
{% endapply %}"#,
            expect![[r#"
                ROOT@0..64
                  TWIG_APPLY@0..64
                    TWIG_APPLY_STARTING_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_APPLY@3..8 "apply"
                      TWIG_LITERAL_NAME@8..14
                        TK_WHITESPACE@8..9 " "
                        TK_WORD@9..14 "upper"
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                    BODY@17..49
                      HTML_TEXT@17..49
                        TK_LINE_BREAK@17..18 "\n"
                        TK_WHITESPACE@18..22 "    "
                        TK_WORD@22..26 "This"
                        TK_WHITESPACE@26..27 " "
                        TK_WORD@27..31 "text"
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..39 "becomes"
                        TK_WHITESPACE@39..40 " "
                        TK_WORD@40..49 "uppercase"
                    TWIG_APPLY_ENDING_BLOCK@49..64
                      TK_LINE_BREAK@49..50 "\n"
                      TK_CURLY_PERCENT@50..52 "{%"
                      TK_WHITESPACE@52..53 " "
                      TK_ENDAPPLY@53..61 "endapply"
                      TK_WHITESPACE@61..62 " "
                      TK_PERCENT_CURLY@62..64 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_apply_filter_with_arguments() {
        check_parse(
            r#"{% apply trim('-', side='left') %}
    This text becomes trimmed
{% endapply %}"#,
            expect![[r#"
                ROOT@0..79
                  TWIG_APPLY@0..79
                    TWIG_APPLY_STARTING_BLOCK@0..34
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_APPLY@3..8 "apply"
                      TWIG_LITERAL_NAME@8..13
                        TK_WHITESPACE@8..9 " "
                        TK_WORD@9..13 "trim"
                      TWIG_ARGUMENTS@13..31
                        TK_OPEN_PARENTHESIS@13..14 "("
                        TWIG_EXPRESSION@14..17
                          TWIG_LITERAL_STRING@14..17
                            TK_SINGLE_QUOTES@14..15 "'"
                            TWIG_LITERAL_STRING_INNER@15..16
                              TK_MINUS@15..16 "-"
                            TK_SINGLE_QUOTES@16..17 "'"
                        TK_COMMA@17..18 ","
                        TWIG_NAMED_ARGUMENT@18..30
                          TK_WHITESPACE@18..19 " "
                          TK_WORD@19..23 "side"
                          TK_EQUAL@23..24 "="
                          TWIG_EXPRESSION@24..30
                            TWIG_LITERAL_STRING@24..30
                              TK_SINGLE_QUOTES@24..25 "'"
                              TWIG_LITERAL_STRING_INNER@25..29
                                TK_WORD@25..29 "left"
                              TK_SINGLE_QUOTES@29..30 "'"
                        TK_CLOSE_PARENTHESIS@30..31 ")"
                      TK_WHITESPACE@31..32 " "
                      TK_PERCENT_CURLY@32..34 "%}"
                    BODY@34..64
                      HTML_TEXT@34..64
                        TK_LINE_BREAK@34..35 "\n"
                        TK_WHITESPACE@35..39 "    "
                        TK_WORD@39..43 "This"
                        TK_WHITESPACE@43..44 " "
                        TK_WORD@44..48 "text"
                        TK_WHITESPACE@48..49 " "
                        TK_WORD@49..56 "becomes"
                        TK_WHITESPACE@56..57 " "
                        TK_WORD@57..64 "trimmed"
                    TWIG_APPLY_ENDING_BLOCK@64..79
                      TK_LINE_BREAK@64..65 "\n"
                      TK_CURLY_PERCENT@65..67 "{%"
                      TK_WHITESPACE@67..68 " "
                      TK_ENDAPPLY@68..76 "endapply"
                      TK_WHITESPACE@76..77 " "
                      TK_PERCENT_CURLY@77..79 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_apply_filter_chained() {
        check_parse(
            r#"{% apply lower|escape('html')|trim('-', side='left') %}
    <strong>SOME TEXT</strong>
{% endapply %}"#,
            expect![[r#"
                ROOT@0..101
                  TWIG_APPLY@0..101
                    TWIG_APPLY_STARTING_BLOCK@0..55
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_APPLY@3..8 "apply"
                      TWIG_FILTER@8..52
                        TWIG_OPERAND@8..29
                          TWIG_FILTER@8..29
                            TWIG_OPERAND@8..14
                              TWIG_LITERAL_NAME@8..14
                                TK_WHITESPACE@8..9 " "
                                TK_WORD@9..14 "lower"
                            TK_SINGLE_PIPE@14..15 "|"
                            TWIG_OPERAND@15..29
                              TWIG_LITERAL_NAME@15..21
                                TK_WORD@15..21 "escape"
                              TWIG_ARGUMENTS@21..29
                                TK_OPEN_PARENTHESIS@21..22 "("
                                TWIG_EXPRESSION@22..28
                                  TWIG_LITERAL_STRING@22..28
                                    TK_SINGLE_QUOTES@22..23 "'"
                                    TWIG_LITERAL_STRING_INNER@23..27
                                      TK_WORD@23..27 "html"
                                    TK_SINGLE_QUOTES@27..28 "'"
                                TK_CLOSE_PARENTHESIS@28..29 ")"
                        TK_SINGLE_PIPE@29..30 "|"
                        TWIG_OPERAND@30..52
                          TWIG_LITERAL_NAME@30..34
                            TK_WORD@30..34 "trim"
                          TWIG_ARGUMENTS@34..52
                            TK_OPEN_PARENTHESIS@34..35 "("
                            TWIG_EXPRESSION@35..38
                              TWIG_LITERAL_STRING@35..38
                                TK_SINGLE_QUOTES@35..36 "'"
                                TWIG_LITERAL_STRING_INNER@36..37
                                  TK_MINUS@36..37 "-"
                                TK_SINGLE_QUOTES@37..38 "'"
                            TK_COMMA@38..39 ","
                            TWIG_NAMED_ARGUMENT@39..51
                              TK_WHITESPACE@39..40 " "
                              TK_WORD@40..44 "side"
                              TK_EQUAL@44..45 "="
                              TWIG_EXPRESSION@45..51
                                TWIG_LITERAL_STRING@45..51
                                  TK_SINGLE_QUOTES@45..46 "'"
                                  TWIG_LITERAL_STRING_INNER@46..50
                                    TK_WORD@46..50 "left"
                                  TK_SINGLE_QUOTES@50..51 "'"
                            TK_CLOSE_PARENTHESIS@51..52 ")"
                      TK_WHITESPACE@52..53 " "
                      TK_PERCENT_CURLY@53..55 "%}"
                    BODY@55..86
                      HTML_TAG@55..86
                        HTML_STARTING_TAG@55..68
                          TK_LINE_BREAK@55..56 "\n"
                          TK_WHITESPACE@56..60 "    "
                          TK_LESS_THAN@60..61 "<"
                          TK_WORD@61..67 "strong"
                          HTML_ATTRIBUTE_LIST@67..67
                          TK_GREATER_THAN@67..68 ">"
                        BODY@68..77
                          HTML_TEXT@68..77
                            TK_WORD@68..72 "SOME"
                            TK_WHITESPACE@72..73 " "
                            TK_WORD@73..77 "TEXT"
                        HTML_ENDING_TAG@77..86
                          TK_LESS_THAN_SLASH@77..79 "</"
                          TK_WORD@79..85 "strong"
                          TK_GREATER_THAN@85..86 ">"
                    TWIG_APPLY_ENDING_BLOCK@86..101
                      TK_LINE_BREAK@86..87 "\n"
                      TK_CURLY_PERCENT@87..89 "{%"
                      TK_WHITESPACE@89..90 " "
                      TK_ENDAPPLY@90..98 "endapply"
                      TK_WHITESPACE@98..99 " "
                      TK_PERCENT_CURLY@99..101 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_apply_missing_filter() {
        check_parse(
            r#"{% apply %}
    SOME TEXT
{% endapply %}"#,
            expect![[r#"
                ROOT@0..40
                  TWIG_APPLY@0..40
                    TWIG_APPLY_STARTING_BLOCK@0..11
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_APPLY@3..8 "apply"
                      TK_WHITESPACE@8..9 " "
                      TK_PERCENT_CURLY@9..11 "%}"
                    BODY@11..25
                      HTML_TEXT@11..25
                        TK_LINE_BREAK@11..12 "\n"
                        TK_WHITESPACE@12..16 "    "
                        TK_WORD@16..20 "SOME"
                        TK_WHITESPACE@20..21 " "
                        TK_WORD@21..25 "TEXT"
                    TWIG_APPLY_ENDING_BLOCK@25..40
                      TK_LINE_BREAK@25..26 "\n"
                      TK_CURLY_PERCENT@26..28 "{%"
                      TK_WHITESPACE@28..29 " "
                      TK_ENDAPPLY@29..37 "endapply"
                      TK_WHITESPACE@37..38 " "
                      TK_PERCENT_CURLY@38..40 "%}"
                error at 9..11: expected twig filter but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_apply_wrong_type() {
        check_parse(
            r#"{% apply 5 %}
    SOME TEXT
{% endapply %}"#,
            expect![[r#"
                ROOT@0..42
                  TWIG_APPLY@0..42
                    TWIG_APPLY_STARTING_BLOCK@0..13
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_APPLY@3..8 "apply"
                      ERROR@8..10
                        TK_WHITESPACE@8..9 " "
                        TK_NUMBER@9..10 "5"
                      TK_WHITESPACE@10..11 " "
                      TK_PERCENT_CURLY@11..13 "%}"
                    BODY@13..27
                      HTML_TEXT@13..27
                        TK_LINE_BREAK@13..14 "\n"
                        TK_WHITESPACE@14..18 "    "
                        TK_WORD@18..22 "SOME"
                        TK_WHITESPACE@22..23 " "
                        TK_WORD@23..27 "TEXT"
                    TWIG_APPLY_ENDING_BLOCK@27..42
                      TK_LINE_BREAK@27..28 "\n"
                      TK_CURLY_PERCENT@28..30 "{%"
                      TK_WHITESPACE@30..31 " "
                      TK_ENDAPPLY@31..39 "endapply"
                      TK_WHITESPACE@39..40 " "
                      TK_PERCENT_CURLY@40..42 "%}"
                error at 9..10: expected twig filter but found number"#]],
        );
    }

    #[test]
    fn parse_twig_autoescape() {
        check_parse(
            r#"{% autoescape %}
    Everything will be automatically escaped in this block
    using the HTML strategy
{% endautoescape %}"#,
            expect![[r#"
                ROOT@0..123
                  TWIG_AUTOESCAPE@0..123
                    TWIG_AUTOESCAPE_STARTING_BLOCK@0..16
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_AUTOESCAPE@3..13 "autoescape"
                      TK_WHITESPACE@13..14 " "
                      TK_PERCENT_CURLY@14..16 "%}"
                    BODY@16..103
                      HTML_TEXT@16..103
                        TK_LINE_BREAK@16..17 "\n"
                        TK_WHITESPACE@17..21 "    "
                        TK_WORD@21..31 "Everything"
                        TK_WHITESPACE@31..32 " "
                        TK_WORD@32..36 "will"
                        TK_WHITESPACE@36..37 " "
                        TK_WORD@37..39 "be"
                        TK_WHITESPACE@39..40 " "
                        TK_WORD@40..53 "automatically"
                        TK_WHITESPACE@53..54 " "
                        TK_WORD@54..61 "escaped"
                        TK_WHITESPACE@61..62 " "
                        TK_IN@62..64 "in"
                        TK_WHITESPACE@64..65 " "
                        TK_WORD@65..69 "this"
                        TK_WHITESPACE@69..70 " "
                        TK_BLOCK@70..75 "block"
                        TK_LINE_BREAK@75..76 "\n"
                        TK_WHITESPACE@76..80 "    "
                        TK_WORD@80..85 "using"
                        TK_WHITESPACE@85..86 " "
                        TK_WORD@86..89 "the"
                        TK_WHITESPACE@89..90 " "
                        TK_WORD@90..94 "HTML"
                        TK_WHITESPACE@94..95 " "
                        TK_WORD@95..103 "strategy"
                    TWIG_AUTOESCAPE_ENDING_BLOCK@103..123
                      TK_LINE_BREAK@103..104 "\n"
                      TK_CURLY_PERCENT@104..106 "{%"
                      TK_WHITESPACE@106..107 " "
                      TK_ENDAUTOESCAPE@107..120 "endautoescape"
                      TK_WHITESPACE@120..121 " "
                      TK_PERCENT_CURLY@121..123 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_autoescape_strategy() {
        check_parse(
            r#"{% autoescape 'js' %}
    Everything will be automatically escaped in this block
    using the js escaping strategy
{% endautoescape %}"#,
            expect![[r#"
                ROOT@0..135
                  TWIG_AUTOESCAPE@0..135
                    TWIG_AUTOESCAPE_STARTING_BLOCK@0..21
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_AUTOESCAPE@3..13 "autoescape"
                      TWIG_LITERAL_STRING@13..18
                        TK_WHITESPACE@13..14 " "
                        TK_SINGLE_QUOTES@14..15 "'"
                        TWIG_LITERAL_STRING_INNER@15..17
                          TK_WORD@15..17 "js"
                        TK_SINGLE_QUOTES@17..18 "'"
                      TK_WHITESPACE@18..19 " "
                      TK_PERCENT_CURLY@19..21 "%}"
                    BODY@21..115
                      HTML_TEXT@21..115
                        TK_LINE_BREAK@21..22 "\n"
                        TK_WHITESPACE@22..26 "    "
                        TK_WORD@26..36 "Everything"
                        TK_WHITESPACE@36..37 " "
                        TK_WORD@37..41 "will"
                        TK_WHITESPACE@41..42 " "
                        TK_WORD@42..44 "be"
                        TK_WHITESPACE@44..45 " "
                        TK_WORD@45..58 "automatically"
                        TK_WHITESPACE@58..59 " "
                        TK_WORD@59..66 "escaped"
                        TK_WHITESPACE@66..67 " "
                        TK_IN@67..69 "in"
                        TK_WHITESPACE@69..70 " "
                        TK_WORD@70..74 "this"
                        TK_WHITESPACE@74..75 " "
                        TK_BLOCK@75..80 "block"
                        TK_LINE_BREAK@80..81 "\n"
                        TK_WHITESPACE@81..85 "    "
                        TK_WORD@85..90 "using"
                        TK_WHITESPACE@90..91 " "
                        TK_WORD@91..94 "the"
                        TK_WHITESPACE@94..95 " "
                        TK_WORD@95..97 "js"
                        TK_WHITESPACE@97..98 " "
                        TK_WORD@98..106 "escaping"
                        TK_WHITESPACE@106..107 " "
                        TK_WORD@107..115 "strategy"
                    TWIG_AUTOESCAPE_ENDING_BLOCK@115..135
                      TK_LINE_BREAK@115..116 "\n"
                      TK_CURLY_PERCENT@116..118 "{%"
                      TK_WHITESPACE@118..119 " "
                      TK_ENDAUTOESCAPE@119..132 "endautoescape"
                      TK_WHITESPACE@132..133 " "
                      TK_PERCENT_CURLY@133..135 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_autoescape_false() {
        check_parse(
            r#"{% autoescape false %}
    Everything will be outputted as is in this block
{% endautoescape %}"#,
            expect![[r#"
                ROOT@0..95
                  TWIG_AUTOESCAPE@0..95
                    TWIG_AUTOESCAPE_STARTING_BLOCK@0..22
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_AUTOESCAPE@3..13 "autoescape"
                      TK_WHITESPACE@13..14 " "
                      TK_FALSE@14..19 "false"
                      TK_WHITESPACE@19..20 " "
                      TK_PERCENT_CURLY@20..22 "%}"
                    BODY@22..75
                      HTML_TEXT@22..75
                        TK_LINE_BREAK@22..23 "\n"
                        TK_WHITESPACE@23..27 "    "
                        TK_WORD@27..37 "Everything"
                        TK_WHITESPACE@37..38 " "
                        TK_WORD@38..42 "will"
                        TK_WHITESPACE@42..43 " "
                        TK_WORD@43..45 "be"
                        TK_WHITESPACE@45..46 " "
                        TK_WORD@46..55 "outputted"
                        TK_WHITESPACE@55..56 " "
                        TK_AS@56..58 "as"
                        TK_WHITESPACE@58..59 " "
                        TK_IS@59..61 "is"
                        TK_WHITESPACE@61..62 " "
                        TK_IN@62..64 "in"
                        TK_WHITESPACE@64..65 " "
                        TK_WORD@65..69 "this"
                        TK_WHITESPACE@69..70 " "
                        TK_BLOCK@70..75 "block"
                    TWIG_AUTOESCAPE_ENDING_BLOCK@75..95
                      TK_LINE_BREAK@75..76 "\n"
                      TK_CURLY_PERCENT@76..78 "{%"
                      TK_WHITESPACE@78..79 " "
                      TK_ENDAUTOESCAPE@79..92 "endautoescape"
                      TK_WHITESPACE@92..93 " "
                      TK_PERCENT_CURLY@93..95 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_autoescape_wrong_var() {
        check_parse(
            r#"{% autoescape my_var %}
    Everything will be automatically escaped in this block
    using the js escaping strategy
{% endautoescape %}"#,
            expect![[r#"
                ROOT@0..137
                  TWIG_AUTOESCAPE@0..137
                    TWIG_AUTOESCAPE_STARTING_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_AUTOESCAPE@3..13 "autoescape"
                      ERROR@13..20
                        TK_WHITESPACE@13..14 " "
                        TK_WORD@14..20 "my_var"
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..117
                      HTML_TEXT@23..117
                        TK_LINE_BREAK@23..24 "\n"
                        TK_WHITESPACE@24..28 "    "
                        TK_WORD@28..38 "Everything"
                        TK_WHITESPACE@38..39 " "
                        TK_WORD@39..43 "will"
                        TK_WHITESPACE@43..44 " "
                        TK_WORD@44..46 "be"
                        TK_WHITESPACE@46..47 " "
                        TK_WORD@47..60 "automatically"
                        TK_WHITESPACE@60..61 " "
                        TK_WORD@61..68 "escaped"
                        TK_WHITESPACE@68..69 " "
                        TK_IN@69..71 "in"
                        TK_WHITESPACE@71..72 " "
                        TK_WORD@72..76 "this"
                        TK_WHITESPACE@76..77 " "
                        TK_BLOCK@77..82 "block"
                        TK_LINE_BREAK@82..83 "\n"
                        TK_WHITESPACE@83..87 "    "
                        TK_WORD@87..92 "using"
                        TK_WHITESPACE@92..93 " "
                        TK_WORD@93..96 "the"
                        TK_WHITESPACE@96..97 " "
                        TK_WORD@97..99 "js"
                        TK_WHITESPACE@99..100 " "
                        TK_WORD@100..108 "escaping"
                        TK_WHITESPACE@108..109 " "
                        TK_WORD@109..117 "strategy"
                    TWIG_AUTOESCAPE_ENDING_BLOCK@117..137
                      TK_LINE_BREAK@117..118 "\n"
                      TK_CURLY_PERCENT@118..120 "{%"
                      TK_WHITESPACE@120..121 " "
                      TK_ENDAUTOESCAPE@121..134 "endautoescape"
                      TK_WHITESPACE@134..135 " "
                      TK_PERCENT_CURLY@135..137 "%}"
                error at 14..20: expected twig escape strategy as string or 'false' but found word"#]],
        );
    }

    #[test]
    fn parse_twig_deprecated() {
        check_parse(
            r#"{% deprecated 'The "base.twig" template is deprecated, use "layout.twig" instead.' %}"#,
            expect![[r#"
                ROOT@0..85
                  TWIG_DEPRECATED@0..85
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_DEPRECATED@3..13 "deprecated"
                    TWIG_LITERAL_STRING@13..82
                      TK_WHITESPACE@13..14 " "
                      TK_SINGLE_QUOTES@14..15 "'"
                      TWIG_LITERAL_STRING_INNER@15..81
                        TK_WORD@15..18 "The"
                        TK_WHITESPACE@18..19 " "
                        TK_DOUBLE_QUOTES@19..20 "\""
                        TK_WORD@20..24 "base"
                        TK_DOT@24..25 "."
                        TK_WORD@25..29 "twig"
                        TK_DOUBLE_QUOTES@29..30 "\""
                        TK_WHITESPACE@30..31 " "
                        TK_WORD@31..39 "template"
                        TK_WHITESPACE@39..40 " "
                        TK_IS@40..42 "is"
                        TK_WHITESPACE@42..43 " "
                        TK_DEPRECATED@43..53 "deprecated"
                        TK_COMMA@53..54 ","
                        TK_WHITESPACE@54..55 " "
                        TK_USE@55..58 "use"
                        TK_WHITESPACE@58..59 " "
                        TK_DOUBLE_QUOTES@59..60 "\""
                        TK_WORD@60..66 "layout"
                        TK_DOT@66..67 "."
                        TK_WORD@67..71 "twig"
                        TK_DOUBLE_QUOTES@71..72 "\""
                        TK_WHITESPACE@72..73 " "
                        TK_WORD@73..80 "instead"
                        TK_DOT@80..81 "."
                      TK_SINGLE_QUOTES@81..82 "'"
                    TK_WHITESPACE@82..83 " "
                    TK_PERCENT_CURLY@83..85 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_deprecated_missing_string() {
        check_parse(
            r#"{% deprecated %}"#,
            expect![[r#"
                ROOT@0..16
                  TWIG_DEPRECATED@0..16
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_DEPRECATED@3..13 "deprecated"
                    TK_WHITESPACE@13..14 " "
                    TK_PERCENT_CURLY@14..16 "%}"
                error at 14..16: expected twig deprecation message as string but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_do() {
        check_parse(
            r#"{% do 1 + 2 %}"#,
            expect![[r#"
            ROOT@0..14
              TWIG_DO@0..14
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_DO@3..5 "do"
                TWIG_EXPRESSION@5..11
                  TWIG_BINARY_EXPRESSION@5..11
                    TWIG_EXPRESSION@5..7
                      TWIG_LITERAL_NUMBER@5..7
                        TK_WHITESPACE@5..6 " "
                        TK_NUMBER@6..7 "1"
                    TK_WHITESPACE@7..8 " "
                    TK_PLUS@8..9 "+"
                    TWIG_EXPRESSION@9..11
                      TWIG_LITERAL_NUMBER@9..11
                        TK_WHITESPACE@9..10 " "
                        TK_NUMBER@10..11 "2"
                TK_WHITESPACE@11..12 " "
                TK_PERCENT_CURLY@12..14 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_do_missing_expression() {
        check_parse(
            r#"{% do %}"#,
            expect![[r#"
            ROOT@0..8
              TWIG_DO@0..8
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_DO@3..5 "do"
                TK_WHITESPACE@5..6 " "
                TK_PERCENT_CURLY@6..8 "%}"
            error at 6..8: expected twig expression but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_embed_template_with_value() {
        check_parse(
            r#"{% embed "base" with {'foo': 'bar'} %}
    ...
{% endembed %}"#,
            expect![[r#"
                ROOT@0..61
                  TWIG_EMBED@0..61
                    TWIG_EMBED_STARTING_BLOCK@0..38
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_EMBED@3..8 "embed"
                      TWIG_EXPRESSION@8..15
                        TWIG_LITERAL_STRING@8..15
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..14
                            TK_WORD@10..14 "base"
                          TK_DOUBLE_QUOTES@14..15 "\""
                      TWIG_INCLUDE_WITH@15..35
                        TK_WHITESPACE@15..16 " "
                        TK_WITH@16..20 "with"
                        TWIG_EXPRESSION@20..35
                          TWIG_LITERAL_HASH@20..35
                            TK_WHITESPACE@20..21 " "
                            TK_OPEN_CURLY@21..22 "{"
                            TWIG_LITERAL_HASH_ITEMS@22..34
                              TWIG_LITERAL_HASH_PAIR@22..34
                                TWIG_LITERAL_HASH_KEY@22..27
                                  TWIG_LITERAL_STRING@22..27
                                    TK_SINGLE_QUOTES@22..23 "'"
                                    TWIG_LITERAL_STRING_INNER@23..26
                                      TK_WORD@23..26 "foo"
                                    TK_SINGLE_QUOTES@26..27 "'"
                                TK_COLON@27..28 ":"
                                TWIG_EXPRESSION@28..34
                                  TWIG_LITERAL_STRING@28..34
                                    TK_WHITESPACE@28..29 " "
                                    TK_SINGLE_QUOTES@29..30 "'"
                                    TWIG_LITERAL_STRING_INNER@30..33
                                      TK_WORD@30..33 "bar"
                                    TK_SINGLE_QUOTES@33..34 "'"
                            TK_CLOSE_CURLY@34..35 "}"
                      TK_WHITESPACE@35..36 " "
                      TK_PERCENT_CURLY@36..38 "%}"
                    BODY@38..46
                      HTML_TEXT@38..46
                        TK_LINE_BREAK@38..39 "\n"
                        TK_WHITESPACE@39..43 "    "
                        TK_DOUBLE_DOT@43..45 ".."
                        TK_DOT@45..46 "."
                    TWIG_EMBED_ENDING_BLOCK@46..61
                      TK_LINE_BREAK@46..47 "\n"
                      TK_CURLY_PERCENT@47..49 "{%"
                      TK_WHITESPACE@49..50 " "
                      TK_ENDEMBED@50..58 "endembed"
                      TK_WHITESPACE@58..59 " "
                      TK_PERCENT_CURLY@59..61 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_embed_template_with_value_only() {
        check_parse(
            r#"{% embed "base" with {'foo': 'bar'} only %}
    ...
{% endembed %}"#,
            expect![[r#"
                ROOT@0..66
                  TWIG_EMBED@0..66
                    TWIG_EMBED_STARTING_BLOCK@0..43
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_EMBED@3..8 "embed"
                      TWIG_EXPRESSION@8..15
                        TWIG_LITERAL_STRING@8..15
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..14
                            TK_WORD@10..14 "base"
                          TK_DOUBLE_QUOTES@14..15 "\""
                      TWIG_INCLUDE_WITH@15..35
                        TK_WHITESPACE@15..16 " "
                        TK_WITH@16..20 "with"
                        TWIG_EXPRESSION@20..35
                          TWIG_LITERAL_HASH@20..35
                            TK_WHITESPACE@20..21 " "
                            TK_OPEN_CURLY@21..22 "{"
                            TWIG_LITERAL_HASH_ITEMS@22..34
                              TWIG_LITERAL_HASH_PAIR@22..34
                                TWIG_LITERAL_HASH_KEY@22..27
                                  TWIG_LITERAL_STRING@22..27
                                    TK_SINGLE_QUOTES@22..23 "'"
                                    TWIG_LITERAL_STRING_INNER@23..26
                                      TK_WORD@23..26 "foo"
                                    TK_SINGLE_QUOTES@26..27 "'"
                                TK_COLON@27..28 ":"
                                TWIG_EXPRESSION@28..34
                                  TWIG_LITERAL_STRING@28..34
                                    TK_WHITESPACE@28..29 " "
                                    TK_SINGLE_QUOTES@29..30 "'"
                                    TWIG_LITERAL_STRING_INNER@30..33
                                      TK_WORD@30..33 "bar"
                                    TK_SINGLE_QUOTES@33..34 "'"
                            TK_CLOSE_CURLY@34..35 "}"
                      TK_WHITESPACE@35..36 " "
                      TK_ONLY@36..40 "only"
                      TK_WHITESPACE@40..41 " "
                      TK_PERCENT_CURLY@41..43 "%}"
                    BODY@43..51
                      HTML_TEXT@43..51
                        TK_LINE_BREAK@43..44 "\n"
                        TK_WHITESPACE@44..48 "    "
                        TK_DOUBLE_DOT@48..50 ".."
                        TK_DOT@50..51 "."
                    TWIG_EMBED_ENDING_BLOCK@51..66
                      TK_LINE_BREAK@51..52 "\n"
                      TK_CURLY_PERCENT@52..54 "{%"
                      TK_WHITESPACE@54..55 " "
                      TK_ENDEMBED@55..63 "endembed"
                      TK_WHITESPACE@63..64 " "
                      TK_PERCENT_CURLY@64..66 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_embed_template_ignore_missing() {
        check_parse(
            r#"{% embed "base" ignore missing %}
    ...
{% endembed %}"#,
            expect![[r#"
                ROOT@0..56
                  TWIG_EMBED@0..56
                    TWIG_EMBED_STARTING_BLOCK@0..33
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_EMBED@3..8 "embed"
                      TWIG_EXPRESSION@8..15
                        TWIG_LITERAL_STRING@8..15
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..14
                            TK_WORD@10..14 "base"
                          TK_DOUBLE_QUOTES@14..15 "\""
                      TK_WHITESPACE@15..16 " "
                      TK_IGNORE_MISSING@16..30 "ignore missing"
                      TK_WHITESPACE@30..31 " "
                      TK_PERCENT_CURLY@31..33 "%}"
                    BODY@33..41
                      HTML_TEXT@33..41
                        TK_LINE_BREAK@33..34 "\n"
                        TK_WHITESPACE@34..38 "    "
                        TK_DOUBLE_DOT@38..40 ".."
                        TK_DOT@40..41 "."
                    TWIG_EMBED_ENDING_BLOCK@41..56
                      TK_LINE_BREAK@41..42 "\n"
                      TK_CURLY_PERCENT@42..44 "{%"
                      TK_WHITESPACE@44..45 " "
                      TK_ENDEMBED@45..53 "endembed"
                      TK_WHITESPACE@53..54 " "
                      TK_PERCENT_CURLY@54..56 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_flush() {
        check_parse(
            r#"{% flush %}"#,
            expect![[r#"
            ROOT@0..11
              TWIG_FLUSH@0..11
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_FLUSH@3..8 "flush"
                TK_WHITESPACE@8..9 " "
                TK_PERCENT_CURLY@9..11 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_from_template_import() {
        check_parse(
            r#"{% from 'forms.html' import input as input_field, textarea %}"#,
            expect![[r#"
                ROOT@0..61
                  TWIG_FROM@0..61
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_FROM@3..7 "from"
                    TWIG_EXPRESSION@7..20
                      TWIG_LITERAL_STRING@7..20
                        TK_WHITESPACE@7..8 " "
                        TK_SINGLE_QUOTES@8..9 "'"
                        TWIG_LITERAL_STRING_INNER@9..19
                          TK_WORD@9..14 "forms"
                          TK_DOT@14..15 "."
                          TK_WORD@15..19 "html"
                        TK_SINGLE_QUOTES@19..20 "'"
                    TK_WHITESPACE@20..21 " "
                    TK_IMPORT@21..27 "import"
                    TWIG_OVERRIDE@27..48
                      TWIG_LITERAL_NAME@27..33
                        TK_WHITESPACE@27..28 " "
                        TK_WORD@28..33 "input"
                      TK_WHITESPACE@33..34 " "
                      TK_AS@34..36 "as"
                      TWIG_LITERAL_NAME@36..48
                        TK_WHITESPACE@36..37 " "
                        TK_WORD@37..48 "input_field"
                    TK_COMMA@48..49 ","
                    TWIG_OVERRIDE@49..58
                      TWIG_LITERAL_NAME@49..58
                        TK_WHITESPACE@49..50 " "
                        TK_WORD@50..58 "textarea"
                    TK_WHITESPACE@58..59 " "
                    TK_PERCENT_CURLY@59..61 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_from_expression_import() {
        check_parse(
            r#"{% from my_var|trim import input as input_field, textarea %}"#,
            expect![[r#"
                ROOT@0..60
                  TWIG_FROM@0..60
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_FROM@3..7 "from"
                    TWIG_EXPRESSION@7..19
                      TWIG_FILTER@7..19
                        TWIG_OPERAND@7..14
                          TWIG_LITERAL_NAME@7..14
                            TK_WHITESPACE@7..8 " "
                            TK_WORD@8..14 "my_var"
                        TK_SINGLE_PIPE@14..15 "|"
                        TWIG_OPERAND@15..19
                          TWIG_LITERAL_NAME@15..19
                            TK_WORD@15..19 "trim"
                    TK_WHITESPACE@19..20 " "
                    TK_IMPORT@20..26 "import"
                    TWIG_OVERRIDE@26..47
                      TWIG_LITERAL_NAME@26..32
                        TK_WHITESPACE@26..27 " "
                        TK_WORD@27..32 "input"
                      TK_WHITESPACE@32..33 " "
                      TK_AS@33..35 "as"
                      TWIG_LITERAL_NAME@35..47
                        TK_WHITESPACE@35..36 " "
                        TK_WORD@36..47 "input_field"
                    TK_COMMA@47..48 ","
                    TWIG_OVERRIDE@48..57
                      TWIG_LITERAL_NAME@48..57
                        TK_WHITESPACE@48..49 " "
                        TK_WORD@49..57 "textarea"
                    TK_WHITESPACE@57..58 " "
                    TK_PERCENT_CURLY@58..60 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_from_missing_macros() {
        check_parse(
            r#"{% from 'forms.html' import %}"#,
            expect![[r#"
            ROOT@0..30
              TWIG_FROM@0..30
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_FROM@3..7 "from"
                TWIG_EXPRESSION@7..20
                  TWIG_LITERAL_STRING@7..20
                    TK_WHITESPACE@7..8 " "
                    TK_SINGLE_QUOTES@8..9 "'"
                    TWIG_LITERAL_STRING_INNER@9..19
                      TK_WORD@9..14 "forms"
                      TK_DOT@14..15 "."
                      TK_WORD@15..19 "html"
                    TK_SINGLE_QUOTES@19..20 "'"
                TK_WHITESPACE@20..21 " "
                TK_IMPORT@21..27 "import"
                TK_WHITESPACE@27..28 " "
                TK_PERCENT_CURLY@28..30 "%}"
            error at 28..30: expected at least one macro name as macro name but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_from_missing_import_and_macros() {
        check_parse(
            r#"{% from 'forms.html' %}"#,
            expect![[r#"
            ROOT@0..23
              TWIG_FROM@0..23
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_FROM@3..7 "from"
                TWIG_EXPRESSION@7..20
                  TWIG_LITERAL_STRING@7..20
                    TK_WHITESPACE@7..8 " "
                    TK_SINGLE_QUOTES@8..9 "'"
                    TWIG_LITERAL_STRING_INNER@9..19
                      TK_WORD@9..14 "forms"
                      TK_DOT@14..15 "."
                      TK_WORD@15..19 "html"
                    TK_SINGLE_QUOTES@19..20 "'"
                TK_WHITESPACE@20..21 " "
                TK_PERCENT_CURLY@21..23 "%}"
            error at 21..23: expected import but found %}
            error at 21..23: expected at least one macro name as macro name but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_import_template_as_macro() {
        check_parse(
            r#"{% import "forms.html" as forms %}"#,
            expect![[r#"
            ROOT@0..34
              TWIG_IMPORT@0..34
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_IMPORT@3..9 "import"
                TWIG_EXPRESSION@9..22
                  TWIG_LITERAL_STRING@9..22
                    TK_WHITESPACE@9..10 " "
                    TK_DOUBLE_QUOTES@10..11 "\""
                    TWIG_LITERAL_STRING_INNER@11..21
                      TK_WORD@11..16 "forms"
                      TK_DOT@16..17 "."
                      TK_WORD@17..21 "html"
                    TK_DOUBLE_QUOTES@21..22 "\""
                TK_WHITESPACE@22..23 " "
                TK_AS@23..25 "as"
                TWIG_LITERAL_NAME@25..31
                  TK_WHITESPACE@25..26 " "
                  TK_WORD@26..31 "forms"
                TK_WHITESPACE@31..32 " "
                TK_PERCENT_CURLY@32..34 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_import_expression_as_macro() {
        check_parse(
            r#"{% import my_var|trim as forms %}"#,
            expect![[r#"
                ROOT@0..33
                  TWIG_IMPORT@0..33
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_IMPORT@3..9 "import"
                    TWIG_EXPRESSION@9..21
                      TWIG_FILTER@9..21
                        TWIG_OPERAND@9..16
                          TWIG_LITERAL_NAME@9..16
                            TK_WHITESPACE@9..10 " "
                            TK_WORD@10..16 "my_var"
                        TK_SINGLE_PIPE@16..17 "|"
                        TWIG_OPERAND@17..21
                          TWIG_LITERAL_NAME@17..21
                            TK_WORD@17..21 "trim"
                    TK_WHITESPACE@21..22 " "
                    TK_AS@22..24 "as"
                    TWIG_LITERAL_NAME@24..30
                      TK_WHITESPACE@24..25 " "
                      TK_WORD@25..30 "forms"
                    TK_WHITESPACE@30..31 " "
                    TK_PERCENT_CURLY@31..33 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_import_template_as_missing_macro() {
        check_parse(
            r#"{% import "forms.html" as %}"#,
            expect![[r#"
            ROOT@0..28
              TWIG_IMPORT@0..28
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_IMPORT@3..9 "import"
                TWIG_EXPRESSION@9..22
                  TWIG_LITERAL_STRING@9..22
                    TK_WHITESPACE@9..10 " "
                    TK_DOUBLE_QUOTES@10..11 "\""
                    TWIG_LITERAL_STRING_INNER@11..21
                      TK_WORD@11..16 "forms"
                      TK_DOT@16..17 "."
                      TK_WORD@17..21 "html"
                    TK_DOUBLE_QUOTES@21..22 "\""
                TK_WHITESPACE@22..23 " "
                TK_AS@23..25 "as"
                TK_WHITESPACE@25..26 " "
                TK_PERCENT_CURLY@26..28 "%}"
            error at 26..28: expected name for twig macro but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_import_template_missing_as() {
        check_parse(
            r#"{% import "forms.html" %}"#,
            expect![[r#"
            ROOT@0..25
              TWIG_IMPORT@0..25
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_IMPORT@3..9 "import"
                TWIG_EXPRESSION@9..22
                  TWIG_LITERAL_STRING@9..22
                    TK_WHITESPACE@9..10 " "
                    TK_DOUBLE_QUOTES@10..11 "\""
                    TWIG_LITERAL_STRING_INNER@11..21
                      TK_WORD@11..16 "forms"
                      TK_DOT@16..17 "."
                      TK_WORD@17..21 "html"
                    TK_DOUBLE_QUOTES@21..22 "\""
                TK_WHITESPACE@22..23 " "
                TK_PERCENT_CURLY@23..25 "%}"
            error at 23..25: expected as but found %}
            error at 23..25: expected name for twig macro but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_sandbox() {
        check_parse(
            r#"{% sandbox %}
    {% include 'user.html' %}
{% endsandbox %}"#,
            expect![[r#"
                ROOT@0..60
                  TWIG_SANDBOX@0..60
                    TWIG_SANDBOX_STARTING_BLOCK@0..13
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SANDBOX@3..10 "sandbox"
                      TK_WHITESPACE@10..11 " "
                      TK_PERCENT_CURLY@11..13 "%}"
                    BODY@13..43
                      TWIG_INCLUDE@13..43
                        TK_LINE_BREAK@13..14 "\n"
                        TK_WHITESPACE@14..18 "    "
                        TK_CURLY_PERCENT@18..20 "{%"
                        TK_WHITESPACE@20..21 " "
                        TK_INCLUDE@21..28 "include"
                        TWIG_EXPRESSION@28..40
                          TWIG_LITERAL_STRING@28..40
                            TK_WHITESPACE@28..29 " "
                            TK_SINGLE_QUOTES@29..30 "'"
                            TWIG_LITERAL_STRING_INNER@30..39
                              TK_WORD@30..34 "user"
                              TK_DOT@34..35 "."
                              TK_WORD@35..39 "html"
                            TK_SINGLE_QUOTES@39..40 "'"
                        TK_WHITESPACE@40..41 " "
                        TK_PERCENT_CURLY@41..43 "%}"
                    TWIG_SANDBOX_ENDING_BLOCK@43..60
                      TK_LINE_BREAK@43..44 "\n"
                      TK_CURLY_PERCENT@44..46 "{%"
                      TK_WHITESPACE@46..47 " "
                      TK_ENDSANDBOX@47..57 "endsandbox"
                      TK_WHITESPACE@57..58 " "
                      TK_PERCENT_CURLY@58..60 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_verbatim() {
        check_parse(
            r#"{% verbatim %}
    <ul>
    {% for item in seq %}
        <li>{{ item }}</li>
    {% endfor %}
    </ul>
{% endverbatim %}"#,
            expect![[r#"
                ROOT@0..122
                  TWIG_VERBATIM@0..122
                    TWIG_VERBATIM_STARTING_BLOCK@0..14
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_VERBATIM@3..11 "verbatim"
                      TK_WHITESPACE@11..12 " "
                      TK_PERCENT_CURLY@12..14 "%}"
                    BODY@14..104
                      HTML_TAG@14..104
                        HTML_STARTING_TAG@14..23
                          TK_LINE_BREAK@14..15 "\n"
                          TK_WHITESPACE@15..19 "    "
                          TK_LESS_THAN@19..20 "<"
                          TK_WORD@20..22 "ul"
                          HTML_ATTRIBUTE_LIST@22..22
                          TK_GREATER_THAN@22..23 ">"
                        BODY@23..94
                          TWIG_FOR@23..94
                            TWIG_FOR_BLOCK@23..49
                              TK_LINE_BREAK@23..24 "\n"
                              TK_WHITESPACE@24..28 "    "
                              TK_CURLY_PERCENT@28..30 "{%"
                              TK_WHITESPACE@30..31 " "
                              TK_FOR@31..34 "for"
                              TWIG_LITERAL_NAME@34..39
                                TK_WHITESPACE@34..35 " "
                                TK_WORD@35..39 "item"
                              TK_WHITESPACE@39..40 " "
                              TK_IN@40..42 "in"
                              TWIG_EXPRESSION@42..46
                                TWIG_LITERAL_NAME@42..46
                                  TK_WHITESPACE@42..43 " "
                                  TK_WORD@43..46 "seq"
                              TK_WHITESPACE@46..47 " "
                              TK_PERCENT_CURLY@47..49 "%}"
                            BODY@49..77
                              HTML_TAG@49..77
                                HTML_STARTING_TAG@49..62
                                  TK_LINE_BREAK@49..50 "\n"
                                  TK_WHITESPACE@50..58 "        "
                                  TK_LESS_THAN@58..59 "<"
                                  TK_WORD@59..61 "li"
                                  HTML_ATTRIBUTE_LIST@61..61
                                  TK_GREATER_THAN@61..62 ">"
                                BODY@62..72
                                  TWIG_VAR@62..72
                                    TK_OPEN_CURLY_CURLY@62..64 "{{"
                                    TWIG_EXPRESSION@64..69
                                      TWIG_LITERAL_NAME@64..69
                                        TK_WHITESPACE@64..65 " "
                                        TK_WORD@65..69 "item"
                                    TK_WHITESPACE@69..70 " "
                                    TK_CLOSE_CURLY_CURLY@70..72 "}}"
                                HTML_ENDING_TAG@72..77
                                  TK_LESS_THAN_SLASH@72..74 "</"
                                  TK_WORD@74..76 "li"
                                  TK_GREATER_THAN@76..77 ">"
                            TWIG_ENDFOR_BLOCK@77..94
                              TK_LINE_BREAK@77..78 "\n"
                              TK_WHITESPACE@78..82 "    "
                              TK_CURLY_PERCENT@82..84 "{%"
                              TK_WHITESPACE@84..85 " "
                              TK_ENDFOR@85..91 "endfor"
                              TK_WHITESPACE@91..92 " "
                              TK_PERCENT_CURLY@92..94 "%}"
                        HTML_ENDING_TAG@94..104
                          TK_LINE_BREAK@94..95 "\n"
                          TK_WHITESPACE@95..99 "    "
                          TK_LESS_THAN_SLASH@99..101 "</"
                          TK_WORD@101..103 "ul"
                          TK_GREATER_THAN@103..104 ">"
                    TWIG_VERBATIM_ENDING_BLOCK@104..122
                      TK_LINE_BREAK@104..105 "\n"
                      TK_CURLY_PERCENT@105..107 "{%"
                      TK_WHITESPACE@107..108 " "
                      TK_ENDVERBATIM@108..119 "endverbatim"
                      TK_WHITESPACE@119..120 " "
                      TK_PERCENT_CURLY@120..122 "%}""#]],
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn parse_twig_macro() {
        check_parse(
            r#"{% macro input(name, value, type = "text", size = 20) %}
    <input type="{{ type }}" name="{{ name }}" value="{{ value|e }}" size="{{ size }}"/>
{% endmacro %}"#,
            expect![[r#"
                ROOT@0..160
                  TWIG_MACRO@0..160
                    TWIG_MACRO_STARTING_BLOCK@0..56
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_MACRO@3..8 "macro"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "input"
                      TWIG_ARGUMENTS@14..53
                        TK_OPEN_PARENTHESIS@14..15 "("
                        TWIG_EXPRESSION@15..19
                          TWIG_LITERAL_NAME@15..19
                            TK_WORD@15..19 "name"
                        TK_COMMA@19..20 ","
                        TWIG_EXPRESSION@20..26
                          TWIG_LITERAL_NAME@20..26
                            TK_WHITESPACE@20..21 " "
                            TK_WORD@21..26 "value"
                        TK_COMMA@26..27 ","
                        TWIG_NAMED_ARGUMENT@27..41
                          TK_WHITESPACE@27..28 " "
                          TK_WORD@28..32 "type"
                          TK_WHITESPACE@32..33 " "
                          TK_EQUAL@33..34 "="
                          TWIG_EXPRESSION@34..41
                            TWIG_LITERAL_STRING@34..41
                              TK_WHITESPACE@34..35 " "
                              TK_DOUBLE_QUOTES@35..36 "\""
                              TWIG_LITERAL_STRING_INNER@36..40
                                TK_WORD@36..40 "text"
                              TK_DOUBLE_QUOTES@40..41 "\""
                        TK_COMMA@41..42 ","
                        TWIG_NAMED_ARGUMENT@42..52
                          TK_WHITESPACE@42..43 " "
                          TK_WORD@43..47 "size"
                          TK_WHITESPACE@47..48 " "
                          TK_EQUAL@48..49 "="
                          TWIG_EXPRESSION@49..52
                            TWIG_LITERAL_NUMBER@49..52
                              TK_WHITESPACE@49..50 " "
                              TK_NUMBER@50..52 "20"
                        TK_CLOSE_PARENTHESIS@52..53 ")"
                      TK_WHITESPACE@53..54 " "
                      TK_PERCENT_CURLY@54..56 "%}"
                    BODY@56..145
                      HTML_TAG@56..145
                        HTML_STARTING_TAG@56..145
                          TK_LINE_BREAK@56..57 "\n"
                          TK_WHITESPACE@57..61 "    "
                          TK_LESS_THAN@61..62 "<"
                          TK_WORD@62..67 "input"
                          HTML_ATTRIBUTE_LIST@67..143
                            HTML_ATTRIBUTE@67..85
                              TK_WHITESPACE@67..68 " "
                              TK_WORD@68..72 "type"
                              TK_EQUAL@72..73 "="
                              HTML_STRING@73..85
                                TK_DOUBLE_QUOTES@73..74 "\""
                                HTML_STRING_INNER@74..84
                                  TWIG_VAR@74..84
                                    TK_OPEN_CURLY_CURLY@74..76 "{{"
                                    TWIG_EXPRESSION@76..81
                                      TWIG_LITERAL_NAME@76..81
                                        TK_WHITESPACE@76..77 " "
                                        TK_WORD@77..81 "type"
                                    TK_WHITESPACE@81..82 " "
                                    TK_CLOSE_CURLY_CURLY@82..84 "}}"
                                TK_DOUBLE_QUOTES@84..85 "\""
                            HTML_ATTRIBUTE@85..103
                              TK_WHITESPACE@85..86 " "
                              TK_WORD@86..90 "name"
                              TK_EQUAL@90..91 "="
                              HTML_STRING@91..103
                                TK_DOUBLE_QUOTES@91..92 "\""
                                HTML_STRING_INNER@92..102
                                  TWIG_VAR@92..102
                                    TK_OPEN_CURLY_CURLY@92..94 "{{"
                                    TWIG_EXPRESSION@94..99
                                      TWIG_LITERAL_NAME@94..99
                                        TK_WHITESPACE@94..95 " "
                                        TK_WORD@95..99 "name"
                                    TK_WHITESPACE@99..100 " "
                                    TK_CLOSE_CURLY_CURLY@100..102 "}}"
                                TK_DOUBLE_QUOTES@102..103 "\""
                            HTML_ATTRIBUTE@103..125
                              TK_WHITESPACE@103..104 " "
                              TK_WORD@104..109 "value"
                              TK_EQUAL@109..110 "="
                              HTML_STRING@110..125
                                TK_DOUBLE_QUOTES@110..111 "\""
                                HTML_STRING_INNER@111..124
                                  TWIG_VAR@111..124
                                    TK_OPEN_CURLY_CURLY@111..113 "{{"
                                    TWIG_EXPRESSION@113..121
                                      TWIG_FILTER@113..121
                                        TWIG_OPERAND@113..119
                                          TWIG_LITERAL_NAME@113..119
                                            TK_WHITESPACE@113..114 " "
                                            TK_WORD@114..119 "value"
                                        TK_SINGLE_PIPE@119..120 "|"
                                        TWIG_OPERAND@120..121
                                          TWIG_LITERAL_NAME@120..121
                                            TK_WORD@120..121 "e"
                                    TK_WHITESPACE@121..122 " "
                                    TK_CLOSE_CURLY_CURLY@122..124 "}}"
                                TK_DOUBLE_QUOTES@124..125 "\""
                            HTML_ATTRIBUTE@125..143
                              TK_WHITESPACE@125..126 " "
                              TK_WORD@126..130 "size"
                              TK_EQUAL@130..131 "="
                              HTML_STRING@131..143
                                TK_DOUBLE_QUOTES@131..132 "\""
                                HTML_STRING_INNER@132..142
                                  TWIG_VAR@132..142
                                    TK_OPEN_CURLY_CURLY@132..134 "{{"
                                    TWIG_EXPRESSION@134..139
                                      TWIG_LITERAL_NAME@134..139
                                        TK_WHITESPACE@134..135 " "
                                        TK_WORD@135..139 "size"
                                    TK_WHITESPACE@139..140 " "
                                    TK_CLOSE_CURLY_CURLY@140..142 "}}"
                                TK_DOUBLE_QUOTES@142..143 "\""
                          TK_SLASH_GREATER_THAN@143..145 "/>"
                    TWIG_MACRO_ENDING_BLOCK@145..160
                      TK_LINE_BREAK@145..146 "\n"
                      TK_CURLY_PERCENT@146..148 "{%"
                      TK_WHITESPACE@148..149 " "
                      TK_ENDMACRO@149..157 "endmacro"
                      TK_WHITESPACE@157..158 " "
                      TK_PERCENT_CURLY@158..160 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_macro_with_matching_end_tag() {
        check_parse(
            r#"{% macro input() %}
    ...
{% endmacro input %}"#,
            expect![[r#"
                ROOT@0..48
                  TWIG_MACRO@0..48
                    TWIG_MACRO_STARTING_BLOCK@0..19
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_MACRO@3..8 "macro"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "input"
                      TWIG_ARGUMENTS@14..16
                        TK_OPEN_PARENTHESIS@14..15 "("
                        TK_CLOSE_PARENTHESIS@15..16 ")"
                      TK_WHITESPACE@16..17 " "
                      TK_PERCENT_CURLY@17..19 "%}"
                    BODY@19..27
                      HTML_TEXT@19..27
                        TK_LINE_BREAK@19..20 "\n"
                        TK_WHITESPACE@20..24 "    "
                        TK_DOUBLE_DOT@24..26 ".."
                        TK_DOT@26..27 "."
                    TWIG_MACRO_ENDING_BLOCK@27..48
                      TK_LINE_BREAK@27..28 "\n"
                      TK_CURLY_PERCENT@28..30 "{%"
                      TK_WHITESPACE@30..31 " "
                      TK_ENDMACRO@31..39 "endmacro"
                      TK_WHITESPACE@39..40 " "
                      TK_WORD@40..45 "input"
                      TK_WHITESPACE@45..46 " "
                      TK_PERCENT_CURLY@46..48 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_macro_with_non_matching_end_tag() {
        check_parse(
            r#"{% macro input() %}
    ...
{% endmacro inp %}"#,
            expect![[r#"
                ROOT@0..46
                  TWIG_MACRO@0..46
                    TWIG_MACRO_STARTING_BLOCK@0..19
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_MACRO@3..8 "macro"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "input"
                      TWIG_ARGUMENTS@14..16
                        TK_OPEN_PARENTHESIS@14..15 "("
                        TK_CLOSE_PARENTHESIS@15..16 ")"
                      TK_WHITESPACE@16..17 " "
                      TK_PERCENT_CURLY@17..19 "%}"
                    BODY@19..27
                      HTML_TEXT@19..27
                        TK_LINE_BREAK@19..20 "\n"
                        TK_WHITESPACE@20..24 "    "
                        TK_DOUBLE_DOT@24..26 ".."
                        TK_DOT@26..27 "."
                    TWIG_MACRO_ENDING_BLOCK@27..46
                      TK_LINE_BREAK@27..28 "\n"
                      TK_CURLY_PERCENT@28..30 "{%"
                      TK_WHITESPACE@30..31 " "
                      TK_ENDMACRO@31..39 "endmacro"
                      TK_WHITESPACE@39..40 " "
                      TK_WORD@40..43 "inp"
                      TK_WHITESPACE@43..44 " "
                      TK_PERCENT_CURLY@44..46 "%}"
                error at 40..43: expected nothing or same twig macro name as opening (input) but found word"#]],
        );
    }

    #[test]
    fn parse_twig_macro_missing_arguments() {
        check_parse(
            r#"{% macro input %}
    ...
{% endmacro input %}"#,
            expect![[r#"
                ROOT@0..46
                  TWIG_MACRO@0..46
                    TWIG_MACRO_STARTING_BLOCK@0..17
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_MACRO@3..8 "macro"
                      TK_WHITESPACE@8..9 " "
                      TK_WORD@9..14 "input"
                      TWIG_ARGUMENTS@14..14
                      TK_WHITESPACE@14..15 " "
                      TK_PERCENT_CURLY@15..17 "%}"
                    BODY@17..25
                      HTML_TEXT@17..25
                        TK_LINE_BREAK@17..18 "\n"
                        TK_WHITESPACE@18..22 "    "
                        TK_DOUBLE_DOT@22..24 ".."
                        TK_DOT@24..25 "."
                    TWIG_MACRO_ENDING_BLOCK@25..46
                      TK_LINE_BREAK@25..26 "\n"
                      TK_CURLY_PERCENT@26..28 "{%"
                      TK_WHITESPACE@28..29 " "
                      TK_ENDMACRO@29..37 "endmacro"
                      TK_WHITESPACE@37..38 " "
                      TK_WORD@38..43 "input"
                      TK_WHITESPACE@43..44 " "
                      TK_PERCENT_CURLY@44..46 "%}"
                error at 15..17: expected ( but found %}
                error at 15..17: expected ) but found %}"#]],
        );
    }

    #[test]
    fn parse_twig_with() {
        check_parse(
            r#"{% with %}
    {% set foo = 42 %}
    {{ foo }} {# foo is 42 here #}
{% endwith %}"#,
            expect![[r##"
                ROOT@0..82
                  TWIG_WITH@0..82
                    TWIG_WITH_STARTING_BLOCK@0..10
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_WITH@3..7 "with"
                      TK_WHITESPACE@7..8 " "
                      TK_PERCENT_CURLY@8..10 "%}"
                    BODY@10..68
                      TWIG_SET@10..33
                        TWIG_SET_BLOCK@10..33
                          TK_LINE_BREAK@10..11 "\n"
                          TK_WHITESPACE@11..15 "    "
                          TK_CURLY_PERCENT@15..17 "{%"
                          TK_WHITESPACE@17..18 " "
                          TK_SET@18..21 "set"
                          TWIG_ASSIGNMENT@21..30
                            TWIG_LITERAL_NAME@21..25
                              TK_WHITESPACE@21..22 " "
                              TK_WORD@22..25 "foo"
                            TK_WHITESPACE@25..26 " "
                            TK_EQUAL@26..27 "="
                            TWIG_EXPRESSION@27..30
                              TWIG_LITERAL_NUMBER@27..30
                                TK_WHITESPACE@27..28 " "
                                TK_NUMBER@28..30 "42"
                          TK_WHITESPACE@30..31 " "
                          TK_PERCENT_CURLY@31..33 "%}"
                      TWIG_VAR@33..47
                        TK_LINE_BREAK@33..34 "\n"
                        TK_WHITESPACE@34..38 "    "
                        TK_OPEN_CURLY_CURLY@38..40 "{{"
                        TWIG_EXPRESSION@40..44
                          TWIG_LITERAL_NAME@40..44
                            TK_WHITESPACE@40..41 " "
                            TK_WORD@41..44 "foo"
                        TK_WHITESPACE@44..45 " "
                        TK_CLOSE_CURLY_CURLY@45..47 "}}"
                      TWIG_COMMENT@47..68
                        TK_WHITESPACE@47..48 " "
                        TK_OPEN_CURLY_HASHTAG@48..50 "{#"
                        TK_WHITESPACE@50..51 " "
                        TK_WORD@51..54 "foo"
                        TK_WHITESPACE@54..55 " "
                        TK_IS@55..57 "is"
                        TK_WHITESPACE@57..58 " "
                        TK_NUMBER@58..60 "42"
                        TK_WHITESPACE@60..61 " "
                        TK_WORD@61..65 "here"
                        TK_WHITESPACE@65..66 " "
                        TK_HASHTAG_CLOSE_CURLY@66..68 "#}"
                    TWIG_WITH_ENDING_BLOCK@68..82
                      TK_LINE_BREAK@68..69 "\n"
                      TK_CURLY_PERCENT@69..71 "{%"
                      TK_WHITESPACE@71..72 " "
                      TK_ENDWITH@72..79 "endwith"
                      TK_WHITESPACE@79..80 " "
                      TK_PERCENT_CURLY@80..82 "%}""##]],
        );
    }

    #[test]
    fn parse_twig_with_and_hash() {
        check_parse(
            r#"{% with { foo: 42 } %}
    {{ foo }} {# foo is 42 here #}
{% endwith %}"#,
            expect![[r##"
                ROOT@0..71
                  TWIG_WITH@0..71
                    TWIG_WITH_STARTING_BLOCK@0..22
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_WITH@3..7 "with"
                      TWIG_EXPRESSION@7..19
                        TWIG_LITERAL_HASH@7..19
                          TK_WHITESPACE@7..8 " "
                          TK_OPEN_CURLY@8..9 "{"
                          TWIG_LITERAL_HASH_ITEMS@9..17
                            TWIG_LITERAL_HASH_PAIR@9..17
                              TWIG_LITERAL_HASH_KEY@9..13
                                TK_WHITESPACE@9..10 " "
                                TK_WORD@10..13 "foo"
                              TK_COLON@13..14 ":"
                              TWIG_EXPRESSION@14..17
                                TWIG_LITERAL_NUMBER@14..17
                                  TK_WHITESPACE@14..15 " "
                                  TK_NUMBER@15..17 "42"
                          TK_WHITESPACE@17..18 " "
                          TK_CLOSE_CURLY@18..19 "}"
                      TK_WHITESPACE@19..20 " "
                      TK_PERCENT_CURLY@20..22 "%}"
                    BODY@22..57
                      TWIG_VAR@22..36
                        TK_LINE_BREAK@22..23 "\n"
                        TK_WHITESPACE@23..27 "    "
                        TK_OPEN_CURLY_CURLY@27..29 "{{"
                        TWIG_EXPRESSION@29..33
                          TWIG_LITERAL_NAME@29..33
                            TK_WHITESPACE@29..30 " "
                            TK_WORD@30..33 "foo"
                        TK_WHITESPACE@33..34 " "
                        TK_CLOSE_CURLY_CURLY@34..36 "}}"
                      TWIG_COMMENT@36..57
                        TK_WHITESPACE@36..37 " "
                        TK_OPEN_CURLY_HASHTAG@37..39 "{#"
                        TK_WHITESPACE@39..40 " "
                        TK_WORD@40..43 "foo"
                        TK_WHITESPACE@43..44 " "
                        TK_IS@44..46 "is"
                        TK_WHITESPACE@46..47 " "
                        TK_NUMBER@47..49 "42"
                        TK_WHITESPACE@49..50 " "
                        TK_WORD@50..54 "here"
                        TK_WHITESPACE@54..55 " "
                        TK_HASHTAG_CLOSE_CURLY@55..57 "#}"
                    TWIG_WITH_ENDING_BLOCK@57..71
                      TK_LINE_BREAK@57..58 "\n"
                      TK_CURLY_PERCENT@58..60 "{%"
                      TK_WHITESPACE@60..61 " "
                      TK_ENDWITH@61..68 "endwith"
                      TK_WHITESPACE@68..69 " "
                      TK_PERCENT_CURLY@69..71 "%}""##]],
        );
    }

    #[test]
    fn parse_twig_with_and_expression() {
        check_parse(
            r#"{% set vars = { foo: 42 } %}
{% with vars %}
    ...
{% endwith %}"#,
            expect![[r#"
                ROOT@0..66
                  TWIG_SET@0..28
                    TWIG_SET_BLOCK@0..28
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..25
                        TWIG_LITERAL_NAME@6..11
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..11 "vars"
                        TK_WHITESPACE@11..12 " "
                        TK_EQUAL@12..13 "="
                        TWIG_EXPRESSION@13..25
                          TWIG_LITERAL_HASH@13..25
                            TK_WHITESPACE@13..14 " "
                            TK_OPEN_CURLY@14..15 "{"
                            TWIG_LITERAL_HASH_ITEMS@15..23
                              TWIG_LITERAL_HASH_PAIR@15..23
                                TWIG_LITERAL_HASH_KEY@15..19
                                  TK_WHITESPACE@15..16 " "
                                  TK_WORD@16..19 "foo"
                                TK_COLON@19..20 ":"
                                TWIG_EXPRESSION@20..23
                                  TWIG_LITERAL_NUMBER@20..23
                                    TK_WHITESPACE@20..21 " "
                                    TK_NUMBER@21..23 "42"
                            TK_WHITESPACE@23..24 " "
                            TK_CLOSE_CURLY@24..25 "}"
                      TK_WHITESPACE@25..26 " "
                      TK_PERCENT_CURLY@26..28 "%}"
                  TWIG_WITH@28..66
                    TWIG_WITH_STARTING_BLOCK@28..44
                      TK_LINE_BREAK@28..29 "\n"
                      TK_CURLY_PERCENT@29..31 "{%"
                      TK_WHITESPACE@31..32 " "
                      TK_WITH@32..36 "with"
                      TWIG_EXPRESSION@36..41
                        TWIG_LITERAL_NAME@36..41
                          TK_WHITESPACE@36..37 " "
                          TK_WORD@37..41 "vars"
                      TK_WHITESPACE@41..42 " "
                      TK_PERCENT_CURLY@42..44 "%}"
                    BODY@44..52
                      HTML_TEXT@44..52
                        TK_LINE_BREAK@44..45 "\n"
                        TK_WHITESPACE@45..49 "    "
                        TK_DOUBLE_DOT@49..51 ".."
                        TK_DOT@51..52 "."
                    TWIG_WITH_ENDING_BLOCK@52..66
                      TK_LINE_BREAK@52..53 "\n"
                      TK_CURLY_PERCENT@53..55 "{%"
                      TK_WHITESPACE@55..56 " "
                      TK_ENDWITH@56..63 "endwith"
                      TK_WHITESPACE@63..64 " "
                      TK_PERCENT_CURLY@64..66 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_with_and_hash_only() {
        check_parse(
            r#"{% set bar = 'bar' %}
{% with { foo: 42 } only %}
    {# only foo is defined #}
    {# bar is not defined #}
{% endwith %}"#,
            expect![[r##"
                ROOT@0..122
                  TWIG_SET@0..21
                    TWIG_SET_BLOCK@0..21
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_SET@3..6 "set"
                      TWIG_ASSIGNMENT@6..18
                        TWIG_LITERAL_NAME@6..10
                          TK_WHITESPACE@6..7 " "
                          TK_WORD@7..10 "bar"
                        TK_WHITESPACE@10..11 " "
                        TK_EQUAL@11..12 "="
                        TWIG_EXPRESSION@12..18
                          TWIG_LITERAL_STRING@12..18
                            TK_WHITESPACE@12..13 " "
                            TK_SINGLE_QUOTES@13..14 "'"
                            TWIG_LITERAL_STRING_INNER@14..17
                              TK_WORD@14..17 "bar"
                            TK_SINGLE_QUOTES@17..18 "'"
                      TK_WHITESPACE@18..19 " "
                      TK_PERCENT_CURLY@19..21 "%}"
                  TWIG_WITH@21..122
                    TWIG_WITH_STARTING_BLOCK@21..49
                      TK_LINE_BREAK@21..22 "\n"
                      TK_CURLY_PERCENT@22..24 "{%"
                      TK_WHITESPACE@24..25 " "
                      TK_WITH@25..29 "with"
                      TWIG_EXPRESSION@29..41
                        TWIG_LITERAL_HASH@29..41
                          TK_WHITESPACE@29..30 " "
                          TK_OPEN_CURLY@30..31 "{"
                          TWIG_LITERAL_HASH_ITEMS@31..39
                            TWIG_LITERAL_HASH_PAIR@31..39
                              TWIG_LITERAL_HASH_KEY@31..35
                                TK_WHITESPACE@31..32 " "
                                TK_WORD@32..35 "foo"
                              TK_COLON@35..36 ":"
                              TWIG_EXPRESSION@36..39
                                TWIG_LITERAL_NUMBER@36..39
                                  TK_WHITESPACE@36..37 " "
                                  TK_NUMBER@37..39 "42"
                          TK_WHITESPACE@39..40 " "
                          TK_CLOSE_CURLY@40..41 "}"
                      TK_WHITESPACE@41..42 " "
                      TK_ONLY@42..46 "only"
                      TK_WHITESPACE@46..47 " "
                      TK_PERCENT_CURLY@47..49 "%}"
                    BODY@49..108
                      TWIG_COMMENT@49..79
                        TK_LINE_BREAK@49..50 "\n"
                        TK_WHITESPACE@50..54 "    "
                        TK_OPEN_CURLY_HASHTAG@54..56 "{#"
                        TK_WHITESPACE@56..57 " "
                        TK_ONLY@57..61 "only"
                        TK_WHITESPACE@61..62 " "
                        TK_WORD@62..65 "foo"
                        TK_WHITESPACE@65..66 " "
                        TK_IS@66..68 "is"
                        TK_WHITESPACE@68..69 " "
                        TK_DEFINED@69..76 "defined"
                        TK_WHITESPACE@76..77 " "
                        TK_HASHTAG_CLOSE_CURLY@77..79 "#}"
                      TWIG_COMMENT@79..108
                        TK_LINE_BREAK@79..80 "\n"
                        TK_WHITESPACE@80..84 "    "
                        TK_OPEN_CURLY_HASHTAG@84..86 "{#"
                        TK_WHITESPACE@86..87 " "
                        TK_WORD@87..90 "bar"
                        TK_WHITESPACE@90..91 " "
                        TK_IS@91..93 "is"
                        TK_WHITESPACE@93..94 " "
                        TK_NOT@94..97 "not"
                        TK_WHITESPACE@97..98 " "
                        TK_DEFINED@98..105 "defined"
                        TK_WHITESPACE@105..106 " "
                        TK_HASHTAG_CLOSE_CURLY@106..108 "#}"
                    TWIG_WITH_ENDING_BLOCK@108..122
                      TK_LINE_BREAK@108..109 "\n"
                      TK_CURLY_PERCENT@109..111 "{%"
                      TK_WHITESPACE@111..112 " "
                      TK_ENDWITH@112..119 "endwith"
                      TK_WHITESPACE@119..120 " "
                      TK_PERCENT_CURLY@120..122 "%}""##]],
        );
    }

    #[test]
    fn parse_twig_cache_key() {
        check_parse(
            r#"{% cache "cache key" %}
    Cached forever (depending on the cache implementation)
{% endcache %}"#,
            expect![[r#"
                ROOT@0..97
                  TWIG_CACHE@0..97
                    TWIG_CACHE_STARTING_BLOCK@0..23
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TWIG_EXPRESSION@8..20
                        TWIG_LITERAL_STRING@8..20
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..19
                            TK_CACHE@10..15 "cache"
                            TK_WHITESPACE@15..16 " "
                            TK_WORD@16..19 "key"
                          TK_DOUBLE_QUOTES@19..20 "\""
                      TK_WHITESPACE@20..21 " "
                      TK_PERCENT_CURLY@21..23 "%}"
                    BODY@23..82
                      HTML_TEXT@23..82
                        TK_LINE_BREAK@23..24 "\n"
                        TK_WHITESPACE@24..28 "    "
                        TK_WORD@28..34 "Cached"
                        TK_WHITESPACE@34..35 " "
                        TK_WORD@35..42 "forever"
                        TK_WHITESPACE@42..43 " "
                        TK_OPEN_PARENTHESIS@43..44 "("
                        TK_WORD@44..53 "depending"
                        TK_WHITESPACE@53..54 " "
                        TK_WORD@54..56 "on"
                        TK_WHITESPACE@56..57 " "
                        TK_WORD@57..60 "the"
                        TK_WHITESPACE@60..61 " "
                        TK_CACHE@61..66 "cache"
                        TK_WHITESPACE@66..67 " "
                        TK_WORD@67..81 "implementation"
                        TK_CLOSE_PARENTHESIS@81..82 ")"
                    TWIG_CACHE_ENDING_BLOCK@82..97
                      TK_LINE_BREAK@82..83 "\n"
                      TK_CURLY_PERCENT@83..85 "{%"
                      TK_WHITESPACE@85..86 " "
                      TK_ENDCACHE@86..94 "endcache"
                      TK_WHITESPACE@94..95 " "
                      TK_PERCENT_CURLY@95..97 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_cache_key_ttl() {
        check_parse(
            r#"{% cache "cache key" ttl(300) %}
    Cached for 300 seconds
{% endcache %}"#,
            expect![[r#"
                ROOT@0..74
                  TWIG_CACHE@0..74
                    TWIG_CACHE_STARTING_BLOCK@0..32
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TWIG_EXPRESSION@8..20
                        TWIG_LITERAL_STRING@8..20
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..19
                            TK_CACHE@10..15 "cache"
                            TK_WHITESPACE@15..16 " "
                            TK_WORD@16..19 "key"
                          TK_DOUBLE_QUOTES@19..20 "\""
                      TWIG_CACHE_TTL@20..29
                        TK_WHITESPACE@20..21 " "
                        TK_TTL@21..24 "ttl"
                        TK_OPEN_PARENTHESIS@24..25 "("
                        TWIG_EXPRESSION@25..28
                          TWIG_LITERAL_NUMBER@25..28
                            TK_NUMBER@25..28 "300"
                        TK_CLOSE_PARENTHESIS@28..29 ")"
                      TK_WHITESPACE@29..30 " "
                      TK_PERCENT_CURLY@30..32 "%}"
                    BODY@32..59
                      HTML_TEXT@32..59
                        TK_LINE_BREAK@32..33 "\n"
                        TK_WHITESPACE@33..37 "    "
                        TK_WORD@37..43 "Cached"
                        TK_WHITESPACE@43..44 " "
                        TK_FOR@44..47 "for"
                        TK_WHITESPACE@47..48 " "
                        TK_NUMBER@48..51 "300"
                        TK_WHITESPACE@51..52 " "
                        TK_WORD@52..59 "seconds"
                    TWIG_CACHE_ENDING_BLOCK@59..74
                      TK_LINE_BREAK@59..60 "\n"
                      TK_CURLY_PERCENT@60..62 "{%"
                      TK_WHITESPACE@62..63 " "
                      TK_ENDCACHE@63..71 "endcache"
                      TK_WHITESPACE@71..72 " "
                      TK_PERCENT_CURLY@72..74 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_cache_key_tags() {
        check_parse(
            r#"{% cache "cache key" tags(['cms', 'blog']) %}
    Some code
{% endcache %}"#,
            expect![[r#"
                ROOT@0..74
                  TWIG_CACHE@0..74
                    TWIG_CACHE_STARTING_BLOCK@0..45
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TWIG_EXPRESSION@8..20
                        TWIG_LITERAL_STRING@8..20
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..19
                            TK_CACHE@10..15 "cache"
                            TK_WHITESPACE@15..16 " "
                            TK_WORD@16..19 "key"
                          TK_DOUBLE_QUOTES@19..20 "\""
                      TWIG_CACHE_TAGS@20..42
                        TK_WHITESPACE@20..21 " "
                        TK_TAGS@21..25 "tags"
                        TK_OPEN_PARENTHESIS@25..26 "("
                        TWIG_EXPRESSION@26..41
                          TWIG_LITERAL_ARRAY@26..41
                            TK_OPEN_SQUARE@26..27 "["
                            TWIG_LITERAL_ARRAY_INNER@27..40
                              TWIG_EXPRESSION@27..32
                                TWIG_LITERAL_STRING@27..32
                                  TK_SINGLE_QUOTES@27..28 "'"
                                  TWIG_LITERAL_STRING_INNER@28..31
                                    TK_WORD@28..31 "cms"
                                  TK_SINGLE_QUOTES@31..32 "'"
                              TK_COMMA@32..33 ","
                              TWIG_EXPRESSION@33..40
                                TWIG_LITERAL_STRING@33..40
                                  TK_WHITESPACE@33..34 " "
                                  TK_SINGLE_QUOTES@34..35 "'"
                                  TWIG_LITERAL_STRING_INNER@35..39
                                    TK_WORD@35..39 "blog"
                                  TK_SINGLE_QUOTES@39..40 "'"
                            TK_CLOSE_SQUARE@40..41 "]"
                        TK_CLOSE_PARENTHESIS@41..42 ")"
                      TK_WHITESPACE@42..43 " "
                      TK_PERCENT_CURLY@43..45 "%}"
                    BODY@45..59
                      HTML_TEXT@45..59
                        TK_LINE_BREAK@45..46 "\n"
                        TK_WHITESPACE@46..50 "    "
                        TK_WORD@50..54 "Some"
                        TK_WHITESPACE@54..55 " "
                        TK_WORD@55..59 "code"
                    TWIG_CACHE_ENDING_BLOCK@59..74
                      TK_LINE_BREAK@59..60 "\n"
                      TK_CURLY_PERCENT@60..62 "{%"
                      TK_WHITESPACE@62..63 " "
                      TK_ENDCACHE@63..71 "endcache"
                      TK_WHITESPACE@71..72 " "
                      TK_PERCENT_CURLY@72..74 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_cache_key_ttl_tags() {
        check_parse(
            r#"{% cache "cache key" ttl(5) tags(['cms', 'blog']) %}
    Some code
{% endcache %}"#,
            expect![[r#"
                ROOT@0..81
                  TWIG_CACHE@0..81
                    TWIG_CACHE_STARTING_BLOCK@0..52
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TWIG_EXPRESSION@8..20
                        TWIG_LITERAL_STRING@8..20
                          TK_WHITESPACE@8..9 " "
                          TK_DOUBLE_QUOTES@9..10 "\""
                          TWIG_LITERAL_STRING_INNER@10..19
                            TK_CACHE@10..15 "cache"
                            TK_WHITESPACE@15..16 " "
                            TK_WORD@16..19 "key"
                          TK_DOUBLE_QUOTES@19..20 "\""
                      TWIG_CACHE_TTL@20..27
                        TK_WHITESPACE@20..21 " "
                        TK_TTL@21..24 "ttl"
                        TK_OPEN_PARENTHESIS@24..25 "("
                        TWIG_EXPRESSION@25..26
                          TWIG_LITERAL_NUMBER@25..26
                            TK_NUMBER@25..26 "5"
                        TK_CLOSE_PARENTHESIS@26..27 ")"
                      TWIG_CACHE_TAGS@27..49
                        TK_WHITESPACE@27..28 " "
                        TK_TAGS@28..32 "tags"
                        TK_OPEN_PARENTHESIS@32..33 "("
                        TWIG_EXPRESSION@33..48
                          TWIG_LITERAL_ARRAY@33..48
                            TK_OPEN_SQUARE@33..34 "["
                            TWIG_LITERAL_ARRAY_INNER@34..47
                              TWIG_EXPRESSION@34..39
                                TWIG_LITERAL_STRING@34..39
                                  TK_SINGLE_QUOTES@34..35 "'"
                                  TWIG_LITERAL_STRING_INNER@35..38
                                    TK_WORD@35..38 "cms"
                                  TK_SINGLE_QUOTES@38..39 "'"
                              TK_COMMA@39..40 ","
                              TWIG_EXPRESSION@40..47
                                TWIG_LITERAL_STRING@40..47
                                  TK_WHITESPACE@40..41 " "
                                  TK_SINGLE_QUOTES@41..42 "'"
                                  TWIG_LITERAL_STRING_INNER@42..46
                                    TK_WORD@42..46 "blog"
                                  TK_SINGLE_QUOTES@46..47 "'"
                            TK_CLOSE_SQUARE@47..48 "]"
                        TK_CLOSE_PARENTHESIS@48..49 ")"
                      TK_WHITESPACE@49..50 " "
                      TK_PERCENT_CURLY@50..52 "%}"
                    BODY@52..66
                      HTML_TEXT@52..66
                        TK_LINE_BREAK@52..53 "\n"
                        TK_WHITESPACE@53..57 "    "
                        TK_WORD@57..61 "Some"
                        TK_WHITESPACE@61..62 " "
                        TK_WORD@62..66 "code"
                    TWIG_CACHE_ENDING_BLOCK@66..81
                      TK_LINE_BREAK@66..67 "\n"
                      TK_CURLY_PERCENT@67..69 "{%"
                      TK_WHITESPACE@69..70 " "
                      TK_ENDCACHE@70..78 "endcache"
                      TK_WHITESPACE@78..79 " "
                      TK_PERCENT_CURLY@79..81 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_cache_key_string_concatenation() {
        check_parse(
            r#"{% cache "blog_post;v1;" ~ post.id ~ ";" ~ post.updated_at %}
    Some code
{% endcache %}"#,
            expect![[r#"
                ROOT@0..90
                  TWIG_CACHE@0..90
                    TWIG_CACHE_STARTING_BLOCK@0..61
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TWIG_EXPRESSION@8..58
                        TWIG_BINARY_EXPRESSION@8..58
                          TWIG_BINARY_EXPRESSION@8..40
                            TWIG_BINARY_EXPRESSION@8..34
                              TWIG_EXPRESSION@8..24
                                TWIG_LITERAL_STRING@8..24
                                  TK_WHITESPACE@8..9 " "
                                  TK_DOUBLE_QUOTES@9..10 "\""
                                  TWIG_LITERAL_STRING_INNER@10..23
                                    TK_WORD@10..19 "blog_post"
                                    TK_SEMICOLON@19..20 ";"
                                    TK_WORD@20..22 "v1"
                                    TK_SEMICOLON@22..23 ";"
                                  TK_DOUBLE_QUOTES@23..24 "\""
                              TK_WHITESPACE@24..25 " "
                              TK_TILDE@25..26 "~"
                              TWIG_EXPRESSION@26..34
                                TWIG_ACCESSOR@26..34
                                  TWIG_OPERAND@26..31
                                    TWIG_LITERAL_NAME@26..31
                                      TK_WHITESPACE@26..27 " "
                                      TK_WORD@27..31 "post"
                                  TK_DOT@31..32 "."
                                  TWIG_OPERAND@32..34
                                    TWIG_LITERAL_NAME@32..34
                                      TK_WORD@32..34 "id"
                            TK_WHITESPACE@34..35 " "
                            TK_TILDE@35..36 "~"
                            TWIG_EXPRESSION@36..40
                              TWIG_LITERAL_STRING@36..40
                                TK_WHITESPACE@36..37 " "
                                TK_DOUBLE_QUOTES@37..38 "\""
                                TWIG_LITERAL_STRING_INNER@38..39
                                  TK_SEMICOLON@38..39 ";"
                                TK_DOUBLE_QUOTES@39..40 "\""
                          TK_WHITESPACE@40..41 " "
                          TK_TILDE@41..42 "~"
                          TWIG_EXPRESSION@42..58
                            TWIG_ACCESSOR@42..58
                              TWIG_OPERAND@42..47
                                TWIG_LITERAL_NAME@42..47
                                  TK_WHITESPACE@42..43 " "
                                  TK_WORD@43..47 "post"
                              TK_DOT@47..48 "."
                              TWIG_OPERAND@48..58
                                TWIG_LITERAL_NAME@48..58
                                  TK_WORD@48..58 "updated_at"
                      TK_WHITESPACE@58..59 " "
                      TK_PERCENT_CURLY@59..61 "%}"
                    BODY@61..75
                      HTML_TEXT@61..75
                        TK_LINE_BREAK@61..62 "\n"
                        TK_WHITESPACE@62..66 "    "
                        TK_WORD@66..70 "Some"
                        TK_WHITESPACE@70..71 " "
                        TK_WORD@71..75 "code"
                    TWIG_CACHE_ENDING_BLOCK@75..90
                      TK_LINE_BREAK@75..76 "\n"
                      TK_CURLY_PERCENT@76..78 "{%"
                      TK_WHITESPACE@78..79 " "
                      TK_ENDCACHE@79..87 "endcache"
                      TK_WHITESPACE@87..88 " "
                      TK_PERCENT_CURLY@88..90 "%}""#]],
        );
    }

    #[test]
    fn parse_twig_cache_missing_key() {
        check_parse(
            r#"{% cache %}
    Some code
{% endcache %}"#,
            expect![[r#"
                ROOT@0..40
                  TWIG_CACHE@0..40
                    TWIG_CACHE_STARTING_BLOCK@0..11
                      TK_CURLY_PERCENT@0..2 "{%"
                      TK_WHITESPACE@2..3 " "
                      TK_CACHE@3..8 "cache"
                      TK_WHITESPACE@8..9 " "
                      TK_PERCENT_CURLY@9..11 "%}"
                    BODY@11..25
                      HTML_TEXT@11..25
                        TK_LINE_BREAK@11..12 "\n"
                        TK_WHITESPACE@12..16 "    "
                        TK_WORD@16..20 "Some"
                        TK_WHITESPACE@20..21 " "
                        TK_WORD@21..25 "code"
                    TWIG_CACHE_ENDING_BLOCK@25..40
                      TK_LINE_BREAK@25..26 "\n"
                      TK_CURLY_PERCENT@26..28 "{%"
                      TK_WHITESPACE@28..29 " "
                      TK_ENDCACHE@29..37 "endcache"
                      TK_WHITESPACE@37..38 " "
                      TK_PERCENT_CURLY@38..40 "%}"
                error at 9..11: expected twig expression as cache key but found %}"#]],
        );
    }

  #[test]
  fn parse_twig_trans() {
      check_parse(
          "{% trans %} hello world {% endtrans %}",
          expect![[r#"
              ROOT@0..38
                TWIG_TRANS@0..38
                  TWIG_TRANS_STARTING_BLOCK@0..11
                    TK_CURLY_PERCENT@0..2 "{%"
                    TK_WHITESPACE@2..3 " "
                    TK_TRANS@3..8 "trans"
                    TK_WHITESPACE@8..9 " "
                    TK_PERCENT_CURLY@9..11 "%}"
                  BODY@11..23
                    HTML_TEXT@11..23
                      TK_WHITESPACE@11..12 " "
                      TK_WORD@12..17 "hello"
                      TK_WHITESPACE@17..18 " "
                      TK_WORD@18..23 "world"
                  TWIG_TRANS_ENDING_BLOCK@23..38
                    TK_WHITESPACE@23..24 " "
                    TK_CURLY_PERCENT@24..26 "{%"
                    TK_WHITESPACE@26..27 " "
                    TK_ENDTRANS@27..35 "endtrans"
                    TK_WHITESPACE@35..36 " "
                    TK_PERCENT_CURLY@36..38 "%}""#]],
    );
  }
}
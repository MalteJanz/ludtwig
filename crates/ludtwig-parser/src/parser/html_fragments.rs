use rowan::NodeOrToken;
use rowan::ast::AstNode;

use crate::T;
use crate::parser::{DYNAMIC_HTML_TAG_PREFIX, ParseError};
use crate::syntax::typed::{HtmlEndingTag, HtmlTag};
use crate::syntax::untyped::{SyntaxKind, SyntaxNode, TextRange};

#[derive(Clone, Eq, PartialEq)]
struct Context {
    conditions: Vec<(String, bool)>,
    scopes: Vec<(SyntaxKind, TextRange)>,
}

struct Fragment {
    name: String,
    key: String,
    context: Context,
    range: TextRange,
    opening: bool,
}

fn tag_name(node: &SyntaxNode) -> Option<(String, String, TextRange)> {
    if let Some(token) = node
        .children_with_tokens()
        .filter_map(NodeOrToken::into_token)
        .find(|token| token.kind() == T![word] || token.kind() == T![twig component name])
    {
        return Some((
            token.text().to_string(),
            token.text().to_string(),
            token.text_range(),
        ));
    }

    let twig_var = node
        .children()
        .find(|child| child.kind() == SyntaxKind::TWIG_VAR)?;
    let expression = twig_var
        .children()
        .find(|child| child.kind() == SyntaxKind::TWIG_EXPRESSION)?;
    let key = expression
        .descendants_with_tokens()
        .filter_map(NodeOrToken::into_token)
        .filter(|token| !token.kind().is_trivia())
        .map(|token| token.text().to_owned())
        .collect::<Vec<_>>()
        .join("\0");
    Some((
        twig_var.to_string(),
        format!("{DYNAMIC_HTML_TAG_PREFIX}{key}\0"),
        twig_var.text_range(),
    ))
}

fn branch_context(node: &SyntaxNode) -> Context {
    let mut conditions = Vec::new();
    let mut scopes = Vec::new();

    for ancestor in node.ancestors() {
        if ancestor.kind() == SyntaxKind::TWIG_IF {
            let body = node
                .ancestors()
                .find(|parent| parent.parent().as_ref() == Some(&ancestor));
            let mut branch_conditions: Vec<(String, bool)> = Vec::new();
            for child in ancestor.children() {
                match child.kind() {
                    SyntaxKind::TWIG_IF_BLOCK | SyntaxKind::TWIG_ELSE_IF_BLOCK => {
                        if let Some((_, positive)) = branch_conditions.last_mut() {
                            *positive = false;
                        }
                        let expression = child
                            .children()
                            .find(|expression| expression.kind() == SyntaxKind::TWIG_EXPRESSION)
                            .map(|expression| {
                                expression
                                    .descendants_with_tokens()
                                    .filter_map(NodeOrToken::into_token)
                                    .filter(|token| !token.kind().is_trivia())
                                    .map(|token| token.text().to_owned())
                                    .collect::<Vec<_>>()
                                    .join("\0")
                            })
                            .unwrap_or_default();
                        branch_conditions.push((expression, true));
                    }
                    SyntaxKind::TWIG_ELSE_BLOCK => {
                        if let Some((_, positive)) = branch_conditions.last_mut() {
                            *positive = false;
                        }
                    }
                    SyntaxKind::BODY if body.as_ref() == Some(&child) => break,
                    _ => {}
                }
            }
            conditions.extend(branch_conditions);
        } else if is_twig_scope(ancestor.kind()) {
            scopes.push((ancestor.kind(), ancestor.text_range()));
        }
    }

    Context { conditions, scopes }
}

fn is_twig_scope(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::TWIG_BLOCK
            | SyntaxKind::TWIG_SET
            | SyntaxKind::TWIG_FOR
            | SyntaxKind::TWIG_APPLY
            | SyntaxKind::TWIG_AUTOESCAPE
            | SyntaxKind::TWIG_EMBED
            | SyntaxKind::TWIG_SANDBOX
            | SyntaxKind::TWIG_VERBATIM
            | SyntaxKind::TWIG_MACRO
            | SyntaxKind::TWIG_WITH
            | SyntaxKind::TWIG_CACHE
            | SyntaxKind::TWIG_COMPONENT
            | SyntaxKind::TWIG_TRANS
            | SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL
    )
}

fn parse_error(range: TextRange, expected: String, opening: bool) -> ParseError {
    ParseError {
        range,
        found: Some(if opening { T!["<"] } else { T!["</"] }),
        expected,
        secondary: None,
        message: None,
    }
}

fn mismatched_fragment_error(opening: &Fragment, closing: &Fragment) -> ParseError {
    let mut error = parse_error(
        closing.range,
        format!(
            "an opening <{}> under the same Twig conditions",
            closing.name
        ),
        false,
    );
    let reason = if opening.key != closing.key {
        "different tag-name expressions"
    } else if opening.context.conditions == closing.context.conditions {
        "different Twig scopes"
    } else {
        "different Twig conditions"
    };
    error.message = Some(format!(
        "closing </{}> does not match opening <{}>: {reason}",
        closing.name, opening.name
    ));
    error.secondary = Some((opening.range, format!("opening <{}> is here", opening.name)));
    error
}

fn mutually_exclusive(a: &Context, b: &Context) -> bool {
    a.conditions.iter().any(|(expression, positive)| {
        b.conditions
            .iter()
            .any(|(other, other_positive)| expression == other && positive != other_positive)
    })
}

fn collect_fragments(root: &SyntaxNode) -> (Vec<Fragment>, Vec<ParseError>) {
    let mut fragments = Vec::new();
    let mut errors = Vec::new();
    for node in root.descendants() {
        if let Some(tag) = HtmlTag::cast(node.clone()) {
            if let (Some(starting), Some(ending)) = (tag.starting_tag(), tag.ending_tag()) {
                if let (
                    Some((opening, opening_key, opening_range)),
                    Some((closing, closing_key, closing_range)),
                ) = (tag_name(starting.syntax()), tag_name(ending.syntax()))
                {
                    if opening_key != closing_key {
                        let opening = Fragment {
                            name: opening,
                            key: opening_key,
                            context: branch_context(&node),
                            range: opening_range,
                            opening: true,
                        };
                        let closing = Fragment {
                            name: closing,
                            key: closing_key,
                            context: branch_context(&node),
                            range: closing_range,
                            opening: false,
                        };
                        errors.push(mismatched_fragment_error(&opening, &closing));
                    }
                }
            }
            if tag
                .ending_tag()
                .is_some_and(|ending| tag_name(ending.syntax()).is_none())
            {
                if let Some((name, key, range)) = tag
                    .starting_tag()
                    .and_then(|starting| tag_name(starting.syntax()))
                {
                    fragments.push(Fragment {
                        name,
                        key,
                        context: branch_context(&node),
                        range,
                        opening: true,
                    });
                }
            }
        } else if let Some(ending) = HtmlEndingTag::cast(node.clone()) {
            if ending.html_tag().is_none() {
                if let Some((name, key, range)) = tag_name(&node) {
                    fragments.push(Fragment {
                        name,
                        key,
                        context: branch_context(&node),
                        range,
                        opening: false,
                    });
                }
            }
        }
    }

    (fragments, errors)
}

pub(super) fn validate(root: &SyntaxNode) -> Vec<ParseError> {
    let (fragments, mut errors) = collect_fragments(root);
    let mut openings = Vec::new();
    let mut pairs = Vec::new();
    for (index, fragment) in fragments.iter().enumerate() {
        if fragment.opening {
            openings.push(index);
        } else if let Some(position) = openings.iter().rposition(|opening| {
            let candidate = &fragments[*opening];
            candidate.key == fragment.key
                && candidate.context.conditions == fragment.context.conditions
                && (candidate.context.conditions.is_empty()
                    || candidate.context.scopes == fragment.context.scopes)
        }) {
            let opening = openings.remove(position);
            pairs.push((opening, index));
        } else if let Some(position) = openings.iter().rposition(|opening| {
            fragments[*opening].key == fragment.key
                || (fragments[*opening].key.starts_with(DYNAMIC_HTML_TAG_PREFIX)
                    && fragment.key.starts_with(DYNAMIC_HTML_TAG_PREFIX))
        }) {
            let opening = &fragments[openings.remove(position)];
            errors.push(mismatched_fragment_error(opening, fragment));
        } else {
            errors.push(parse_error(
                fragment.range,
                format!(
                    "an opening <{}> under the same Twig conditions",
                    fragment.name
                ),
                false,
            ));
        }
    }
    for opening in openings {
        let fragment = &fragments[opening];
        errors.push(parse_error(
            fragment.range,
            format!(
                "a closing </{}> under the same Twig conditions",
                fragment.name
            ),
            true,
        ));
    }

    for (i, &(left_open, left_close)) in pairs.iter().enumerate() {
        for &(right_open, right_close) in &pairs[i + 1..] {
            if left_open < right_open
                && right_open < left_close
                && left_close < right_close
                && !mutually_exclusive(
                    &fragments[left_open].context,
                    &fragments[right_open].context,
                )
            {
                errors.push(parse_error(
                    fragments[left_close].range,
                    "properly nested HTML tags under the same Twig conditions".to_string(),
                    false,
                ));
            }
        }
    }

    errors
}

use rowan::NodeOrToken;
use rowan::ast::AstNode;

use crate::T;
use crate::parser::ParseError;
use crate::syntax::typed::{HtmlEndingTag, HtmlTag};
use crate::syntax::untyped::{SyntaxKind, SyntaxNode, TextRange};

#[derive(Clone, Eq, PartialEq)]
struct Context {
    conditions: Vec<(String, bool)>,
    scopes: Vec<(SyntaxKind, TextRange)>,
}

struct Fragment {
    name: String,
    context: Context,
    range: TextRange,
    opening: bool,
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
    }
}

fn mutually_exclusive(a: &Context, b: &Context) -> bool {
    a.conditions.iter().any(|(expression, positive)| {
        b.conditions
            .iter()
            .any(|(other, other_positive)| expression == other && positive != other_positive)
    })
}

pub(super) fn validate(root: &SyntaxNode) -> Vec<ParseError> {
    let mut fragments = Vec::new();
    for node in root.descendants() {
        if let Some(tag) = HtmlTag::cast(node.clone()) {
            if tag
                .ending_tag()
                .is_some_and(|ending| ending.name().is_none())
            {
                if let Some(name) = tag.name() {
                    fragments.push(Fragment {
                        name: name.text().to_string(),
                        context: branch_context(&node),
                        range: name.text_range(),
                        opening: true,
                    });
                }
            }
        } else if let Some(ending) = HtmlEndingTag::cast(node.clone()) {
            if ending.html_tag().is_none() {
                if let Some(name) = ending.name() {
                    fragments.push(Fragment {
                        name: name.text().to_string(),
                        context: branch_context(&node),
                        range: name.text_range(),
                        opening: false,
                    });
                }
            }
        }
    }

    let mut errors = Vec::new();
    let mut openings = Vec::new();
    let mut pairs = Vec::new();
    for (index, fragment) in fragments.iter().enumerate() {
        if fragment.opening {
            openings.push(index);
        } else if let Some(position) = openings.iter().rposition(|opening| {
            let candidate = &fragments[*opening];
            candidate.name == fragment.name
                && candidate.context.conditions == fragment.context.conditions
                && (candidate.context.conditions.is_empty()
                    || candidate.context.scopes == fragment.context.scopes)
        }) {
            let opening = openings.remove(position);
            pairs.push((opening, index));
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

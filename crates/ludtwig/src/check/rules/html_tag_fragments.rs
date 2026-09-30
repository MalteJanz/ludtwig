use ludtwig_parser::syntax::typed::{AstNode, HtmlEndingTag, HtmlTag};
use ludtwig_parser::syntax::untyped::{
    SyntaxElement, SyntaxKind, SyntaxNode, TextRange, WalkEvent,
};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleHtmlTagFragments;

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
    ignored: bool,
}

pub(crate) fn is_trivia_sensitive_tag(tag: &HtmlTag) -> bool {
    tag.starting_tag()
        .and_then(|starting| starting.name())
        .is_some_and(|name| {
            matches!(
                name.text().to_ascii_lowercase().as_str(),
                "pre" | "textarea" | "script" | "style"
            )
        })
}

pub(crate) fn uncertain_trivia_ranges(root: &SyntaxNode) -> Vec<TextRange> {
    let mut openings = Vec::new();
    let mut ranges = Vec::new();
    for node in root.descendants() {
        if let Some(tag) = HtmlTag::cast(node.clone()) {
            if is_trivia_sensitive_tag(&tag)
                && tag
                    .ending_tag()
                    .is_some_and(|ending| ending.name().is_none())
            {
                if let Some(name) = tag.starting_tag().and_then(|starting| starting.name()) {
                    openings.push((name.text().to_owned(), tag.syntax().text_range().end()));
                }
            }
        } else if let Some(ending) = HtmlEndingTag::cast(node) {
            if ending.html_tag().is_none() {
                if let Some(name) = ending.name() {
                    if let Some(index) = openings
                        .iter()
                        .rposition(|(opening_name, _)| opening_name == name.text())
                    {
                        let (_, start) = openings.remove(index);
                        ranges.push(TextRange::new(start, ending.syntax().text_range().end()));
                    }
                }
            }
        }
    }
    for (_, start) in openings {
        ranges.push(TextRange::new(start, root.text_range().end()));
    }
    ranges
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
                                    .filter_map(SyntaxElement::into_token)
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

fn unmatched_fragment_error(rule: &RuleHtmlTagFragments, fragment: &Fragment) -> CheckResult {
    let message = if fragment.opening {
        format!("opening <{}> has no matching closing tag", fragment.name)
    } else {
        format!("closing </{}> has no matching opening tag", fragment.name)
    };
    rule.create_result(Severity::Error, message)
        .primary_note(fragment.range, "unmatched HTML tag fragment")
}

fn mismatched_fragment_error(
    rule: &RuleHtmlTagFragments,
    opening: &Fragment,
    closing: &Fragment,
) -> CheckResult {
    let reason = if opening.context.conditions == closing.context.conditions {
        "different Twig scopes"
    } else {
        "different Twig conditions"
    };
    rule.create_result(
        Severity::Error,
        format!(
            "closing </{}> does not match opening <{}>: {reason}",
            closing.name, opening.name
        ),
    )
    .primary_note(closing.range, "closing tag")
    .secondary_note(opening.range, format!("opening <{}> is here", opening.name))
}

fn mutually_exclusive(a: &Context, b: &Context) -> bool {
    a.conditions.iter().any(|(expression, positive)| {
        b.conditions
            .iter()
            .any(|(other, other_positive)| expression == other && positive != other_positive)
    })
}

fn collect_fragments(rule: &RuleHtmlTagFragments, root: &SyntaxNode) -> Vec<Fragment> {
    let mut fragments = Vec::new();
    let mut tree_iter = root.preorder();
    while let Some(walk) = tree_iter.next() {
        let node = match walk {
            WalkEvent::Enter(node) => {
                if node.kind() == SyntaxKind::ERROR {
                    tree_iter.skip_subtree();
                    continue;
                }
                node
            }
            WalkEvent::Leave(_) => continue,
        };
        if let Some(tag) = HtmlTag::cast(node.clone()) {
            let ignored = rule.is_ignored_for_node(&node);
            if tag
                .ending_tag()
                .is_some_and(|ending| ending.name().is_none())
            {
                if let Some(name) = tag.starting_tag().and_then(|starting| starting.name()) {
                    fragments.push(Fragment {
                        name: name.text().to_owned(),
                        context: branch_context(&node),
                        range: name.text_range(),
                        opening: true,
                        ignored,
                    });
                }
            }
        } else if let Some(ending) = HtmlEndingTag::cast(node.clone()) {
            if ending.html_tag().is_none() {
                if let Some(name) = ending.name() {
                    fragments.push(Fragment {
                        name: name.text().to_owned(),
                        context: branch_context(&node),
                        range: name.text_range(),
                        opening: false,
                        ignored: rule.is_ignored_for_node(&node),
                    });
                }
            }
        }
    }

    fragments
}

fn validate(rule: &RuleHtmlTagFragments, root: &SyntaxNode) -> Vec<CheckResult> {
    let fragments = collect_fragments(rule, root);
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
                && candidate.context.scopes == fragment.context.scopes
        }) {
            let opening = openings.remove(position);
            pairs.push((opening, index));
        } else if let Some(position) = openings
            .iter()
            .rposition(|opening| fragments[*opening].name == fragment.name)
        {
            let opening = &fragments[openings.remove(position)];
            if !opening.ignored && !fragment.ignored {
                errors.push(mismatched_fragment_error(rule, opening, fragment));
            }
        } else if !fragment.ignored {
            errors.push(unmatched_fragment_error(rule, fragment));
        }
    }
    for opening in openings {
        let fragment = &fragments[opening];
        if !fragment.ignored {
            errors.push(unmatched_fragment_error(rule, fragment));
        }
    }

    for (i, &(left_open, left_close)) in pairs.iter().enumerate() {
        for &(right_open, right_close) in &pairs[i + 1..] {
            if left_open < right_open
                && right_open < left_close
                && left_close < right_close
                && ![left_open, left_close, right_open, right_close]
                    .iter()
                    .any(|index| fragments[*index].ignored)
                && !mutually_exclusive(
                    &fragments[left_open].context,
                    &fragments[right_open].context,
                )
            {
                errors.push(
                    rule.create_result(
                        Severity::Error,
                        "HTML tag fragments are not properly nested",
                    )
                    .primary_note(fragments[left_close].range, "closed before the inner tag")
                    .secondary_note(fragments[right_open].range, "inner tag opened here"),
                );
            }
        }
    }

    errors
}

impl Rule for RuleHtmlTagFragments {
    fn name(&self) -> &'static str {
        "html-tag-fragments"
    }

    fn check_root(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let results = validate(self, &node);
        if results.is_empty() {
            None
        } else {
            Some(results)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn accepts_matching_fragments() {
        for source in [
            "{% if a %}<div>{% endif %}{% if a %}</div>{% endif %}",
            "{% block a %}{% if a %}<div>{% endif %}{% if a %}</div>{% endif %}{% endblock %}",
        ] {
            test_rule("html-tag-fragments", source, expect![""]);
        }
    }

    #[test]
    fn rejects_different_conditions() {
        test_rule(
            "html-tag-fragments",
            "{% if a %}<div>{% endif %}\n{% if b %}</div>{% endif %}",
            expect![[r#"
                error[html-tag-fragments]: closing </div> does not match opening <div>: different Twig conditions
                  ┌─ ./debug-rule.html.twig:2:13
                  │
                1 │ {% if a %}<div>{% endif %}
                  │            --- opening <div> is here
                2 │ {% if b %}</div>{% endif %}
                  │             ^^^ closing tag

            "#]],
        );
    }

    #[test]
    fn rejects_different_blocks() {
        test_rule(
            "html-tag-fragments",
            "{% block a %}<div>{% endblock %}{% block b %}</div>{% endblock %}",
            expect![[r#"
                error[html-tag-fragments]: closing </div> does not match opening <div>: different Twig scopes
                  ┌─ ./debug-rule.html.twig:1:48
                  │
                1 │ {% block a %}<div>{% endblock %}{% block b %}</div>{% endblock %}
                  │               ---                              ^^^ closing tag
                  │               │                                 
                  │               opening <div> is here

            "#]],
        );
    }

    #[test]
    fn rejects_crossing_fragments() {
        test_rule(
            "html-tag-fragments",
            "{% if a %}<div><span>{% endif %}{% if a %}</div></span>{% endif %}",
            expect![[r#"
                error[html-tag-fragments]: HTML tag fragments are not properly nested
                  ┌─ ./debug-rule.html.twig:1:45
                  │
                1 │ {% if a %}<div><span>{% endif %}{% if a %}</div></span>{% endif %}
                  │                 ----                        ^^^ closed before the inner tag
                  │                 │                            
                  │                 inner tag opened here

            "#]],
        );
    }

    #[test]
    fn rejects_unmatched_fragment() {
        test_rule(
            "html-tag-fragments",
            "{% if a %}<div>{% endif %}",
            expect![[r#"
                error[html-tag-fragments]: opening <div> has no matching closing tag
                  ┌─ ./debug-rule.html.twig:1:12
                  │
                1 │ {% if a %}<div>{% endif %}
                  │            ^^^ unmatched HTML tag fragment

            "#]],
        );
    }

    #[test]
    fn ignore_directive_skips_its_block() {
        test_rule(
            "html-tag-fragments",
            "{# ludtwig-ignore html-tag-fragments #}{% block a %}{% if a %}<div>{% endif %}{% if b %}</div>{% endif %}{% endblock %}",
            expect![""],
        );
    }

    #[test]
    fn ignore_directive_on_closing_fragment_only_suppresses_its_pair() {
        let source = "{% block a %}<html>{% endblock %}{# ludtwig-ignore html-tag-fragments #}</html>{% if a %}<div>{% endif %}{% if b %}</div>{% endif %}";
        let (root, parse_errors) = ludtwig_parser::parse(source).split();
        assert!(parse_errors.is_empty());

        let errors = super::validate(&super::RuleHtmlTagFragments, &root);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("different Twig conditions"));
    }

    #[test]
    fn ignore_directive_on_opening_fragment_suppresses_its_pair() {
        test_rule(
            "html-tag-fragments",
            "{# ludtwig-ignore html-tag-fragments #}{% block a %}<html>{% endblock %}</html>",
            expect![""],
        );
    }

    #[test]
    fn reports_missing_closing_tag_inside_twig_block() {
        test_rule(
            "html-tag-fragments",
            "<div>{% block a %}<p>hello{% endblock %}<span>world</span></div>",
            expect![[r#"
                error[html-tag-fragments]: opening <p> has no matching closing tag
                  ┌─ ./debug-rule.html.twig:1:20
                  │
                1 │ <div>{% block a %}<p>hello{% endblock %}<span>world</span></div>
                  │                    ^ unmatched HTML tag fragment

            "#]],
        );
    }
}

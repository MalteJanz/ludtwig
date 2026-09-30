use ludtwig_parser::syntax::typed::{AstNode, HtmlAttribute};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode, WalkEvent};
use std::collections::HashMap;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleHtmlDuplicateId;

fn in_different_if_branches(first: &HtmlAttribute, second: &HtmlAttribute) -> bool {
    for conditional in first
        .syntax()
        .ancestors()
        .filter(|node| node.kind() == SyntaxKind::TWIG_IF)
    {
        if !second.syntax().ancestors().any(|node| node == conditional) {
            continue;
        }

        let branch = |attribute: &HtmlAttribute| {
            conditional.children().find(|node| {
                node.kind() == SyntaxKind::BODY
                    && node
                        .text_range()
                        .contains_range(attribute.syntax().text_range())
            })
        };

        if let (Some(first_branch), Some(second_branch)) = (branch(first), branch(second)) {
            if first_branch != second_branch {
                return true;
            }
        }
    }

    false
}

impl Rule for RuleHtmlDuplicateId {
    fn name(&self) -> &'static str {
        "html-duplicate-id"
    }

    fn check_root(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let mut id_table: HashMap<String, Vec<HtmlAttribute>> = HashMap::new();

        let mut is_ignored = false;
        let mut check_results = vec![];
        let mut tree_iter = node.preorder();
        while let Some(walk) = tree_iter.next() {
            match walk {
                WalkEvent::Enter(element) => {
                    if element.kind() == SyntaxKind::ERROR {
                        tree_iter.skip_subtree();
                        continue;
                    }
                    if self.check_for_rule_ignore_enter(&mut is_ignored, &mut tree_iter, &element) {
                        continue;
                    }
                    if is_ignored {
                        continue;
                    }

                    let Some(attribute) = HtmlAttribute::cast(element) else {
                        continue;
                    };
                    let Some(name_token) = attribute.name() else {
                        continue;
                    };
                    if name_token.text() != "id" {
                        continue;
                    }
                    let Some(value) = attribute.value() else {
                        continue;
                    };
                    let Some(inner) = value.get_inner() else {
                        continue;
                    };

                    // Skip dynamic IDs that contain twig expressions (child nodes)
                    if inner.syntax().children().next().is_some() {
                        continue;
                    }

                    let id_text = inner.syntax().text().to_string();
                    if id_text.is_empty() {
                        continue;
                    }

                    let previous = id_table.entry(id_text.clone()).or_default();
                    if let Some(first_inner) = previous
                        .iter()
                        .filter(|first| !in_different_if_branches(first, &attribute))
                        .find_map(|first| first.value()?.get_inner())
                    {
                        check_results.push(
                            self.create_result(
                                Severity::Warning,
                                "duplicate HTML element id attribute value",
                            )
                            .primary_note(
                                inner.syntax().text_range(),
                                format!("duplicate id '{id_text}'"),
                            )
                            .secondary_note(
                                first_inner.syntax().text_range(),
                                "first defined here",
                            ),
                        );
                    }
                    previous.push(attribute);
                }
                WalkEvent::Leave(element) => {
                    self.check_for_rule_ignore_leave(&mut is_ignored, &element);
                }
            }
        }

        if check_results.is_empty() {
            None
        } else {
            Some(check_results)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn rule_reports_duplicate_ids() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
<span id="bar"></span>
<p id="foo"></p>"#,
            expect![[r#"
                warning[html-duplicate-id]: duplicate HTML element id attribute value
                  ┌─ ./debug-rule.html.twig:3:8
                  │
                1 │ <div id="foo"></div>
                  │          --- first defined here
                2 │ <span id="bar"></span>
                3 │ <p id="foo"></p>
                  │        ^^^ duplicate id 'foo'

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report_unique_ids() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
<span id="bar"></span>
<p id="baz"></p>"#,
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_ids_in_exclusive_if_branches() {
        test_rule(
            "html-duplicate-id",
            r#"{% if feature('v6.8.0.0') %}<ul id="footerColumns"></ul>{% else %}<div id="footerColumns"></div>{% endif %}"#,
            expect![""],
        );
    }

    #[test]
    fn rule_does_not_report_dynamic_ids() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="item-{{ id }}"></div>
<span id="item-{{ id }}"></span>"#,
            expect![""],
        );
    }

    #[test]
    fn rule_ignores_with_specific_rule_directive() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
{# ludtwig-ignore html-duplicate-id #}
<p id="foo"></p>"#,
            expect![""],
        );
    }

    #[test]
    fn rule_ignores_with_blanket_directive() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
{# ludtwig-ignore #}
<p id="foo"></p>"#,
            expect![""],
        );
    }

    #[test]
    fn rule_still_reports_after_ignore_directive_scope() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
<div>
    {# ludtwig-ignore html-duplicate-id #}
    <span id="foo"></span>
</div>
<p id="foo"></p>"#,
            expect![[r#"
                warning[html-duplicate-id]: duplicate HTML element id attribute value
                  ┌─ ./debug-rule.html.twig:6:8
                  │
                1 │ <div id="foo"></div>
                  │          --- first defined here
                  ·
                6 │ <p id="foo"></p>
                  │        ^^^ duplicate id 'foo'

            "#]],
        );
    }

    #[test]
    fn rule_ignores_only_targeted_rule() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
{# ludtwig-ignore some-other-rule #}
<p id="foo"></p>"#,
            expect![[r#"
                warning[html-duplicate-id]: duplicate HTML element id attribute value
                  ┌─ ./debug-rule.html.twig:3:8
                  │
                1 │ <div id="foo"></div>
                  │          --- first defined here
                2 │ {# ludtwig-ignore some-other-rule #}
                3 │ <p id="foo"></p>
                  │        ^^^ duplicate id 'foo'

            "#]],
        );
    }

    #[test]
    fn rule_reports_multiple_duplicates() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="foo"></div>
<span id="foo"></span>
<p id="foo"></p>"#,
            expect![[r#"
                warning[html-duplicate-id]: duplicate HTML element id attribute value
                  ┌─ ./debug-rule.html.twig:2:11
                  │
                1 │ <div id="foo"></div>
                  │          --- first defined here
                2 │ <span id="foo"></span>
                  │           ^^^ duplicate id 'foo'

                warning[html-duplicate-id]: duplicate HTML element id attribute value
                  ┌─ ./debug-rule.html.twig:3:8
                  │
                1 │ <div id="foo"></div>
                  │          --- first defined here
                2 │ <span id="foo"></span>
                3 │ <p id="foo"></p>
                  │        ^^^ duplicate id 'foo'

            "#]],
        );
    }
}

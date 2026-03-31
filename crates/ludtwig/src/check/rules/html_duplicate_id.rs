use ludtwig_parser::syntax::typed::{AstNode, HtmlAttribute, LudtwigDirectiveIgnore};
use ludtwig_parser::syntax::untyped::{Preorder, SyntaxKind, SyntaxNode, WalkEvent};
use std::collections::HashMap;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleHtmlDuplicateId;

impl Rule for RuleHtmlDuplicateId {
    fn name(&self) -> &'static str {
        "html-duplicate-id"
    }

    fn check_root(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let mut id_table: HashMap<String, HtmlAttribute> = HashMap::new();

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

                    match id_table
                        .get(&id_text)
                        .and_then(|attr| attr.value()?.get_inner())
                    {
                        Some(first_inner) => {
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
                        _ => {
                            id_table.insert(id_text, attribute);
                        }
                    }
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

impl RuleHtmlDuplicateId {
    fn check_for_rule_ignore_enter(
        &self,
        is_ignored: &mut bool,
        tree_iter: &mut Preorder,
        n: &SyntaxNode,
    ) -> bool {
        if let Some(node) = n.prev_sibling() {
            if let Some(directive) = LudtwigDirectiveIgnore::cast(node) {
                let ignored_rules = directive.get_rules();
                if ignored_rules.is_empty() {
                    tree_iter.skip_subtree();
                    return true;
                }

                if ignored_rules.iter().any(|r| r == self.name()) {
                    *is_ignored = true;
                }
            }
        }
        false
    }

    fn check_for_rule_ignore_leave(&self, is_ignored: &mut bool, n: &SyntaxNode) {
        if let Some(node) = n.prev_sibling() {
            if let Some(directive) = LudtwigDirectiveIgnore::cast(node) {
                let ignored_rules = directive.get_rules();

                if ignored_rules.iter().any(|r| r == self.name()) {
                    *is_ignored = false;
                }
            }
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
    fn rule_does_not_report_dynamic_ids() {
        test_rule(
            "html-duplicate-id",
            r#"<div id="item-{{ id }}"></div>
<span id="item-{{ id }}"></span>"#,
            expect![""],
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

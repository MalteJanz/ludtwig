use ludtwig_parser::syntax::typed::{AstNode, LudtwigDirectiveIgnore, TwigBlock};
use ludtwig_parser::syntax::untyped::{Preorder, SyntaxKind, SyntaxNode, WalkEvent};
use std::collections::HashMap;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigBlockDuplicate;

impl Rule for RuleTwigBlockDuplicate {
    fn name(&self) -> &'static str {
        "twig-block-duplicate"
    }

    fn check_root(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        // keep track of some state during tree traversal
        let mut block_table: HashMap<String, TwigBlock> = HashMap::new();

        let mut is_ignored = false;
        let mut check_results = vec![];
        let mut tree_iter = node.preorder();
        while let Some(walk) = tree_iter.next() {
            match walk {
                WalkEvent::Enter(element) => {
                    if element.kind() == SyntaxKind::ERROR {
                        tree_iter.skip_subtree(); // Skip everything under error nodes!
                    }
                    if self.check_for_rule_ignore_enter(&mut is_ignored, &mut tree_iter, &element) {
                        continue;
                    }
                    if is_ignored {
                        continue;
                    }

                    let Some(block) = TwigBlock::cast(element) else {
                        continue;
                    };
                    let Some(name) = block.name() else {
                        continue;
                    };
                    if let Some(first_definition) =
                        block_table.get(name.text()).and_then(TwigBlock::name)
                    {
                        check_results.push(
                            self.create_result(Severity::Error, "block name duplicate")
                                .primary_note(
                                    name.text_range(),
                                    format!("duplicate block '{}'", name.text()),
                                )
                                .secondary_note(
                                    first_definition.text_range(),
                                    "first defined here",
                                ),
                        );
                    } else {
                        // found new unique block definition
                        block_table.insert(name.text().to_owned(), block);
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

impl RuleTwigBlockDuplicate {
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
                    // all rules are disabled
                    tree_iter.skip_subtree();
                    return true;
                }

                if ignored_rules.iter().any(|r| r == self.name()) {
                    // this rule is now ignored
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
                    // this rule is no longer ignored
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
    fn rule_reports() {
        test_rule(
            "twig-block-duplicate",
            r#"
            {% block foo %}
            {% endblock %}

            {% block first_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block second_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}"#,
            expect![[r#"
                error[twig-block-duplicate]: block name duplicate
                  ┌─ ./debug-rule.html.twig:6:26
                  │
                2 │             {% block foo %}
                  │                      --- first defined here
                  ·
                6 │                 {% block foo %}
                  │                          ^^^ duplicate block 'foo'

                error[twig-block-duplicate]: block name duplicate
                   ┌─ ./debug-rule.html.twig:11:26
                   │
                 2 │             {% block foo %}
                   │                      --- first defined here
                   ·
                11 │                 {% block foo %}
                   │                          ^^^ duplicate block 'foo'

            "#]],
        );
    }

    #[test]
    fn rule_does_not_report() {
        test_rule(
            "twig-block-duplicate",
            r#"
            {% block foo %}
            {% endblock %}

            {% block first_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block second_duplicate %}
                {# ludtwig-ignore twig-block-duplicate #}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block third_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}"#,
            expect![[r#"
                error[twig-block-duplicate]: block name duplicate
                  ┌─ ./debug-rule.html.twig:6:26
                  │
                2 │             {% block foo %}
                  │                      --- first defined here
                  ·
                6 │                 {% block foo %}
                  │                          ^^^ duplicate block 'foo'

                error[twig-block-duplicate]: block name duplicate
                   ┌─ ./debug-rule.html.twig:17:26
                   │
                 2 │             {% block foo %}
                   │                      --- first defined here
                   ·
                17 │                 {% block foo %}
                   │                          ^^^ duplicate block 'foo'

            "#]],
        );
    }
}

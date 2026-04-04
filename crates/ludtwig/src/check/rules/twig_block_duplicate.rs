use ludtwig_parser::syntax::typed::{AstNode, TwigBlock};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode, WalkEvent};
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
                    match block_table.get(name.text()).and_then(TwigBlock::name) {
                        Some(first_definition) => {
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
                        }
                        _ => {
                            // found new unique block definition
                            block_table.insert(name.text().to_owned(), block);
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::test_rule;

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-block-duplicate",
            r"
            {% block foo %}
            {% endblock %}

            {% block first_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block second_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}",
            expect![[r"
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

            "]],
        );
    }

    #[test]
    fn rule_ignores_with_specific_rule_directive() {
        test_rule(
            "twig-block-duplicate",
            r"
            {% block foo %}
            {% endblock %}

            {% block first_duplicate %}
                {# ludtwig-ignore twig-block-duplicate #}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block second_duplicate %}
                {# ludtwig-ignore twig-block-duplicate #}
                {% block foo %}
                {% endblock %}
            {% endblock %}",
            expect![""],
        );
    }

    #[test]
    fn rule_ignores_with_blanket_directive() {
        test_rule(
            "twig-block-duplicate",
            r"
            {% block foo %}
            {% endblock %}

            {% block wrapper %}
                {# ludtwig-ignore #}
                {% block foo %}
                {% endblock %}
            {% endblock %}",
            expect![""],
        );
    }

    #[test]
    fn rule_still_reports_after_ignore_directive_scope() {
        test_rule(
            "twig-block-duplicate",
            r"
            {% block foo %}
            {% endblock %}

            {% block first_duplicate %}
                {# ludtwig-ignore twig-block-duplicate #}
                {% block foo %}
                {% endblock %}
            {% endblock %}

            {% block third_duplicate %}
                {% block foo %}
                {% endblock %}
            {% endblock %}",
            expect![[r"
                error[twig-block-duplicate]: block name duplicate
                   ┌─ ./debug-rule.html.twig:12:26
                   │
                 2 │             {% block foo %}
                   │                      --- first defined here
                   ·
                12 │                 {% block foo %}
                   │                          ^^^ duplicate block 'foo'

            "]],
        );
    }

    #[test]
    fn rule_ignores_only_targeted_rule() {
        test_rule(
            "twig-block-duplicate",
            r"
            {% block foo %}
            {% endblock %}

            {% block wrapper %}
                {# ludtwig-ignore some-other-rule #}
                {% block foo %}
                {% endblock %}
            {% endblock %}",
            expect![[r"
                error[twig-block-duplicate]: block name duplicate
                  ┌─ ./debug-rule.html.twig:7:26
                  │
                2 │             {% block foo %}
                  │                      --- first defined here
                  ·
                7 │                 {% block foo %}
                  │                          ^^^ duplicate block 'foo'

            "]],
        );
    }
}

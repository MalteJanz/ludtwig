use ludtwig_parser::syntax::typed::{AstNode, TwigBlock};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode, WalkEvent};
use std::collections::HashMap;

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleDuplicateBlockNames;

impl Rule for RuleDuplicateBlockNames {
    fn name(&self) -> &'static str {
        "duplicate-block-names"
    }

    fn check_root(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        // keep track of some state during tree traversal
        let mut block_table: HashMap<String, TwigBlock> = HashMap::new();

        let mut check_results = vec![];
        let mut tree_iter = node.preorder();
        while let Some(walk) = tree_iter.next() {
            match walk {
                WalkEvent::Enter(element) => {
                    if element.kind() == SyntaxKind::ERROR {
                        tree_iter.skip_subtree(); // Skip everything under error nodes!
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
                                .secodary_note(first_definition.text_range(), "first defined here"),
                        );
                    }
                    block_table.insert(name.text().to_owned(), block);
                }
                WalkEvent::Leave(_element) => {}
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
            "duplicate-block-names",
            r#"{% block foo %}{% endblock %} {%block foo%} {% endblock %}"#,
            expect![[r#"
                error[duplicate-block-names]: block name duplicate
                  ┌─ ./debug-rule.html.twig:1:39
                  │
                1 │ {% block foo %}{% endblock %} {%block foo%} {% endblock %}
                  │          ---                          ^^^ duplicate block 'foo'
                  │          │                             
                  │          first defined here

            "#]],
        );
    }
}

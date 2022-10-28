use ludtwig_parser::syntax::typed::{AstNode, LudtwigDirectiveIgnore};
use ludtwig_parser::syntax::untyped::{
    SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize, WalkEvent,
};

use crate::check::rule::{Rule, RuleContext, Severity};

pub struct RuleIndentation;

impl Rule for RuleIndentation {
    fn name(&self) -> &'static str {
        "indentation"
    }

    fn check_root(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let mut line_break_encountered = true;
        let mut indentation = 0;
        let indent_block_children = ctx.config().format.indent_children_of_blocks;

        let mut is_ignored = false;
        let mut tree_iter = node.preorder_with_tokens();
        while let Some(walk) = tree_iter.next() {
            match walk {
                WalkEvent::Enter(element) => {
                    match element {
                        SyntaxElement::Node(n) if n.kind() == SyntaxKind::ERROR => {
                            tree_iter.skip_subtree(); // Skip everything under error nodes!
                        }
                        SyntaxElement::Token(t) if t.kind() == SyntaxKind::TK_LINE_BREAK => {
                            line_break_encountered = true;
                        }
                        SyntaxElement::Token(t) if !is_ignored && line_break_encountered => {
                            self.handle_first_token_in_line(t, indentation, ctx);
                            line_break_encountered = false;
                        }
                        SyntaxElement::Token(_) => {
                            // any other token encountered
                            line_break_encountered = false;
                        }
                        SyntaxElement::Node(n) => {
                            // rule ignore check
                            if let Some(node) = n.prev_sibling() {
                                if let Some(directive) = LudtwigDirectiveIgnore::cast(node) {
                                    let ignored_rules = directive.get_rules();
                                    if ignored_rules.is_empty() {
                                        // all rules are disabled
                                        tree_iter.skip_subtree();
                                        continue;
                                    }

                                    if ignored_rules.iter().any(|r| r == self.name()) {
                                        // this rule is now ignored
                                        is_ignored = true;
                                    }
                                }
                            }

                            if n.kind() == SyntaxKind::BODY
                                && (indent_block_children
                                    || !n
                                        .parent()
                                        .map_or(false, |p| p.kind() == SyntaxKind::TWIG_BLOCK))
                            {
                                indentation += 1;
                            }
                        }
                    }
                }
                WalkEvent::Leave(element) => {
                    if let SyntaxElement::Node(n) = element {
                        // rule ignore check
                        if let Some(node) = n.prev_sibling() {
                            if let Some(directive) = LudtwigDirectiveIgnore::cast(node) {
                                let ignored_rules = directive.get_rules();

                                if ignored_rules.iter().any(|r| r == self.name()) {
                                    // this rule is no longer ignored
                                    is_ignored = false;
                                }
                            }
                        }

                        if n.kind() == SyntaxKind::BODY
                            && (indent_block_children
                                || !n
                                    .parent()
                                    .map_or(false, |p| p.kind() == SyntaxKind::TWIG_BLOCK))
                        {
                            indentation -= 1;
                        }
                    }
                }
            }
        }

        None
    }
}

impl RuleIndentation {
    fn handle_first_token_in_line(
        &self,
        token: SyntaxToken,
        indentation: usize,
        ctx: &mut RuleContext,
    ) {
        let indent_char = ctx.config().format.indentation_mode.corresponding_char();
        let indent_char_count = ctx.config().format.indentation_count;
        let expected_str = std::iter::repeat(indent_char)
            .take(indentation * indent_char_count as usize)
            .collect::<String>();

        match token.kind() {
            SyntaxKind::TK_WHITESPACE => {
                if token.text() != expected_str {
                    // report wrong indentation
                    let result = ctx
                        .create_result(self.name(), Severity::Help, "Wrong indentation")
                        .primary_note(
                            token.text_range(),
                            format!(
                                "Expected indentation of {} {} here",
                                indentation * indent_char_count as usize,
                                ctx.config().format.indentation_mode
                            ),
                        )
                        .suggestion(
                            token.text_range(),
                            expected_str,
                            format!(
                                "Change indentation to {} {}",
                                indentation * indent_char_count as usize,
                                ctx.config().format.indentation_mode
                            ),
                        );
                    ctx.add_result(result);
                }
            }
            _ => {
                if indentation > 0 {
                    // report missing whitespace token
                    let range = TextRange::at(token.text_range().start(), TextSize::from(0));
                    let result = ctx
                        .create_result(self.name(), Severity::Help, "Missing indentation")
                        .primary_note(
                            range,
                            format!(
                                "Expected indentation of {} {} before this",
                                indentation * indent_char_count as usize,
                                ctx.config().format.indentation_mode
                            ),
                        )
                        .suggestion(
                            range,
                            expected_str,
                            format!(
                                "Add {} {} indentation",
                                indentation * indent_char_count as usize,
                                ctx.config().format.indentation_mode
                            ),
                        );
                    ctx.add_result(result);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    #[test]
    fn rule_reports() {
        test_rule(
            "indentation",
            r#"{% block outer %}
                <div>
        inner
                      </div>
                  {% endblock %}"#,
            expect![[r#"
                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:2:1
                  │
                2 │                 <div>
                  │ ^^^^^^^^^^^^^^^^
                  │ │
                  │ Expected indentation of 4 spaces here
                  │ Change indentation to 4 spaces:     

                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:4:1
                  │
                4 │                       </div>
                  │ ^^^^^^^^^^^^^^^^^^^^^^
                  │ │
                  │ Expected indentation of 4 spaces here
                  │ Change indentation to 4 spaces:     

                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:5:1
                  │
                5 │                   {% endblock %}
                  │ ^^^^^^^^^^^^^^^^^^
                  │ │
                  │ Expected indentation of 0 spaces here
                  │ Change indentation to 0 spaces: 

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "indentation",
            r#"{% block outer %}
                <div>
        inner
                      </div>
                  {% endblock %}"#,
            expect![[r#"
                {% block outer %}
                    <div>
                        inner
                    </div>
                {% endblock %}"#]],
        );
    }

    #[test]
    fn rule_ignores() {
        test_rule(
            "indentation",
            r#"{% block outer %}
    {# ludtwig-ignore indentation #}
    <div>
    inner
    </div>
{% endblock %}"#,
            expect![[r#""#]],
        );
    }
}

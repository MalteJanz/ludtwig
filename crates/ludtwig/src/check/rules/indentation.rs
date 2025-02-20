use ludtwig_parser::syntax::typed::{AstNode, HtmlStartingTag, HtmlTag, LudtwigDirectiveIgnore};
use ludtwig_parser::syntax::untyped::{
    PreorderWithTokens, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange, TextSize,
    WalkEvent,
};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleIndentation;

impl Rule for RuleIndentation {
    fn name(&self) -> &'static str {
        "indentation"
    }

    fn check_root(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        // keep track of some state during tree traversal
        let mut line_break_encountered = true;
        let mut indentation_level = 0; // whole indentation levels like nested elements
        let mut indentation_substeps = 0; // additional spaces for alignment (like attributes)
        let mut inside_trivia_sensitive_node = false;
        let mut is_ignored = false;

        let indent_block_children = ctx.config().format.indent_children_of_blocks;

        let mut check_results = vec![];
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
                            if !inside_trivia_sensitive_node {
                                check_results.append(&mut self.handle_first_token_in_line(
                                    &t,
                                    indentation_level,
                                    indentation_substeps,
                                    ctx,
                                ));
                            }
                            line_break_encountered = false;
                        }
                        SyntaxElement::Token(_) => {
                            // any other token encountered
                            line_break_encountered = false;
                        }
                        SyntaxElement::Node(n) => {
                            if self.check_for_rule_ignore_enter(&mut is_ignored, &mut tree_iter, &n)
                            {
                                continue;
                            }

                            Self::check_for_trivia_sensitivity(
                                &mut inside_trivia_sensitive_node,
                                &n,
                                WalkMode::Enter,
                            );

                            Self::check_indentation_level(
                                &mut indentation_level,
                                indent_block_children,
                                &n,
                                WalkMode::Enter,
                            );

                            Self::check_indentation_substeps(
                                &mut indentation_substeps,
                                &n,
                                WalkMode::Enter,
                            );
                        }
                    }
                }
                WalkEvent::Leave(element) => {
                    if let SyntaxElement::Node(n) = element {
                        self.check_for_rule_ignore_leave(&mut is_ignored, &n);

                        Self::check_for_trivia_sensitivity(
                            &mut inside_trivia_sensitive_node,
                            &n,
                            WalkMode::Leave,
                        );

                        Self::check_indentation_level(
                            &mut indentation_level,
                            indent_block_children,
                            &n,
                            WalkMode::Leave,
                        );

                        Self::check_indentation_substeps(
                            &mut indentation_substeps,
                            &n,
                            WalkMode::Leave,
                        );
                    }
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

#[derive(Debug, Copy, Clone)]
enum WalkMode {
    Enter,
    Leave,
}

impl RuleIndentation {
    fn handle_first_token_in_line(
        &self,
        token: &SyntaxToken,
        indentation_level: usize,
        indentation_substeps: usize,
        ctx: &RuleRunContext,
    ) -> Vec<CheckResult> {
        let indent_char = ctx.config().format.indentation_mode.corresponding_char();
        let indent_char_count = ctx.config().format.indentation_count;
        let expected_str = std::iter::repeat(indent_char)
            .take(indentation_level * indent_char_count as usize)
            .chain(" ".repeat(indentation_substeps).chars())
            .collect::<String>();

        let substeps_expectation_notice = if indentation_substeps > 0 {
            format!(" (+{indentation_substeps} spaces)")
        } else {
            String::new()
        };

        match token.kind() {
            SyntaxKind::TK_WHITESPACE => {
                if token.text() != expected_str {
                    // get information about actual indentation
                    let (found_spaces, found_tabs) = get_spaces_and_tabs_count(token.text());

                    // report wrong indentation
                    let result = self
                        .create_result( Severity::Help, "Wrong indentation")
                        .primary_note(
                            token.text_range(),
                            format!(
                                "Found {} spaces and {} tabs but expected indentation of {} {}{} here",
                                found_spaces,
                                found_tabs,
                                indentation_level * indent_char_count as usize,
                                ctx.config().format.indentation_mode,
                                substeps_expectation_notice,
                            ),
                        )
                        .suggestion(
                            token.text_range(),
                            expected_str,
                            format!(
                                "Change indentation to {} {}{}",
                                indentation_level * indent_char_count as usize,
                                ctx.config().format.indentation_mode,
                                substeps_expectation_notice,
                            ),
                        );
                    return vec![result];
                }
            }
            _ => {
                if indentation_level > 0 || indentation_substeps > 0 {
                    // report missing whitespace token
                    let range = TextRange::at(token.text_range().start(), TextSize::from(0));
                    let result = self
                        .create_result(Severity::Help, "Missing indentation")
                        .primary_note(
                            range,
                            format!(
                                "Expected indentation of {} {}{} before this",
                                indentation_level * indent_char_count as usize,
                                ctx.config().format.indentation_mode,
                                substeps_expectation_notice,
                            ),
                        )
                        .suggestion(
                            range,
                            expected_str,
                            format!(
                                "Add {} {}{} indentation",
                                indentation_level * indent_char_count as usize,
                                ctx.config().format.indentation_mode,
                                substeps_expectation_notice,
                            ),
                        );
                    return vec![result];
                }
            }
        }

        vec![]
    }

    fn check_for_rule_ignore_enter(
        &self,
        is_ignored: &mut bool,
        tree_iter: &mut PreorderWithTokens,
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

    fn check_for_trivia_sensitivity(
        inside_trivia_sensitive_node: &mut bool,
        n: &SyntaxNode,
        walk_mode: WalkMode,
    ) {
        if let Some(t) = HtmlTag::cast(n.clone()) {
            if let Some("pre" | "textarea") = t.name().as_ref().map(SyntaxToken::text) {
                match walk_mode {
                    WalkMode::Enter => {
                        *inside_trivia_sensitive_node = true;
                    }
                    WalkMode::Leave => {
                        *inside_trivia_sensitive_node = false;
                    }
                }
            }
        }
    }

    fn check_indentation_level(
        indentation_level: &mut usize,
        indent_block_children: bool,
        n: &SyntaxNode,
        walk_mode: WalkMode,
    ) {
        if matches!(
            n.kind(),
            SyntaxKind::BODY
                | SyntaxKind::TWIG_ARGUMENTS
                | SyntaxKind::TWIG_LITERAL_ARRAY_INNER
                | SyntaxKind::TWIG_LITERAL_HASH_ITEMS
        ) && (indent_block_children
            || n.parent()
                .is_none_or(|p| p.kind() != SyntaxKind::TWIG_BLOCK))
        {
            match walk_mode {
                WalkMode::Enter => {
                    *indentation_level += 1;
                }
                WalkMode::Leave => {
                    *indentation_level -= 1;
                }
            }
        }
    }

    fn check_indentation_substeps(
        indentation_substeps: &mut usize,
        n: &SyntaxNode,
        walk_mode: WalkMode,
    ) {
        if n.kind() == SyntaxKind::HTML_ATTRIBUTE_LIST {
            if let Some(t) = n.parent().and_then(HtmlStartingTag::cast) {
                if let Some(name) = t.name() {
                    let adjustment = 1 + name.text().chars().count() + 1;

                    match walk_mode {
                        WalkMode::Enter => {
                            *indentation_substeps += adjustment;
                        }
                        WalkMode::Leave => {
                            *indentation_substeps -= adjustment;
                        }
                    }
                }
            }
        }
    }
}

fn get_spaces_and_tabs_count(input: &str) -> (i32, i32) {
    input.chars().fold((0, 0), |(mut spaces, mut tabs), c| {
        match c {
            ' ' => spaces += 1,
            '\t' => tabs += 1,
            _ => {}
        }

        (spaces, tabs)
    })
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    #[test]
    fn rule_reports() {
        test_rule(
            "indentation",
            r"{% block outer %}
                <div>
        inner
                      </div>
                  {% endblock %}",
            expect![[r"
                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:2:1
                  │
                2 │                 <div>
                  │ ^^^^^^^^^^^^^^^^
                  │ │
                  │ Found 16 spaces and 0 tabs but expected indentation of 4 spaces here
                  │ Change indentation to 4 spaces:     

                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:4:1
                  │
                4 │                       </div>
                  │ ^^^^^^^^^^^^^^^^^^^^^^
                  │ │
                  │ Found 22 spaces and 0 tabs but expected indentation of 4 spaces here
                  │ Change indentation to 4 spaces:     

                help[indentation]: Wrong indentation
                  ┌─ ./debug-rule.html.twig:5:1
                  │
                5 │                   {% endblock %}
                  │ ^^^^^^^^^^^^^^^^^^
                  │ │
                  │ Found 18 spaces and 0 tabs but expected indentation of 0 spaces here
                  │ Change indentation to 0 spaces: 

            "]],
        );
    }

    #[test]
    fn rule_does_not_report_trivia_sensitive() {
        test_rule(
            "indentation",
            r"<pre>
        hello
        
        
        world
            </pre>
            <pre>
                <code>
        hello
        
        
        world
                </code>
            </pre>
            <textarea>
    hello
    
    
    world
            </textarea>
            <textarea>
                <p>
    hello
    
    
    world
                </p>
            </textarea>
    <div>
        wrong
</div>",
            expect![[r"
                help[indentation]: Wrong indentation
                   ┌─ ./debug-rule.html.twig:29:1
                   │
                29 │     <div>
                   │ ^^^^
                   │ │
                   │ Found 4 spaces and 0 tabs but expected indentation of 0 spaces here
                   │ Change indentation to 0 spaces: 

                help[indentation]: Wrong indentation
                   ┌─ ./debug-rule.html.twig:30:1
                   │
                30 │         wrong
                   │ ^^^^^^^^
                   │ │
                   │ Found 8 spaces and 0 tabs but expected indentation of 4 spaces here
                   │ Change indentation to 4 spaces:     

            "]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "indentation",
            r"{% block outer %}
                <div>
        inner
                      </div>
                  {% endblock %}",
            expect![[r"
                {% block outer %}
                    <div>
                        inner
                    </div>
                {% endblock %}"]],
        );
    }

    #[test]
    fn rule_fixes_attribute_indentation() {
        test_rule_fix(
            "indentation",
            r#"<div id="my-div"
            class="some-class"
            style="background-color: red">
                hello world
            </div>
            "#,
            expect![[r#"
                <div id="my-div"
                     class="some-class"
                     style="background-color: red">
                    hello world
                </div>
            "#]],
        );
    }

    #[test]
    fn rule_ignores() {
        test_rule(
            "indentation",
            r"{% block outer %}
    {# ludtwig-ignore indentation #}
    <div>
    inner
    </div>
{% endblock %}",
            expect![[r""]],
        );
    }
}

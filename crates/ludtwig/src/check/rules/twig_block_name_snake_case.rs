use ludtwig_parser::syntax::typed::{AstNode, TwigStartingBlock};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{Rule, RuleContext, Severity};
use crate::Config;

pub struct RuleTwigBlockNameSnakeCase;

impl Rule for RuleTwigBlockNameSnakeCase {
    fn new(_config: &Config) -> Self {
        Self
    }

    fn name(&self) -> &'static str {
        "twig-block-name-snake-case"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let block_name = TwigStartingBlock::cast(node)?.name()?;
        if !is_valid_ascii_alpha_snake_case(block_name.text()) {
            // name is not valid ascii snake case
            let mut result = ctx
                .create_result(
                    self.name(),
                    Severity::Warning,
                    "Block name is not written in snake_case",
                )
                .primary_note(
                    block_name.text_range(),
                    "help: rename this block in snake_case",
                );

            // try make a suggestion
            if let Some(suggested_name) = try_make_snake_case(block_name.text()) {
                result = result.suggestion(
                    block_name.text_range(),
                    suggested_name,
                    "Try this name instead",
                );
            }

            ctx.add_result(result);
        }
        None
    }
}

fn is_valid_ascii_alpha_snake_case(s: &str) -> bool {
    let mut iter = s.chars().enumerate().peekable();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // first or last should not be an underline
        if (idx == 0 || next.is_none()) && c == '_' {
            return false;
        }

        // chars must be ascii lowercase or an underline
        if !c.is_ascii_lowercase() && c != '_' {
            return false;
        }

        // no two underlines next to each other
        if let Some((_, next)) = next {
            if c == '_' && *next == '_' {
                return false;
            }
        }
    }

    true
}

fn try_make_snake_case(original: &str) -> Option<String> {
    let mut iter = original.chars().enumerate().peekable();
    let mut attempt = String::new();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // special first or last character
        if idx == 0 || next.is_none() {
            // skip minus or underline at the start and end
            if c == '-' || c == '_' {
                continue;
            }

            // first or last uppercase should not be pretended with underline
            if c.is_ascii_uppercase() {
                attempt.push(c.to_ascii_lowercase());
                continue;
            }
        }

        if let Some((_, next)) = next {
            if (c == '-' || c == '_') && (*next == '_' || *next == '-' || next.is_ascii_uppercase())
            {
                continue; // next will already place an underline
            }
        }

        // replace all minus with underline
        if c == '-' {
            attempt.push('_');
            continue;
        }

        // make an underline before each uppercase and replace it with lowercase
        if c.is_ascii_uppercase() {
            attempt.push('_');
        }

        attempt.push(c.to_ascii_lowercase());
    }

    // validate suggestion
    if is_valid_ascii_alpha_snake_case(&attempt) {
        return Some(attempt);
    }

    None
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_fix};

    use super::*;

    #[test]
    fn test_is_ascii_alpha_snake_case() {
        assert!(is_valid_ascii_alpha_snake_case("my_block"));
        assert!(is_valid_ascii_alpha_snake_case("my_b_l_o_c_k"));

        assert!(!is_valid_ascii_alpha_snake_case("my-block"));
        assert!(!is_valid_ascii_alpha_snake_case("myBlock"));
        assert!(!is_valid_ascii_alpha_snake_case("MyBlock"));
        assert!(!is_valid_ascii_alpha_snake_case("my__block"));
        assert!(!is_valid_ascii_alpha_snake_case("_my_block"));
        assert!(!is_valid_ascii_alpha_snake_case("__my_block"));
        assert!(!is_valid_ascii_alpha_snake_case("my_block_"));
        assert!(!is_valid_ascii_alpha_snake_case("my_block__"));
        assert!(!is_valid_ascii_alpha_snake_case("__my_block__"));
    }

    #[test]
    fn test_try_make_snake_case() {
        assert_eq!(
            try_make_snake_case("my-kebab-case-block"),
            Some("my_kebab_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("myCamelCaseBlock"),
            Some("my_camel_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("MyPascalCaseBlock"),
            Some("my_pascal_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("myStrange-Block"),
            Some("my_strange_block".to_string())
        );
        assert_eq!(try_make_snake_case("_My-Broken_-Block_"), None);
    }

    #[test]
    fn rule_reports() {
        test_rule(
            "twig-block-name-snake-case",
            "{% block a-b %}hello{% endblock %}",
            expect![[r#"
                warning[twig-block-name-snake-case]: Block name is not written in snake_case
                  ┌─ ./debug-rule.html.twig:1:10
                  │
                1 │ {% block a-b %}hello{% endblock %}
                  │          ^^^
                  │          │
                  │          help: rename this block in snake_case
                  │          Try this name instead: a_b

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "twig-block-name-snake-case",
            "{% block a-b %}hello{% endblock %}",
            expect!["{% block a_b %}hello{% endblock %}"],
        );
    }
}

use ludtwig_parser::syntax::typed::{AstNode, HtmlAttribute};
use ludtwig_parser::syntax::untyped::SyntaxNode;

use crate::check::rule::{Rule, RuleContext, Severity};

#[derive(Debug, Clone)]
pub struct RuleHtmlAttributeNameKebabCase;

impl Rule for RuleHtmlAttributeNameKebabCase {
    fn name(&self) -> &'static str {
        "html-attribute-name-kebab-case"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        let attribute_name = HtmlAttribute::cast(node)?.name()?;
        if !is_valid_alphanumeric_kebab_case(attribute_name.text()) {
            // name is not valid
            let mut result = ctx
                .create_result(
                    self.name(),
                    Severity::Warning,
                    "Attribute name is not written in kebab-case",
                )
                .primary_note(
                    attribute_name.text_range(),
                    "help: rename this attribute in kebab-case",
                );

            // try make a suggestion
            if let Some(suggested_name) = try_make_kebab_case(attribute_name.text()) {
                result = result.suggestion(
                    attribute_name.text_range(),
                    suggested_name,
                    "Try this name instead",
                );
            }

            ctx.add_result(result);
        }
        None
    }
}

fn is_valid_alphanumeric_kebab_case(s: &str) -> bool {
    let mut iter = s.chars().enumerate().peekable();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // first or last should not be an minus
        if (idx == 0 || next.is_none()) && c == '-' {
            return false;
        }

        if idx == 0 {
            // special rule for first characters
            if !(c.is_ascii_lowercase() || c.is_ascii_digit() || [':', '@', '#'].contains(&c)) {
                return false;
            }
        } else {
            // everything else must be ascii lowercase or minus
            if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-') {
                return false;
            }
        }

        // no two minus next to each other
        if let Some((_, next)) = next {
            if c == '-' && *next == '-' {
                return false;
            }
        }
    }

    true
}

fn try_make_kebab_case(original: &str) -> Option<String> {
    let mut iter = original.chars().enumerate().peekable();
    let mut attempt = String::new();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // special first or last character
        if idx == 0 || next.is_none() {
            // skip minus or underline at the start and end
            if ['-', '_'].contains(&c) {
                continue;
            }

            // first uppercase should not be pretended with minus
            if idx == 0 && c.is_ascii_uppercase() {
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

        // replace all underline with minus
        if c == '_' {
            attempt.push('-');
            continue;
        }

        // make an minus before each uppercase and replace it with lowercase
        if c.is_ascii_uppercase() {
            attempt.push('-');
        }

        attempt.push(c.to_ascii_lowercase());
    }

    // validate suggestion
    if is_valid_alphanumeric_kebab_case(&attempt) {
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
    fn test_is_valid_alphanumeric_kebab_case() {
        assert!(is_valid_alphanumeric_kebab_case("my-attribute"));
        assert!(is_valid_alphanumeric_kebab_case("h1"));
        assert!(is_valid_alphanumeric_kebab_case(":vue-bound"));
        assert!(is_valid_alphanumeric_kebab_case("@vue-event"));
        assert!(is_valid_alphanumeric_kebab_case("#vue-slot"));

        assert!(!is_valid_alphanumeric_kebab_case("my--attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("-my-attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("my-attribute-"));
        assert!(!is_valid_alphanumeric_kebab_case("my-attribute--"));
        assert!(!is_valid_alphanumeric_kebab_case("--my-attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("myAttribute"));
        assert!(!is_valid_alphanumeric_kebab_case("my_attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("myA"));
    }

    #[test]
    fn test_try_make_kebab_case() {
        assert_eq!(
            try_make_kebab_case("my_snake_case_block"),
            Some("my-snake-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("myCamelCaseBlock"),
            Some("my-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("MyPascalCaseBlock"),
            Some("my-pascal-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case(":myVueCamelCaseBlock"),
            Some(":my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("@myVueCamelCaseBlock"),
            Some("@my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("#myVueCamelCaseBlock"),
            Some("#my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("myStrange-Block"),
            Some("my-strange-block".to_string())
        );
        assert_eq!(try_make_kebab_case("myA"), Some("my-a".to_string()));
        assert_eq!(try_make_kebab_case("my-"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("my_"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("-my"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("_my"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("_My-Broken_-Block_"), None);
    }

    #[test]
    fn rule_reports() {
        test_rule(
            "html-attribute-name-kebab-case",
            "<custom aBc/>",
            expect![[r#"
                warning[html-attribute-name-kebab-case]: Attribute name is not written in kebab-case
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ <custom aBc/>
                  │         ^^^
                  │         │
                  │         help: rename this attribute in kebab-case
                  │         Try this name instead: a-bc

            "#]],
        );
    }

    #[test]
    fn rule_fixes() {
        test_rule_fix(
            "html-attribute-name-kebab-case",
            "<custom aBc/>",
            expect!["<custom a-bc/>"],
        );
    }
}

use crate::check::naming_convention::{is_valid_alphanumeric_kebab_case, try_make_kebab_case};
use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};
use ludtwig_parser::syntax::typed::{AstNode, HtmlAttribute};
use ludtwig_parser::syntax::untyped::SyntaxNode;

pub struct RuleHtmlAttributeNameKebabCase;

impl Rule for RuleHtmlAttributeNameKebabCase {
    fn name(&self) -> &'static str {
        "html-attribute-name-kebab-case"
    }

    fn check_node(&self, node: SyntaxNode, _ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let attribute = HtmlAttribute::cast(node)?;
        let attribute_name = attribute.name()?;

        if attribute.html_tag()?.is_twig_component() {
            return None; // skip this rule for twig components, because they often use camelCase as params
        }

        if !is_valid_alphanumeric_kebab_case(attribute_name.text()) {
            // name is not valid
            let mut result = self
                .create_result(
                    Severity::Help,
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

            return Some(vec![result]);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check::rules::test::{test_rule, test_rule_does_not_fix, test_rule_fix};

    #[test]
    fn rule_reports() {
        test_rule(
            "html-attribute-name-kebab-case",
            "<custom aBc/>",
            expect![[r"
                help[html-attribute-name-kebab-case]: Attribute name is not written in kebab-case
                  ┌─ ./debug-rule.html.twig:1:9
                  │
                1 │ <custom aBc/>
                  │         ^^^
                  │         │
                  │         help: rename this attribute in kebab-case
                  │         Try this name instead: a-bc

            "]],
        );
    }

    #[test]
    fn rule_does_not_report() {
        test_rule_does_not_fix(
            "html-attribute-name-kebab-case",
            r#"<twig:namespaced:component :myCamelCaseAttribute="asdf"/>"#,
            expect![[r#"<twig:namespaced:component :myCamelCaseAttribute="asdf"/>"#]],
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

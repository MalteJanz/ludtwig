use ludtwig_parser::T;
use ludtwig_parser::syntax::typed::{
    AstNode, TwigBinaryExpression, TwigComment, TwigExpression, TwigFunctionCall, TwigLiteralName,
    TwigLiteralString, support,
};
use ludtwig_parser::syntax::untyped::{SyntaxKind, SyntaxNode};

use crate::check::rule::{CheckResult, Rule, RuleExt, RuleRunContext, Severity};

pub struct RuleTwigDeprecatedFeatureGuard;

impl Rule for RuleTwigDeprecatedFeatureGuard {
    fn name(&self) -> &'static str {
        "twig-deprecated-feature-guard"
    }

    fn check_node(&self, node: SyntaxNode, ctx: &RuleRunContext) -> Option<Vec<CheckResult>> {
        let comment = TwigComment::cast(node)?;
        let text = comment.syntax().text().to_string();
        let mut words = text.split_whitespace();
        words.find(|word| *word == "@deprecated")?;
        let tag = words.next()?.strip_prefix("tag:")?;
        let flag = ctx.config().twig.deprecated_feature_flags.get(tag)?;

        if comment
            .syntax()
            .ancestors()
            .filter(|ancestor| ancestor.kind() == SyntaxKind::BODY)
            .any(|body| excludes_enabled_flag(&body, flag))
        {
            return None;
        }

        Some(vec![
            self.create_result(
                Severity::Error,
                "deprecated Twig code is not guarded by its feature flag",
            )
            .primary_note(
                comment.syntax().text_range(),
                format!("place this annotation in a branch where feature('{flag}') is false"),
            ),
        ])
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Truth {
    True,
    False,
    Unknown,
}

impl Truth {
    fn negate(self) -> Self {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Unknown => Self::Unknown,
        }
    }

    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::False, _) | (_, Self::False) => Self::False,
            (Self::True, Self::True) => Self::True,
            _ => Self::Unknown,
        }
    }

    fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::True, _) | (_, Self::True) => Self::True,
            (Self::False, Self::False) => Self::False,
            _ => Self::Unknown,
        }
    }
}

// Evaluate a condition assuming this feature flag is enabled. Unknown values may be either true
// or false; only a proven contradiction can establish that a branch is guarded.
fn condition_truth(node: SyntaxNode, flag: &str) -> Truth {
    match node.kind() {
        SyntaxKind::TWIG_EXPRESSION | SyntaxKind::TWIG_PARENTHESES_EXPRESSION => node
            .children()
            .next()
            .map_or(Truth::Unknown, |child| condition_truth(child, flag)),
        SyntaxKind::TWIG_UNARY_EXPRESSION => {
            if !node.children_with_tokens().any(|element| {
                element
                    .into_token()
                    .is_some_and(|token| token.kind() == T!["not"])
            }) {
                return Truth::Unknown;
            }

            node.children().next().map_or(Truth::Unknown, |child| {
                condition_truth(child, flag).negate()
            })
        }
        SyntaxKind::TWIG_BINARY_EXPRESSION => {
            let Some(binary) = TwigBinaryExpression::cast(node) else {
                return Truth::Unknown;
            };
            let (Some(lhs), Some(rhs), Some(operator)) = (
                binary.lhs_expression(),
                binary.rhs_expression(),
                binary.operator(),
            ) else {
                return Truth::Unknown;
            };
            let lhs = condition_truth(lhs.syntax().clone(), flag);
            let rhs = condition_truth(rhs.syntax().clone(), flag);

            match operator.kind() {
                T!["and"] => lhs.and(rhs),
                T!["or"] => lhs.or(rhs),
                _ => Truth::Unknown,
            }
        }
        SyntaxKind::TWIG_FUNCTION_CALL => {
            let Some(call) = TwigFunctionCall::cast(node) else {
                return Truth::Unknown;
            };
            let Some(name) = call
                .name_operand()
                .and_then(|operand| support::child::<TwigLiteralName>(operand.syntax()))
                .and_then(|name| name.get_name())
            else {
                return Truth::Unknown;
            };
            if name.text() != "feature" {
                return Truth::Unknown;
            }

            let Some(arguments) = call.arguments() else {
                return Truth::Unknown;
            };
            let mut children = arguments.syntax().children();
            let Some(argument) = children.next().and_then(TwigExpression::cast) else {
                return Truth::Unknown;
            };
            if children.next().is_some() {
                return Truth::Unknown;
            }
            let Some(value) = support::child::<TwigLiteralString>(argument.syntax()) else {
                return Truth::Unknown;
            };
            let Some(inner) = value.get_inner() else {
                return Truth::Unknown;
            };
            if inner.syntax().children().next().is_some() {
                return Truth::Unknown;
            }

            if inner.syntax().text() == flag {
                Truth::True
            } else {
                Truth::Unknown
            }
        }
        SyntaxKind::TWIG_LITERAL_BOOLEAN => match node.text().to_string().trim() {
            "true" => Truth::True,
            "false" => Truth::False,
            _ => Truth::Unknown,
        },
        _ => Truth::Unknown,
    }
}

// TwigIf contains alternating branch headers and BODY siblings, not BODY children of each header.
fn excludes_enabled_flag(body: &SyntaxNode, flag: &str) -> bool {
    let Some(parent) = body
        .parent()
        .filter(|parent| parent.kind() == SyntaxKind::TWIG_IF)
    else {
        return false;
    };

    let mut can_reach_next_branch = true;
    let mut condition = Truth::Unknown;
    for child in parent.children() {
        match child.kind() {
            SyntaxKind::TWIG_IF_BLOCK | SyntaxKind::TWIG_ELSE_IF_BLOCK => {
                condition = child
                    .children()
                    .find_map(TwigExpression::cast)
                    .map_or(Truth::Unknown, |expr| {
                        condition_truth(expr.syntax().clone(), flag)
                    });
            }
            SyntaxKind::TWIG_ELSE_BLOCK => condition = Truth::True,
            SyntaxKind::BODY => {
                if child == *body {
                    return !can_reach_next_branch || condition == Truth::False;
                }
                if condition == Truth::True {
                    can_reach_next_branch = false;
                }
            }
            _ => {}
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::Config;
    use crate::check::rules::test::test_rule_with_config;

    fn config() -> Config {
        let mut config = Config::new(crate::config::DEFAULT_CONFIG_PATH).unwrap();
        config
            .twig
            .deprecated_feature_flags
            .insert("v6.8.0".into(), "v6.8.0.0".into());
        config
    }

    #[test]
    fn reports_unguarded_annotation() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{# @deprecated tag:v6.8.0 - old output #}",
            config(),
            expect![[r"
                error[twig-deprecated-feature-guard]: deprecated Twig code is not guarded by its feature flag
                  ┌─ ./debug-rule.html.twig:1:1
                  │
                1 │ {# @deprecated tag:v6.8.0 - old output #}
                  │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ place this annotation in a branch where feature('v6.8.0.0') is false

            "]],
        );
    }

    #[test]
    fn accepts_direct_and_nested_guard() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if not feature('v6.8.0.0') %}{% if foo %}{# @deprecated tag:v6.8.0 #}{% endif %}{% endif %}",
            config(),
            expect![""],
        );
    }

    #[test]
    fn accepts_else_of_enabled_flag() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if feature('v6.8.0.0') %}new{% else %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![""],
        );
    }

    #[test]
    fn rejects_enabled_branch() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if feature('v6.8.0.0') %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![[r"
                error[twig-deprecated-feature-guard]: deprecated Twig code is not guarded by its feature flag
                  ┌─ ./debug-rule.html.twig:1:29
                  │
                1 │ {% if feature('v6.8.0.0') %}{# @deprecated tag:v6.8.0 #}{% endif %}
                  │                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ place this annotation in a branch where feature('v6.8.0.0') is false

            "]],
        );
    }

    #[test]
    fn accepts_guarded_conjunction_but_not_disjunction() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if not feature('v6.8.0.0') and foo %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![""],
        );
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if not feature('v6.8.0.0') or foo %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![[r"
                error[twig-deprecated-feature-guard]: deprecated Twig code is not guarded by its feature flag
                  ┌─ ./debug-rule.html.twig:1:40
                  │
                1 │ {% if not feature('v6.8.0.0') or foo %}{# @deprecated tag:v6.8.0 #}{% endif %}
                  │                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ place this annotation in a branch where feature('v6.8.0.0') is false

            "]],
        );
    }

    #[test]
    fn accepts_unreachable_elseif_for_enabled_flag() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if feature('v6.8.0.0') %}new{% elseif foo %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![""],
        );
    }

    #[test]
    fn accepts_negated_flag_in_elseif() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{% if foo %}new{% elseif not feature('v6.8.0.0') %}{# @deprecated tag:v6.8.0 #}{% endif %}",
            config(),
            expect![""],
        );
    }

    #[test]
    fn ignores_unconfigured_deprecations() {
        test_rule_with_config(
            "twig-deprecated-feature-guard",
            "{# @deprecated tag:v6.9.0 #}",
            config(),
            expect![""],
        );
    }
}

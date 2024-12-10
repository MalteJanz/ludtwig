//! This module contains all abstract syntax tree (AST) types.
//! All of them implement the [AstNode] trait.
//!
//! Some of them come with extra utility methods, to quickly access some data
//! (e.g. [TwigBlock::name]).
//!
//! An overview of the syntax tree concept can be found
//! at the [crate level documentation](crate#syntax-trees).

pub use rowan::ast::support;
pub use rowan::ast::AstChildren;
pub use rowan::ast::AstNode;
use rowan::NodeOrToken;
use std::fmt::{Debug, Display, Formatter};

use crate::T;

use super::untyped::{
    debug_tree, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TemplateLanguage,
};

/// So far, we've been working with a homogeneous untyped tree.
/// It's nice to provide generic tree operations, like traversals,
/// but it's a bad fit for semantic analysis.
/// The rowan crate itself does not provide AST facilities directly,
/// but it is possible to layer AST on top of `SyntaxNode` API.
///
/// Let's define AST nodes.
/// It'll be quite a bunch of repetitive code, so we'll use a macro.
///
/// For a real language, you'd want to generate an AST. I find a
/// combination of `serde`, `ron` and `tera` crates invaluable for that!
macro_rules! ast_node {
    ($ast:ident, $kind:path) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $ast {
            pub(crate) syntax: SyntaxNode,
        }

        impl AstNode for $ast {
            type Language = TemplateLanguage;

            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
            where
                Self: Sized,
            {
                kind == $kind
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
            where
                Self: Sized,
            {
                if Self::can_cast(node.kind()) {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                &self.syntax
            }
        }

        impl Display for $ast {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.syntax)?;
                Ok(())
            }
        }

        impl Debug for $ast {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", debug_tree(&self.syntax))?;
                Ok(())
            }
        }
    };
}

ast_node!(TwigBlock, SyntaxKind::TWIG_BLOCK);
impl TwigBlock {
    /// Name of the twig block
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        match self.starting_block() {
            None => None,
            Some(n) => n.name(),
        }
    }

    #[must_use]
    pub fn starting_block(&self) -> Option<TwigStartingBlock> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn body(&self) -> Option<Body> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn ending_block(&self) -> Option<TwigEndingBlock> {
        support::child(&self.syntax)
    }
}

ast_node!(TwigStartingBlock, SyntaxKind::TWIG_STARTING_BLOCK);
impl TwigStartingBlock {
    /// Name of the twig block
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![word])
    }

    /// Parent complete twig block
    #[must_use]
    pub fn twig_block(&self) -> Option<TwigBlock> {
        match self.syntax.parent() {
            Some(p) => TwigBlock::cast(p),
            None => None,
        }
    }
}

ast_node!(TwigEndingBlock, SyntaxKind::TWIG_ENDING_BLOCK);
impl TwigEndingBlock {
    /// Parent complete twig block
    #[must_use]
    pub fn twig_block(&self) -> Option<TwigBlock> {
        match self.syntax.parent() {
            Some(p) => TwigBlock::cast(p),
            None => None,
        }
    }
}

ast_node!(HtmlTag, SyntaxKind::HTML_TAG);
impl HtmlTag {
    /// Name of the tag
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        match self.starting_tag() {
            Some(n) => n.name(),
            None => None,
        }
    }

    /// Returns true if the tag doesn't have an ending tag
    #[must_use]
    pub fn is_self_closing(&self) -> bool {
        self.ending_tag().is_none()
    }

    /// Attributes of the tag
    #[must_use]
    pub fn attributes(&self) -> AstChildren<HtmlAttribute> {
        match self.starting_tag() {
            Some(n) => n.attributes(),
            // create an iterator for HtmlAttribute over the tag itself, which should yield no results
            None => support::children(&self.syntax),
        }
    }

    #[must_use]
    pub fn starting_tag(&self) -> Option<HtmlStartingTag> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn body(&self) -> Option<Body> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn ending_tag(&self) -> Option<HtmlEndingTag> {
        support::child(&self.syntax)
    }
}

ast_node!(HtmlStartingTag, SyntaxKind::HTML_STARTING_TAG);
impl HtmlStartingTag {
    /// Name of the tag
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![word])
    }

    /// Attributes of the tag
    #[must_use]
    pub fn attributes(&self) -> AstChildren<HtmlAttribute> {
        match support::child::<HtmlAttributeList>(&self.syntax) {
            Some(list) => support::children(&list.syntax),
            // create an iterator for HtmlAttribute over the startingTag itself, which should yield no results
            None => support::children(&self.syntax),
        }
    }

    /// Parent complete html tag
    #[must_use]
    pub fn html_tag(&self) -> Option<HtmlTag> {
        match self.syntax.parent() {
            Some(p) => HtmlTag::cast(p),
            None => None,
        }
    }
}

ast_node!(HtmlAttribute, SyntaxKind::HTML_ATTRIBUTE);
impl HtmlAttribute {
    /// Name of the attribute (left side of the equal sign)
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![word])
    }

    /// Value of the attribute
    #[must_use]
    pub fn value(&self) -> Option<HtmlString> {
        support::child(&self.syntax)
    }

    /// Parent starting html tag
    #[must_use]
    pub fn html_tag(&self) -> Option<HtmlStartingTag> {
        match self.syntax.parent() {
            Some(p) => HtmlStartingTag::cast(p),
            None => None,
        }
    }
}

ast_node!(HtmlEndingTag, SyntaxKind::HTML_ENDING_TAG);
impl HtmlEndingTag {
    /// Name of the tag
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![word])
    }

    /// Parent complete html tag
    #[must_use]
    pub fn html_tag(&self) -> Option<HtmlTag> {
        match self.syntax.parent() {
            Some(p) => HtmlTag::cast(p),
            None => None,
        }
    }
}

ast_node!(TwigBinaryExpression, SyntaxKind::TWIG_BINARY_EXPRESSION);
impl TwigBinaryExpression {
    #[must_use]
    pub fn operator(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .find_map(|element| match element {
                SyntaxElement::Token(t) if !t.kind().is_trivia() => Some(t),
                _ => None,
            })
    }

    #[must_use]
    pub fn lhs_expression(&self) -> Option<TwigExpression> {
        self.syntax.children().find_map(TwigExpression::cast)
    }

    #[must_use]
    pub fn rhs_expression(&self) -> Option<TwigExpression> {
        self.syntax
            .children()
            .filter_map(TwigExpression::cast)
            .nth(1)
    }
}

ast_node!(
    LudtwigDirectiveRuleList,
    SyntaxKind::LUDTWIG_DIRECTIVE_RULE_LIST
);
impl LudtwigDirectiveRuleList {
    #[must_use]
    pub fn get_rule_names(&self) -> Vec<String> {
        self.syntax
            .children_with_tokens()
            .filter_map(|element| match element {
                NodeOrToken::Token(t) if t.kind() == SyntaxKind::TK_WORD => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
            .collect()
    }
}

ast_node!(
    LudtwigDirectiveFileIgnore,
    SyntaxKind::LUDTWIG_DIRECTIVE_FILE_IGNORE
);
impl LudtwigDirectiveFileIgnore {
    #[must_use]
    pub fn get_rules(&self) -> Vec<String> {
        match support::child::<LudtwigDirectiveRuleList>(&self.syntax) {
            Some(rule_list) => rule_list.get_rule_names(),
            None => vec![],
        }
    }
}

ast_node!(LudtwigDirectiveIgnore, SyntaxKind::LUDTWIG_DIRECTIVE_IGNORE);
impl LudtwigDirectiveIgnore {
    #[must_use]
    pub fn get_rules(&self) -> Vec<String> {
        match support::child::<LudtwigDirectiveRuleList>(&self.syntax) {
            Some(rule_list) => rule_list.get_rule_names(),
            None => vec![],
        }
    }
}

ast_node!(TwigLiteralString, SyntaxKind::TWIG_LITERAL_STRING);
impl TwigLiteralString {
    #[must_use]
    pub fn get_inner(&self) -> Option<TwigLiteralStringInner> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn get_opening_quote(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .take_while(|element| {
                if element.as_node().is_some() {
                    return false; // found inner string node
                }

                true
            })
            .find_map(|element| match element {
                // first non trivia token should be a quote
                NodeOrToken::Token(t) if !t.kind().is_trivia() => Some(t),
                _ => None,
            })
    }

    #[must_use]
    pub fn get_closing_quote(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .skip_while(|element| {
                if element.as_node().is_some() {
                    return false; // found inner string node, stop skipping
                }

                true
            })
            .find_map(|element| match element {
                // first non trivia token should be a quote
                NodeOrToken::Token(t) if !t.kind().is_trivia() => Some(t),
                _ => None,
            })
    }
}

ast_node!(
    TwigLiteralStringInner,
    SyntaxKind::TWIG_LITERAL_STRING_INNER
);
impl TwigLiteralStringInner {
    #[must_use]
    pub fn get_interpolations(&self) -> AstChildren<TwigLiteralStringInterpolation> {
        support::children(&self.syntax)
    }
}

ast_node!(HtmlString, SyntaxKind::HTML_STRING);
impl HtmlString {
    #[must_use]
    pub fn get_inner(&self) -> Option<HtmlStringInner> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn get_opening_quote(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .take_while(|element| {
                if element.as_node().is_some() {
                    return false; // found inner string node
                }

                true
            })
            .find_map(|element| match element {
                // first non trivia token should be a quote
                NodeOrToken::Token(t) if !t.kind().is_trivia() => Some(t),
                _ => None,
            })
    }

    #[must_use]
    pub fn get_closing_quote(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .skip_while(|element| {
                if element.as_node().is_some() {
                    return false; // found inner string node, stop skipping
                }

                true
            })
            .find_map(|element| match element {
                // first non trivia token should be a quote
                NodeOrToken::Token(t) if !t.kind().is_trivia() => Some(t),
                _ => None,
            })
    }
}

ast_node!(TwigExtends, SyntaxKind::TWIG_EXTENDS);
impl TwigExtends {
    #[must_use]
    pub fn get_extends_keyword(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!["extends"])
    }
}

ast_node!(TwigVar, SyntaxKind::TWIG_VAR);
impl TwigVar {
    #[must_use]
    pub fn get_expression(&self) -> Option<TwigExpression> {
        support::child(&self.syntax)
    }
}

ast_node!(TwigLiteralName, SyntaxKind::TWIG_LITERAL_NAME);
impl TwigLiteralName {
    #[must_use]
    pub fn get_name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::TK_WORD)
    }
}

ast_node!(Body, SyntaxKind::BODY);
ast_node!(TwigExpression, SyntaxKind::TWIG_EXPRESSION);
ast_node!(TwigUnaryExpression, SyntaxKind::TWIG_UNARY_EXPRESSION);
ast_node!(
    TwigParenthesesExpression,
    SyntaxKind::TWIG_PARENTHESES_EXPRESSION
);
ast_node!(
    TwigConditionalExpression,
    SyntaxKind::TWIG_CONDITIONAL_EXPRESSION
);
ast_node!(TwigOperand, SyntaxKind::TWIG_OPERAND);
ast_node!(TwigAccessor, SyntaxKind::TWIG_ACCESSOR);
ast_node!(TwigFilter, SyntaxKind::TWIG_FILTER);
ast_node!(TwigIndexLookup, SyntaxKind::TWIG_INDEX_LOOKUP);
ast_node!(TwigIndex, SyntaxKind::TWIG_INDEX);
ast_node!(TwigIndexRange, SyntaxKind::TWIG_INDEX_RANGE);
ast_node!(TwigFunctionCall, SyntaxKind::TWIG_FUNCTION_CALL);
ast_node!(TwigArrowFunction, SyntaxKind::TWIG_ARROW_FUNCTION);
ast_node!(TwigArguments, SyntaxKind::TWIG_ARGUMENTS);
ast_node!(TwigNamedArgument, SyntaxKind::TWIG_NAMED_ARGUMENT);
ast_node!(
    TwigLiteralStringInterpolation,
    SyntaxKind::TWIG_LITERAL_STRING_INTERPOLATION
);
ast_node!(TwigLiteralNumber, SyntaxKind::TWIG_LITERAL_NUMBER);
ast_node!(TwigLiteralArray, SyntaxKind::TWIG_LITERAL_ARRAY);
ast_node!(TwigLiteralArrayInner, SyntaxKind::TWIG_LITERAL_ARRAY_INNER);
ast_node!(TwigLiteralNull, SyntaxKind::TWIG_LITERAL_NULL);
ast_node!(TwigLiteralBoolean, SyntaxKind::TWIG_LITERAL_BOOLEAN);
ast_node!(TwigLiteralHash, SyntaxKind::TWIG_LITERAL_HASH);
ast_node!(TwigLiteralHashItems, SyntaxKind::TWIG_LITERAL_HASH_ITEMS);
ast_node!(TwigLiteralHashPair, SyntaxKind::TWIG_LITERAL_HASH_PAIR);
ast_node!(TwigLiteralHashKey, SyntaxKind::TWIG_LITERAL_HASH_KEY);
ast_node!(TwigLiteralHashValue, SyntaxKind::TWIG_LITERAL_HASH_VALUE);
ast_node!(TwigComment, SyntaxKind::TWIG_COMMENT);
ast_node!(TwigIf, SyntaxKind::TWIG_IF);
ast_node!(TwigIfBlock, SyntaxKind::TWIG_IF_BLOCK);
ast_node!(TwigElseIfBlock, SyntaxKind::TWIG_ELSE_IF_BLOCK);
ast_node!(TwigElseBlock, SyntaxKind::TWIG_ELSE_BLOCK);
ast_node!(TwigEndIfBlock, SyntaxKind::TWIG_ENDIF_BLOCK);
ast_node!(TwigSet, SyntaxKind::TWIG_SET);
ast_node!(TwigSetBlock, SyntaxKind::TWIG_SET_BLOCK);
ast_node!(TwigEndSetBlock, SyntaxKind::TWIG_ENDSET_BLOCK);
ast_node!(TwigAssignment, SyntaxKind::TWIG_ASSIGNMENT);
ast_node!(TwigFor, SyntaxKind::TWIG_FOR);
ast_node!(TwigForBlock, SyntaxKind::TWIG_FOR_BLOCK);
ast_node!(TwigForElseBlock, SyntaxKind::TWIG_FOR_ELSE_BLOCK);
ast_node!(TwigEndForBlock, SyntaxKind::TWIG_ENDFOR_BLOCK);
ast_node!(TwigInclude, SyntaxKind::TWIG_INCLUDE);
ast_node!(TwigIncludeWith, SyntaxKind::TWIG_INCLUDE_WITH);
ast_node!(TwigUse, SyntaxKind::TWIG_USE);
ast_node!(TwigOverride, SyntaxKind::TWIG_OVERRIDE);
ast_node!(TwigApply, SyntaxKind::TWIG_APPLY);
ast_node!(
    TwigApplyStartingBlock,
    SyntaxKind::TWIG_APPLY_STARTING_BLOCK
);
ast_node!(TwigApplyEndingBlock, SyntaxKind::TWIG_APPLY_ENDING_BLOCK);
ast_node!(TwigAutoescape, SyntaxKind::TWIG_AUTOESCAPE);
ast_node!(
    TwigAutoescapeStartingBlock,
    SyntaxKind::TWIG_AUTOESCAPE_STARTING_BLOCK
);
ast_node!(
    TwigAutoescapeEndingBlock,
    SyntaxKind::TWIG_AUTOESCAPE_ENDING_BLOCK
);
ast_node!(TwigDeprecated, SyntaxKind::TWIG_DEPRECATED);
ast_node!(TwigDo, SyntaxKind::TWIG_DO);
ast_node!(TwigEmbed, SyntaxKind::TWIG_EMBED);
ast_node!(
    TwigEmbedStartingBlock,
    SyntaxKind::TWIG_EMBED_STARTING_BLOCK
);
ast_node!(TwigEmbedEndingBlock, SyntaxKind::TWIG_EMBED_ENDING_BLOCK);
ast_node!(TwigFlush, SyntaxKind::TWIG_FLUSH);
ast_node!(TwigFrom, SyntaxKind::TWIG_FROM);
ast_node!(TwigImport, SyntaxKind::TWIG_IMPORT);
ast_node!(TwigSandbox, SyntaxKind::TWIG_SANDBOX);
ast_node!(
    TwigSandboxStartingBlock,
    SyntaxKind::TWIG_SANDBOX_STARTING_BLOCK
);
ast_node!(
    TwigSandboxEndingBlock,
    SyntaxKind::TWIG_SANDBOX_ENDING_BLOCK
);
ast_node!(TwigVerbatim, SyntaxKind::TWIG_VERBATIM);
ast_node!(
    TwigVerbatimStartingBlock,
    SyntaxKind::TWIG_VERBATIM_STARTING_BLOCK
);
ast_node!(
    TwigVerbatimEndingBlock,
    SyntaxKind::TWIG_VERBATIM_ENDING_BLOCK
);
ast_node!(TwigMacro, SyntaxKind::TWIG_MACRO);
ast_node!(
    TwigMacroStartingBlock,
    SyntaxKind::TWIG_MACRO_STARTING_BLOCK
);
ast_node!(TwigMacroEndingBlock, SyntaxKind::TWIG_MACRO_ENDING_BLOCK);
ast_node!(TwigWith, SyntaxKind::TWIG_WITH);
ast_node!(TwigWithStartingBlock, SyntaxKind::TWIG_WITH_STARTING_BLOCK);
ast_node!(TwigWithEndingBlock, SyntaxKind::TWIG_WITH_ENDING_BLOCK);
ast_node!(TwigCache, SyntaxKind::TWIG_CACHE);
ast_node!(TwigCacheTTL, SyntaxKind::TWIG_CACHE_TTL);
ast_node!(TwigCacheTags, SyntaxKind::TWIG_CACHE_TAGS);
ast_node!(
    TwigCacheStartingBlock,
    SyntaxKind::TWIG_CACHE_STARTING_BLOCK
);
ast_node!(TwigCacheEndingBlock, SyntaxKind::TWIG_CACHE_ENDING_BLOCK);
ast_node!(ShopwareTwigExtends, SyntaxKind::SHOPWARE_TWIG_SW_EXTENDS);
ast_node!(ShopwareTwigInclude, SyntaxKind::SHOPWARE_TWIG_SW_INCLUDE);
ast_node!(
    ShopwareSilentFeatureCall,
    SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL
);
ast_node!(
    ShopwareSilentFeatureCallStartingBlock,
    SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL_STARTING_BLOCK
);
ast_node!(
    ShopwareSilentFeatureCallEndingBlock,
    SyntaxKind::SHOPWARE_SILENT_FEATURE_CALL_ENDING_BLOCK
);
ast_node!(ShopwareReturn, SyntaxKind::SHOPWARE_RETURN);
ast_node!(ShopwareIcon, SyntaxKind::SHOPWARE_ICON);
ast_node!(ShopwareIconStyle, SyntaxKind::SHOPWARE_ICON_STYLE);
ast_node!(ShopwareThumbnails, SyntaxKind::SHOPWARE_THUMBNAILS);
ast_node!(ShopwareThumbnailsWith, SyntaxKind::SHOPWARE_THUMBNAILS_WITH);
ast_node!(HtmlDoctype, SyntaxKind::HTML_DOCTYPE);
ast_node!(HtmlAttributeList, SyntaxKind::HTML_ATTRIBUTE_LIST);
ast_node!(HtmlStringInner, SyntaxKind::HTML_STRING_INNER);
ast_node!(HtmlText, SyntaxKind::HTML_TEXT);
ast_node!(HtmlRawText, SyntaxKind::HTML_RAW_TEXT);
ast_node!(HtmlComment, SyntaxKind::HTML_COMMENT);
ast_node!(Error, SyntaxKind::ERROR);
ast_node!(Root, SyntaxKind::ROOT);
ast_node!(TwigTrans, SyntaxKind::TWIG_TRANS);
ast_node!(
    TwigTransStartingBlock,
    SyntaxKind::TWIG_TRANS_STARTING_BLOCK
);
ast_node!(
    TwigTransEndingBlock,
    SyntaxKind::TWIG_TRANS_ENDING_BLOCK
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use expect_test::expect;

    fn parse_and_extract<T: AstNode<Language = TemplateLanguage>>(input: &str) -> T {
        let (tree, errors) = parse(input).split();
        assert_eq!(errors, vec![]);
        support::child(&tree).unwrap()
    }

    #[test]
    fn simple_html_tag() {
        let raw = r#"<div class="hello">world {{ 42 }}</div>"#;
        let html_tag: HtmlTag = parse_and_extract(raw);

        assert_eq!(format!("{html_tag}"), raw.to_string());
        expect![[r#"
            HTML_TAG@0..39
              HTML_STARTING_TAG@0..19
                TK_LESS_THAN@0..1 "<"
                TK_WORD@1..4 "div"
                HTML_ATTRIBUTE_LIST@4..18
                  HTML_ATTRIBUTE@4..18
                    TK_WHITESPACE@4..5 " "
                    TK_WORD@5..10 "class"
                    TK_EQUAL@10..11 "="
                    HTML_STRING@11..18
                      TK_DOUBLE_QUOTES@11..12 "\""
                      HTML_STRING_INNER@12..17
                        TK_WORD@12..17 "hello"
                      TK_DOUBLE_QUOTES@17..18 "\""
                TK_GREATER_THAN@18..19 ">"
              BODY@19..33
                HTML_TEXT@19..24
                  TK_WORD@19..24 "world"
                TWIG_VAR@24..33
                  TK_WHITESPACE@24..25 " "
                  TK_OPEN_CURLY_CURLY@25..27 "{{"
                  TWIG_EXPRESSION@27..30
                    TWIG_LITERAL_NUMBER@27..30
                      TK_WHITESPACE@27..28 " "
                      TK_NUMBER@28..30 "42"
                  TK_WHITESPACE@30..31 " "
                  TK_CLOSE_CURLY_CURLY@31..33 "}}"
              HTML_ENDING_TAG@33..39
                TK_LESS_THAN_SLASH@33..35 "</"
                TK_WORD@35..38 "div"
                TK_GREATER_THAN@38..39 ">""#]]
        .assert_eq(&format!("{html_tag:?}"));

        assert!(!html_tag.is_self_closing());
        assert_eq!(
            html_tag.name().map(|t| t.to_string()),
            Some("div".to_string())
        );
        assert_eq!(
            html_tag.starting_tag().map(|t| t.to_string()),
            Some(r#"<div class="hello">"#.to_string())
        );
        assert_eq!(
            html_tag.body().map(|t| t.to_string()),
            Some("world {{ 42 }}".to_string())
        );
        assert_eq!(
            html_tag.ending_tag().map(|t| t.to_string()),
            Some("</div>".to_string())
        );
        assert_eq!(html_tag.attributes().count(), 1);
        assert_eq!(
            html_tag
                .attributes()
                .next()
                .and_then(|t| t.name())
                .map(|t| t.to_string()),
            Some("class".to_string())
        );
        assert_eq!(
            html_tag
                .attributes()
                .next()
                .and_then(|t| t.value())
                .and_then(|t| t.get_inner())
                .map(|t| t.to_string()),
            Some("hello".to_string())
        );
    }
}

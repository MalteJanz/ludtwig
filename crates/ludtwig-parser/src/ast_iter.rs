use crate::ast::*;

impl SyntaxNode {
    /// Create a borrowing iterator over this &[SyntaxNode] and all it's children (and their children, ...).
    /// It visits in a pre-order tree traversal:
    /// 1. visits the node itself
    /// 2. visits all the child nodes
    pub fn iter(&self) -> AstIterator<SyntaxNode> {
        self.into_iter()
    }

    /// Create a borrowing iterator over the &[TagAttribute]s of a &[Tag] that iterates over all nested attributes.
    /// If this &[SyntaxNode] is not a &[Tag] it will return an empty iterator.
    ///
    /// It visits in a pre-order tree traversal:
    /// 1. visits the attribute itself
    /// 2. visits all the child attributes
    pub fn attribute_iter(&self) -> AstIterator<TagAttribute> {
        if let SyntaxNode::Tag(t) = self {
            return t.attribute_iter();
        }

        AstIterator { buffer: Vec::new() }
    }
}

impl Tag {
    /// Create a borrowing iterator over the &[TagAttribute]s of this &[Tag] that iterates over all nested attributes.
    /// It visits in a pre-order tree traversal:
    /// 1. visits the attribute itself
    /// 2. visits all the child attributes
    fn attribute_iter(&self) -> AstIterator<TagAttribute> {
        let buffer = self.attributes.iter().rev().collect();

        AstIterator { buffer }
    }
}

impl<'a> IntoIterator for &'a SyntaxNode {
    type Item = &'a SyntaxNode;
    type IntoIter = AstIterator<'a, SyntaxNode>;

    /// Create a borrowing iterator over this &[SyntaxNode] and all it's children (and their children, ...).
    /// It visits in a pre-order tree traversal:
    /// 1. visits the node itself
    /// 2. visits all the child nodes
    fn into_iter(self) -> Self::IntoIter {
        AstIterator {
            buffer: vec![&self],
        }
    }
}

/// Borrowing iterator over the &T tree.
/// It returns a &T with each call to next.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AstIterator<'a, T> {
    buffer: Vec<&'a T>,
}

impl<'a, T> AstIterator<'a, T> {
    fn push_children_onto_stack(&mut self, children: &'a [T]) {
        for child in children.iter().rev() {
            self.buffer.push(child);
        }
    }
}

impl<'a> Iterator for AstIterator<'a, SyntaxNode> {
    type Item = &'a SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.buffer.pop() {
            match node {
                SyntaxNode::Root(ref n) => self.push_children_onto_stack(n),
                SyntaxNode::Tag(n) => self.push_children_onto_stack(&n.children),
                SyntaxNode::TwigStructure(s) => match s {
                    TwigStructure::TwigBlock(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigFor(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigIf(n) => {
                        for arm in &n.if_arms {
                            self.push_children_onto_stack(&arm.children);
                        }
                    }
                    TwigStructure::TwigApply(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigSetCapture(n) => self.push_children_onto_stack(&n.children),
                },
                SyntaxNode::Plain(_) => {}
                SyntaxNode::Whitespace => {}
                SyntaxNode::HtmlComment(_) => {}
                SyntaxNode::OutputExpression(_) => {}
                SyntaxNode::TwigComment(_) => {}
                SyntaxNode::TwigStatement(_) => {}
            }

            Some(node)
        } else {
            None
        }
    }
}

impl<'a> Iterator for AstIterator<'a, TagAttribute> {
    type Item = &'a TagAttribute;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(attribute) = self.buffer.pop() {
            match attribute {
                TagAttribute::TwigStructure(s) => match s {
                    TwigStructure::TwigBlock(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigFor(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigIf(n) => {
                        for arm in &n.if_arms {
                            self.push_children_onto_stack(&arm.children);
                        }
                    }
                    TwigStructure::TwigApply(n) => self.push_children_onto_stack(&n.children),
                    TwigStructure::TwigSetCapture(n) => self.push_children_onto_stack(&n.children),
                },
                TagAttribute::HtmlAttribute(_) => {}
                TagAttribute::TwigComment(_) => {}
            }

            Some(attribute)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;

    #[test]
    fn test_syntax_node_iterator() {
        let first_tag = SyntaxNode::Tag(Tag {
            name: "h2".to_string(),
            self_closed: false,
            attributes: vec![],
            children: vec![],
        });
        let child_plain = SyntaxNode::Plain(Plain {
            plain: "hello world".to_string(),
        });
        let second_tag = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "class".to_string(),
                value: Some("main".to_string()),
            })],
            children: vec![child_plain.clone()],
        });
        let block = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "main".to_string(),
            children: vec![first_tag.clone(), second_tag.clone()],
        }));
        let root = SyntaxNode::Root(vec![block.clone()]);

        let mut tree_iter = root.iter();
        assert_eq!(tree_iter.next(), Some(&root));
        assert_eq!(tree_iter.next(), Some(&block));
        assert_eq!(tree_iter.next(), Some(&first_tag));
        assert_eq!(tree_iter.next(), Some(&second_tag));
        assert_eq!(tree_iter.next(), Some(&child_plain));
        assert_eq!(tree_iter.next(), None);
    }

    #[test]
    fn test_tag_attribute_iterator() {
        let html_attribute = TagAttribute::HtmlAttribute(HtmlAttribute {
            name: "class".to_string(),
            value: Some("main".to_string()),
        });
        let nested_html_attribute = TagAttribute::HtmlAttribute(HtmlAttribute {
            name: "style".to_string(),
            value: Some("display: block".to_string()),
        });
        let second_nested_html_attribute = TagAttribute::HtmlAttribute(HtmlAttribute {
            name: "disabled".to_string(),
            value: None,
        });
        let twig_syntax_attribute = TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
            if_arms: vec![TwigIfArm {
                expression: Some("isDisabled".to_string()),
                children: vec![
                    nested_html_attribute.clone(),
                    second_nested_html_attribute.clone(),
                ],
            }],
        }));
        let comment_attribute = TagAttribute::TwigComment(TwigComment {
            content: "TODO".to_string(),
        });
        let tag = Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                html_attribute.clone(),
                twig_syntax_attribute.clone(),
                comment_attribute.clone(),
            ],
            children: vec![],
        };
        let syntax_node = SyntaxNode::Tag(tag.clone());

        let mut attr_iter = syntax_node.attribute_iter();
        assert_eq!(attr_iter, tag.attribute_iter()); // these iterators should be the same

        assert_eq!(attr_iter.next(), Some(&html_attribute));
        assert_eq!(attr_iter.next(), Some(&twig_syntax_attribute));
        assert_eq!(attr_iter.next(), Some(&nested_html_attribute));
        assert_eq!(attr_iter.next(), Some(&second_nested_html_attribute));
        assert_eq!(attr_iter.next(), Some(&comment_attribute));
        assert_eq!(attr_iter.next(), None);
    }
}

use crate::ast::*;

impl SyntaxNode {
    /// Create a borrowing iterator over this &[SyntaxNode] and all it's children (and their children, ...).
    /// It visits in a pre-order tree traversal:
    /// 1. visits the node itself
    /// 2. visits all the child nodes
    ///
    /// Also each node is visited with a [IteratorContext] that contains the following information:
    /// - the parent node of the currently visited one.
    /// - the previous node in the tree before the visited one (same depth).
    /// - the after node in the tree that occurs after the visited one (same depth).
    pub fn context_iter(&self) -> AstContextIterator<SyntaxNode> {
        AstContextIterator {
            buffer: vec![(
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous: None,
                    next: None,
                },
                &self,
            )],
        }
    }

    /// Create a borrowing iterator over the &[TagAttribute]s of a &[Tag] that iterates over all nested attributes with a context.
    /// If this &[SyntaxNode] is not a &[Tag] it will return an empty iterator.
    ///
    /// Also each attribute is visited with a [IteratorContext] that contains the following information:
    /// - the parent attribute of the currently visited one.
    /// - the previous attribute in the tree before the visited one (same depth).
    /// - the after attribute in the tree that occurs after the visited one (same depth).
    pub fn context_attribute_iter(&self) -> AstContextIterator<TagAttribute> {
        if let SyntaxNode::Tag(t) = self {
            return t.context_attribute_iter();
        }

        AstContextIterator { buffer: Vec::new() }
    }
}

impl Tag {
    /// Create a borrowing iterator over the &[TagAttribute]s of this &[Tag] that iterates over all nested attributes.
    /// It visits in a pre-order tree traversal:
    /// 1. visits the node itself
    /// 2. visits all the child nodes
    ///
    /// Also each attribute is visited with a [IteratorContext] that contains the following information:
    /// - the parent attribute of the currently visited one.
    /// - the previous attribute in the tree before the visited one (same depth).
    /// - the after attribute in the tree that occurs after the visited one (same depth).
    fn context_attribute_iter(&self) -> AstContextIterator<TagAttribute> {
        let mut buffer = Vec::with_capacity(self.attributes.len());
        for idx in (0..self.attributes.len()).rev() {
            let previous = if idx > 0 {
                self.attributes.get(idx - 1)
            } else {
                None
            };
            let child = &self.attributes[idx];
            let after = self.attributes.get(idx + 1);

            buffer.push((
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous,
                    next: after,
                },
                child,
            ));
        }

        AstContextIterator { buffer }
    }
}

/// Context for each call to next on a [AstContextIterator].
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IteratorContext<'a, T> {
    pub depth: usize,
    pub parent: Option<&'a T>,
    pub previous: Option<&'a T>,
    pub next: Option<&'a T>,
}

/// Borrowing iterator over the &T tree.
/// It returns a tuple (context, &T) with each call to next.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AstContextIterator<'a, T> {
    buffer: Vec<(IteratorContext<'a, T>, &'a T)>,
}

impl<'a, T> AstContextIterator<'a, T> {
    fn push_onto_stack<'b>(
        &mut self,
        context: &'b IteratorContext<'a, T>,
        itself: &'a T,
        children: &'a [T],
    ) {
        let child_depth = context.depth + 1;
        for idx in (0..children.len()).rev() {
            let previous = if idx > 0 { children.get(idx - 1) } else { None };
            let child = &children[idx];
            let after = children.get(idx + 1);

            self.buffer.push((
                IteratorContext {
                    depth: child_depth,
                    parent: Some(itself),
                    previous,
                    next: after,
                },
                child,
            ));
        }
    }
}

impl<'a> Iterator for AstContextIterator<'a, SyntaxNode> {
    type Item = (IteratorContext<'a, SyntaxNode>, &'a SyntaxNode);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((context, node)) = self.buffer.pop() {
            match node {
                SyntaxNode::Root(ref n) => self.push_onto_stack(&context, node, n),
                SyntaxNode::Tag(n) => self.push_onto_stack(&context, node, &n.children),
                SyntaxNode::TwigStructure(s) => match s {
                    TwigStructure::TwigBlock(n) => {
                        self.push_onto_stack(&context, node, &n.children)
                    }
                    TwigStructure::TwigFor(n) => self.push_onto_stack(&context, node, &n.children),
                    TwigStructure::TwigIf(n) => {
                        for arm in &n.if_arms {
                            self.push_onto_stack(&context, node, &arm.children);
                        }
                    }
                    TwigStructure::TwigApply(n) => {
                        self.push_onto_stack(&context, node, &n.children)
                    }
                    TwigStructure::TwigSetCapture(n) => {
                        self.push_onto_stack(&context, node, &n.children)
                    }
                },
                SyntaxNode::Plain(_) => {}
                SyntaxNode::Whitespace => {}
                SyntaxNode::HtmlComment(_) => {}
                SyntaxNode::OutputExpression(_) => {}
                SyntaxNode::TwigComment(_) => {}
                SyntaxNode::TwigStatement(_) => {}
            }

            Some((context, node))
        } else {
            None
        }
    }
}

impl<'a> Iterator for AstContextIterator<'a, TagAttribute> {
    type Item = (IteratorContext<'a, TagAttribute>, &'a TagAttribute);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((context, tag)) = self.buffer.pop() {
            match tag {
                TagAttribute::TwigStructure(s) => match s {
                    TwigStructure::TwigBlock(n) => self.push_onto_stack(&context, tag, &n.children),
                    TwigStructure::TwigFor(n) => self.push_onto_stack(&context, tag, &n.children),
                    TwigStructure::TwigIf(n) => {
                        for arm in &n.if_arms {
                            self.push_onto_stack(&context, tag, &arm.children);
                        }
                    }
                    TwigStructure::TwigApply(n) => self.push_onto_stack(&context, tag, &n.children),
                    TwigStructure::TwigSetCapture(n) => {
                        self.push_onto_stack(&context, tag, &n.children)
                    }
                },
                TagAttribute::HtmlAttribute(_) => {}
                TagAttribute::TwigComment(_) => {}
            }

            Some((context, tag))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::ast_context_iter::IteratorContext;

    #[test]
    fn test_syntax_node_advanced_iterator() {
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
        let third_plain = SyntaxNode::Plain(Plain {
            plain: "third".to_string(),
        });
        let block = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "main".to_string(),
            children: vec![first_tag.clone(), second_tag.clone(), third_plain.clone()],
        }));
        let root = SyntaxNode::Root(vec![block.clone()]);

        let mut tree_iter = root.context_iter();
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous: None,
                    next: None,
                },
                &root
            ))
        );
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 1,
                    parent: Some(&root),
                    previous: None,
                    next: None
                },
                &block
            ))
        );
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 2,
                    parent: Some(&block),
                    previous: None,
                    next: Some(&second_tag)
                },
                &first_tag
            ))
        );
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 2,
                    parent: Some(&block),
                    previous: Some(&first_tag),
                    next: Some(&third_plain),
                },
                &second_tag
            ))
        );
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 3,
                    parent: Some(&second_tag),
                    previous: None,
                    next: None
                },
                &child_plain
            ))
        );
        assert_eq!(
            tree_iter.next(),
            Some((
                IteratorContext {
                    depth: 2,
                    parent: Some(&block),
                    previous: Some(&second_tag),
                    next: None
                },
                &third_plain
            ))
        );
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

        let mut attr_iter = syntax_node.context_attribute_iter();
        assert_eq!(attr_iter, tag.context_attribute_iter()); // these iterators should be the same

        assert_eq!(
            attr_iter.next(),
            Some((
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous: None,
                    next: Some(&twig_syntax_attribute)
                },
                &html_attribute
            ))
        );
        assert_eq!(
            attr_iter.next(),
            Some((
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous: Some(&html_attribute),
                    next: Some(&comment_attribute)
                },
                &twig_syntax_attribute
            ))
        );
        assert_eq!(
            attr_iter.next(),
            Some((
                IteratorContext {
                    depth: 1,
                    parent: Some(&twig_syntax_attribute),
                    previous: None,
                    next: Some(&second_nested_html_attribute)
                },
                &nested_html_attribute
            ))
        );
        assert_eq!(
            attr_iter.next(),
            Some((
                IteratorContext {
                    depth: 1,
                    parent: Some(&twig_syntax_attribute),
                    previous: Some(&nested_html_attribute),
                    next: None
                },
                &second_nested_html_attribute
            ))
        );
        assert_eq!(
            attr_iter.next(),
            Some((
                IteratorContext {
                    depth: 0,
                    parent: None,
                    previous: Some(&twig_syntax_attribute),
                    next: None
                },
                &comment_attribute
            ))
        );
        assert_eq!(attr_iter.next(), None);
    }
}

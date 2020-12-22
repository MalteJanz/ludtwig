use crate::process::FileContext;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use tokio::fs::File;
use tokio::io::{AsyncWrite, AsyncWriteExt, BufWriter};
use twig::ast::{HtmlComment, OutputExpression, Plain, SyntaxNode, Tag, TwigBlock, TwigComment};

/// Context for traversing the AST with printing in mind.
#[derive(Clone, PartialEq, Default)]
struct PrintingContext<'a> {
    previous_node: Option<&'a SyntaxNode>,
    after_node: Option<&'a SyntaxNode>,

    /// the last node in the list is the current node. everything before that is up in the hierarchy.
    parent_nodes: Vec<&'a SyntaxNode>,

    /// in tab (4 spaces) count
    indentation: u16,
}

impl<'a> PrintingContext<'a> {
    /// Clones the current context and returns a new one with the increased indentation.
    fn increase_indentation_by(&self, increase: u16) -> Self {
        let mut copy = self.clone();
        copy.indentation += increase;
        copy
    }

    /// Get the parent node for the current context.
    fn get_parent(&self) -> Option<&'a SyntaxNode> {
        self.parent_nodes
            .iter()
            .rev()
            .skip(1)
            .take(1)
            .copied()
            .next()
    }
}

/// Entry function for writing the ast back into files.
pub async fn write_tree(file_context: Arc<FileContext>) {
    let path = create_and_secure_output_path(&file_context).await;
    let file = File::create(path).await.expect("can't create file.");
    let mut writer = BufWriter::new(file);

    print_node(
        &mut writer,
        &file_context.tree,
        &mut PrintingContext::default(),
    )
    .await;

    writer.flush().await.unwrap();
}

/// Append the CLI output path if it is given and
/// create all directories if they do not exists.
/// Returns the full path which is secure to use.
async fn create_and_secure_output_path(file_context: &FileContext) -> PathBuf {
    let base_path = match &file_context.cli_context.output_path {
        None => Path::new(""),
        Some(p) => p,
    };
    let path = base_path.join(&*file_context.file_path);

    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .expect("can't create directory for output");
    }

    path
}

/// Print a single [SyntaxNode] from the AST, which can be anything.
fn print_node<'a, W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &'a mut W,
    node: &'a SyntaxNode,
    context: &'a mut PrintingContext<'a>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        context.parent_nodes.push(&node);

        match node {
            SyntaxNode::Root(root) => {
                print_node_list(writer, &root, context).await;
            }
            SyntaxNode::Tag(tag) => {
                print_tag(writer, &tag, context).await;
            }
            SyntaxNode::Plain(plain) => {
                print_plain(writer, &plain, context).await;
            }
            SyntaxNode::HtmlComment(comment) => {
                print_html_comment(writer, comment, context).await;
            }
            SyntaxNode::OutputExpression(vue) => {
                print_vue_block(writer, &vue, context).await;
            }
            SyntaxNode::TwigBlock(twig) => {
                print_twig_block(writer, &twig, context).await;
            }
            SyntaxNode::TwigParentCall => {
                print_twig_parent_call(writer, context).await;
            }
            SyntaxNode::TwigComment(comment) => {
                print_twig_comment(writer, comment, context).await;
            }
            SyntaxNode::Whitespace => {
                print_whitespace(writer, context).await;
            }
        }
    })
}

/// Print a list of [SyntaxNode]'s and prepare the context for each one.
async fn print_node_list<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    nodes: &[SyntaxNode],
    context: &PrintingContext<'_>,
) {
    for idx in 0..nodes.len() {
        let previous = if idx > 0 { nodes.get(idx - 1) } else { None };
        let current = &nodes[idx];
        let after = nodes.get(idx + 1);

        let mut context = PrintingContext {
            previous_node: previous,
            after_node: after,
            parent_nodes: context.parent_nodes.clone(),
            indentation: context.indentation,
        };

        print_node(writer, current, &mut context).await;
    }
}

async fn print_tag<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    tag: &Tag,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;

    writer.write_all(b"<").await.unwrap();
    writer.write_all(tag.name.as_bytes()).await.unwrap();

    let inline_mode = tag.attributes.len() <= 2 && calculate_tag_line_length(tag, context) <= 120;
    let continuation_indent_mode = tag.name.len() > 8;

    // attributes
    for (index, attribute) in tag.attributes.iter().enumerate() {
        if inline_mode {
            writer.write_all(b" ").await.unwrap();
        } else if continuation_indent_mode {
            writer.write_all(b"\n").await.unwrap();
            print_indentation(writer, &context.increase_indentation_by(2)).await;
        } else {
            // write attribute on first line (same as tag)
            if index == 0 {
                writer.write_all(b" ").await.unwrap();
            } else {
                writer.write_all(b"\n").await.unwrap();

                print_indentation(writer, context).await;
                for _ in 0..(tag.name.len() + 2) {
                    writer.write_all(b" ").await.unwrap();
                }
            }
        }

        writer.write_all(attribute.name.as_bytes()).await.unwrap();

        if let Some(value) = &attribute.value {
            writer.write_all(b"=\"").await.unwrap();
            writer.write_all(value.as_bytes()).await.unwrap();
            writer.write_all(b"\"").await.unwrap();
        }
    }

    if tag.self_closed {
        writer.write_all(b"/>").await.unwrap();
    } else {
        writer.write_all(b">").await.unwrap();
        // only print children if tag is not self_closed!
        print_node_list(writer, &tag.children, &context.increase_indentation_by(1)).await;
    }

    if let Some(last) = tag.children.last() {
        if let SyntaxNode::Whitespace = last {
            print_indentation(writer, context).await;
        }
    }

    if !tag.self_closed {
        writer.write_all(b"</").await.unwrap();
        writer.write_all(tag.name.as_bytes()).await.unwrap();
        writer.write_all(b">").await.unwrap();
    }
}

async fn print_plain<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    plain: &Plain,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(plain.plain.as_bytes()).await.unwrap();
}

async fn print_html_comment<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &HtmlComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"<!-- ").await.unwrap();
    writer.write_all(comment.content.as_bytes()).await.unwrap();
    writer.write_all(b" -->").await.unwrap();
}

async fn print_vue_block<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    vue: &OutputExpression,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{{ ").await.unwrap();
    writer.write_all(vue.content.as_bytes()).await.unwrap();
    writer.write_all(b" }}").await.unwrap();
}

async fn print_twig_block<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    twig: &TwigBlock,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;

    writer.write_all(b"{% block ").await.unwrap();
    writer.write_all(twig.name.as_bytes()).await.unwrap();
    writer.write_all(b" %}").await.unwrap();

    print_node_list(writer, &twig.children, &context.increase_indentation_by(1)).await;

    if let Some(last) = twig.children.last() {
        if let SyntaxNode::Whitespace = last {
            print_indentation(writer, context).await;
        }
    }

    writer.write_all(b"{% endblock %}").await.unwrap();
}

async fn print_twig_parent_call<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{% parent %}").await.unwrap();
}

async fn print_twig_comment<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &TwigComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{# ").await.unwrap();
    writer.write_all(comment.content.as_bytes()).await.unwrap();
    writer.write_all(b" #}").await.unwrap();
}

async fn print_whitespace<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    // decide if additional whitespaces are needed (for example before and after twig blocks
    match (
        context.previous_node,
        context.after_node,
        context.get_parent(),
    ) {
        (Some(prev), Some(aft), Some(par)) => {
            if let SyntaxNode::TwigBlock(_) = par {
                // don't print another whitespace if the parent is also a block.
            } else if let SyntaxNode::TwigBlock(_) = prev {
                // print another whitespace.
                writer.write_all(b"\r\n").await.unwrap();
            } else if let SyntaxNode::TwigBlock(_) = aft {
                // print another whitespace.
                writer.write_all(b"\r\n").await.unwrap();
            }
        }

        (None, Some(aft), Some(par)) => {
            if let SyntaxNode::TwigBlock(_) = par {
                // don't print another whitespace if the parent is also a block.
            } else if let SyntaxNode::TwigBlock(_) = aft {
                // print another whitespace.
                writer.write_all(b"\r\n").await.unwrap();
            }
        }

        (Some(prev), None, Some(par)) => {
            if let SyntaxNode::TwigBlock(_) = par {
                // don't print another whitespace if the parent is also a block.
            } else if let SyntaxNode::TwigBlock(_) = prev {
                // print another whitespace.
                writer.write_all(b"\r\n").await.unwrap();
            }
        }

        (_, _, _) => {}
    }

    writer.write_all(b"\r\n").await.unwrap();
}

async fn print_indentation<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    for _ in 0..context.indentation {
        writer.write_all(b"    ").await.unwrap();
    }
}

async fn print_indentation_if_whitespace_exists_before<W: AsyncWrite + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    if let Some(prev) = context.previous_node {
        if let SyntaxNode::Whitespace = prev {
            print_indentation(writer, context).await;
        }
    }
}

/// Calculates the line length including indentation but only with a maximum of two attributes
/// (everything above that will be ignored).
/// It is possible that this function returns a very high line length (>1000) if this is
/// a inline tag without whitespaces (like `<span>Hello</span>`)
fn calculate_tag_line_length(tag: &Tag, context: &PrintingContext) -> usize {
    context.indentation as usize * 4
        + 1
        + tag.name.len()
        + tag
            .attributes
            .iter()
            .take(2)
            .map(|a| 1 + a.name.len() + a.value.as_ref().map(|v| v.len() + 3).unwrap_or(0))
            .sum::<usize>()
        + tag.self_closed as usize
        + 1
        + if tag
            .children
            .first()
            .map(|f| !matches!(f, SyntaxNode::Whitespace))
            .unwrap_or(true)
        {
            // first child is a whitespace or there are no children
            if tag.children.is_empty() {
                if tag.self_closed {
                    0
                } else {
                    2 + tag.name.len() + 1
                }
            } else {
                1000 //add 1000 to line length if there is no whitespace between opening tag and children
                     // this will enforce to not stay in inline_mode but split the attributes on separate lines.
            }
        } else {
            0
        }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use twig::ast::HtmlAttribute;

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    async fn convert_tree_into_written_string(tree: SyntaxNode) -> String {
        let mut writer_raw: Cursor<Vec<u8>> = Cursor::new(Vec::new());

        print_node(&mut writer_raw, &tree, &mut PrintingContext::default()).await;

        String::from_utf8(writer_raw.into_inner()).unwrap()
    }

    #[tokio::test]
    async fn test_write_empty_html_tag() {
        let tree = SyntaxNode::Tag(Tag {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Tag(Tag {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![],
                    ..Default::default()
                }),
                SyntaxNode::Whitespace,
            ],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<this_is_a_test_one>\r\n    <this_is_a_test_two></this_is_a_test_two>\r\n</this_is_a_test_one>".to_string()
        );
    }

    #[tokio::test]
    async fn test_write_simple_twig_block() {
        let tree = SyntaxNode::TwigBlock(TwigBlock {
            name: "some_twig_block".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Plain(Plain {
                    plain: "Hello world".to_string(),
                }),
                SyntaxNode::Whitespace,
            ],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block some_twig_block %}\r\n    Hello world\r\n{% endblock %}".to_string()
        );
    }

    #[tokio::test]
    async fn test_write_nested_twig_block() {
        let tree = SyntaxNode::TwigBlock(TwigBlock {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigBlock(TwigBlock {
                            name: "this_is_a_test_three".to_string(),
                            children: vec![SyntaxNode::Whitespace],
                        }),
                        SyntaxNode::Whitespace,
                    ],
                }),
                SyntaxNode::Whitespace,
            ],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\r\n    {% block this_is_a_test_two %}\r\n        {% block this_is_a_test_three %}\r\n        {% endblock %}\r\n    {% endblock %}\r\n{% endblock %}".to_string()
        );
    }

    #[tokio::test]
    async fn test_write_nested_twig_block_separation() {
        let tree = SyntaxNode::Tag(Tag {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "Some content".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                }),
                SyntaxNode::Whitespace,
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "this_is_a_test_three".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "Some content".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                }),
                SyntaxNode::Whitespace,
            ],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<this_is_a_test_one>\r\n\r\n    {% block this_is_a_test_two %}\r\n        Some content\r\n    {% endblock %}\r\n\r\n    {% block this_is_a_test_three %}\r\n        Some content\r\n    {% endblock %}\r\n\r\n</this_is_a_test_one>".to_string()
        );
    }

    #[tokio::test]
    async fn test_write_empty_twig_block() {
        let tree = SyntaxNode::Tag(Tag {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![],
                }),
                SyntaxNode::Whitespace,
            ],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<this_is_a_test_one>\r\n\r\n    {% block this_is_a_test_two %}{% endblock %}\r\n\r\n</this_is_a_test_one>".to_string()
        );
    }

    #[tokio::test]
    async fn test_write_tag_and_twig_block_without_whitespace() {
        let tree = SyntaxNode::Tag(Tag {
            name: "slot".to_string(),
            children: vec![SyntaxNode::TwigBlock(TwigBlock {
                name: "sw_grid_slot_pagination".to_string(),
                children: vec![],
            })],
            attributes: vec![HtmlAttribute {
                name: "name".to_string(),
                value: Some("pagination".to_string()),
            }],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}{% endblock %}</slot>"
                .to_string()
        );
    }

    #[tokio::test]
    async fn test_write_tag_and_twig_block_content_without_whitespace() {
        let tree = SyntaxNode::Tag(Tag {
            name: "slot".to_string(),
            children: vec![SyntaxNode::TwigBlock(TwigBlock {
                name: "sw_grid_slot_pagination".to_string(),
                children: vec![SyntaxNode::Plain(Plain {
                    plain: "Hello world".to_string(),
                })],
            })],
            attributes: vec![HtmlAttribute {
                name: "name".to_string(),
                value: Some("pagination".to_string()),
            }],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}Hello world{% endblock %}</slot>"
                .to_string()
        );
    }
}

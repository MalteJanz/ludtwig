use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use tokio::fs::File;
use tokio::io::{AsyncWriteExt, BufWriter};
use twig::ast::{HtmlNode, HtmlPlain, HtmlTag, TwigBlock, VueBlock};

#[derive(Clone, PartialEq)]
struct PrintingContext<'a> {
    previous_node: Option<&'a HtmlNode>,
    after_node: Option<&'a HtmlNode>,

    /// the last node in the list is the current node. everything before that is up in the hierarchy.
    parent_nodes: Vec<&'a HtmlNode>,

    /// in tab count
    indentation: u16,
}

impl<'a> PrintingContext<'a> {
    /// clones the current context and returns a new one with the increased indentation.
    fn increase_indentation_by(&self, increase: u16) -> Self {
        let mut copy = self.clone();
        copy.indentation += increase;
        copy
    }
}

impl<'a> Default for PrintingContext<'a> {
    fn default() -> Self {
        PrintingContext {
            previous_node: None,
            after_node: None,
            parent_nodes: vec![],
            indentation: 0,
        }
    }
}

pub async fn write_tree(path: PathBuf, tree: &HtmlNode) {
    // ToDo: replace this after done with testing.
    let base_path = Path::new("output");
    let path = base_path.join(path);

    println!("path: {:?}", path);

    let parent = path.parent().unwrap();
    tokio::fs::create_dir_all(parent).await.unwrap();

    let file = File::create(path).await.expect("can't create file.");
    let mut writer = BufWriter::new(file);

    print_node(&mut writer, tree, &mut PrintingContext::default()).await;

    writer.flush().await.unwrap();
}

fn print_node<'a>(
    writer: &'a mut BufWriter<File>,
    node: &'a HtmlNode,
    context: &'a mut PrintingContext<'a>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        context.parent_nodes.push(&node);

        match node {
            HtmlNode::Root(root) => {
                print_node_list(writer, &root, context).await;
            }
            HtmlNode::Tag(tag) => {
                print_tag(writer, &tag, context).await;
            }
            HtmlNode::Plain(plain) => {
                print_plain(writer, &plain, context).await;
            }
            HtmlNode::VueBlock(vue) => {
                print_vue_block(writer, &vue, context).await;
            }
            HtmlNode::TwigBlock(twig) => {
                print_twig_block(writer, &twig, context).await;
            }
            HtmlNode::TwigParentCall => {
                print_twig_parent_call(writer, context).await;
            }
            HtmlNode::Whitespace => {
                print_whitespace(writer, context).await;
            }
        }
    })
}

async fn print_node_list(
    writer: &mut BufWriter<File>,
    nodes: &Vec<HtmlNode>,
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

async fn print_tag(writer: &mut BufWriter<File>, tag: &HtmlTag, context: &PrintingContext<'_>) {
    print_indentation(writer, context).await;

    writer.write_all(b"<").await.unwrap();
    writer.write_all(tag.name.as_bytes()).await.unwrap();

    // attributes
    for (key, value) in &tag.attributes {
        if tag.attributes.len() > 2 || tag.name.len() > 24 {
            writer.write_all(b"\n").await.unwrap();
            print_indentation(writer, &context.increase_indentation_by(2)).await;
        } else {
            writer.write_all(b" ").await.unwrap();
        }

        writer.write_all(key.as_bytes()).await.unwrap();

        if value == "" {
            continue;
        }

        writer.write_all(b"=\"").await.unwrap();
        writer.write_all(value.as_bytes()).await.unwrap();
        writer.write_all(b"\"").await.unwrap();
    }

    if tag.self_closed {
        writer.write_all(b"/>").await.unwrap();
    } else {
        writer.write_all(b">").await.unwrap();
    }

    print_node_list(writer, &tag.children, &context.increase_indentation_by(1)).await;

    if !tag.children.is_empty() {
        print_indentation(writer, context).await;
    }

    if !tag.self_closed {
        writer.write_all(b"</").await.unwrap();
        writer.write_all(tag.name.as_bytes()).await.unwrap();
        writer.write_all(b">").await.unwrap();
    }
}

async fn print_plain(
    writer: &mut BufWriter<File>,
    plain: &HtmlPlain,
    context: &PrintingContext<'_>,
) {
    print_indentation(writer, context).await;
    writer.write_all(plain.plain.as_bytes()).await.unwrap();
}

async fn print_vue_block(
    writer: &mut BufWriter<File>,
    vue: &VueBlock,
    context: &PrintingContext<'_>,
) {
    print_indentation(writer, context).await;
    writer.write_all(b"{{ ").await.unwrap();
    writer.write_all(vue.content.as_bytes()).await.unwrap();
    writer.write_all(b" }}").await.unwrap();
}

async fn print_twig_block(
    writer: &mut BufWriter<File>,
    twig: &TwigBlock,
    context: &PrintingContext<'_>,
) {
    print_indentation(writer, context).await;
    writer.write_all(b"{% block ").await.unwrap();
    writer.write_all(twig.name.as_bytes()).await.unwrap();
    writer.write_all(b" %}").await.unwrap();

    print_node_list(writer, &twig.children, &context.increase_indentation_by(1)).await;

    print_indentation(writer, context).await;
    writer.write_all(b"{% endblock %}").await.unwrap();
}

async fn print_twig_parent_call(writer: &mut BufWriter<File>, context: &PrintingContext<'_>) {
    print_indentation(writer, context).await;
    writer.write_all(b"{% parent %}").await.unwrap();
}

async fn print_whitespace(writer: &mut BufWriter<File>, context: &PrintingContext<'_>) {
    if let Some(prev) = context.previous_node {
        if let HtmlNode::TwigBlock(_) = prev {
            // print another whitespace.
            writer.write_all(b"\r\n").await.unwrap();
        }
    }

    writer.write_all(b"\r\n").await.unwrap();
}

async fn print_indentation(writer: &mut BufWriter<File>, context: &PrintingContext<'_>) {
    for _ in 0..context.indentation {
        writer.write_all(b"    ").await.unwrap();
    }
}

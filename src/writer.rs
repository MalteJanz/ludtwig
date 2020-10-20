use std::ffi::OsStr;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use tokio::fs::File;
use tokio::io::{AsyncWriteExt, BufWriter};
use twig::ast::{HtmlNode, HtmlPlain, HtmlTag, TwigBlock, VueBlock};

pub async fn write_tree(path: PathBuf, tree: &HtmlNode) {
    // ToDo: replace this after done with testing.
    let mut base_path = Path::new("output");
    let path = base_path.join(path);

    println!("path: {:?}", path);

    let parent = path.parent().unwrap();
    tokio::fs::create_dir_all(parent).await.unwrap();

    let file = File::create(path).await.expect("can't create file.");
    let mut writer = BufWriter::new(file);

    print_node(&mut writer, tree, 0).await;

    writer.flush().await.unwrap();
}

fn print_node<'a>(
    writer: &'a mut BufWriter<File>,
    node: &'a HtmlNode,
    indentation: u16,
) -> Pin<Box<dyn Future<Output = ()> + 'a>> {
    Box::pin(async move {
        match node {
            HtmlNode::Root(root) => {
                for child in root {
                    print_node(writer, child, indentation).await;
                }
            }
            HtmlNode::Tag(tag) => {
                print_tag(writer, &tag, indentation).await;
            }
            HtmlNode::Plain(plain) => {
                print_plain(writer, &plain, indentation).await;
            }
            HtmlNode::VueBlock(vue) => {
                print_vue_block(writer, &vue, indentation).await;
            }
            HtmlNode::TwigBlock(twig) => {
                print_twig_block(writer, &twig, indentation).await;
            }
            HtmlNode::TwigParentCall => {
                print_twig_parent_call(writer, indentation).await;
            }
        }
    })
}

async fn print_tag(writer: &mut BufWriter<File>, tag: &HtmlTag, indentation: u16) {
    print_indentation(writer, indentation).await;

    writer.write_all("<".as_bytes()).await.unwrap();
    writer.write_all(tag.name.as_bytes()).await.unwrap();

    // attributes
    for (key, value) in &tag.attributes {
        if tag.attributes.len() > 2 {
            writer.write_all("\n".as_bytes()).await.unwrap();
            print_indentation(writer, indentation + 2).await;
        } else {
            writer.write_all(" ".as_bytes()).await.unwrap();
        }

        writer.write_all(key.as_bytes()).await.unwrap();

        if value == "" {
            continue;
        }

        writer.write_all("=\"".as_bytes()).await.unwrap();
        writer.write_all(value.as_bytes()).await.unwrap();
        writer.write_all("\"".as_bytes()).await.unwrap();
    }

    if tag.self_closed {
        writer.write_all("/>\n".as_bytes()).await.unwrap();
    } else {
        writer.write_all(">\n".as_bytes()).await.unwrap();
    }

    for child in &tag.children {
        print_node(writer, child, indentation + 1).await;
    }

    print_indentation(writer, indentation).await;
    if !tag.self_closed {
        writer.write_all("</".as_bytes()).await.unwrap();
        writer.write_all(tag.name.as_bytes()).await.unwrap();
        writer.write_all(">\n".as_bytes()).await.unwrap();
    }
}

async fn print_plain(writer: &mut BufWriter<File>, plain: &HtmlPlain, indentation: u16) {
    print_indentation(writer, indentation).await;
    writer.write_all(plain.plain.as_bytes()).await.unwrap();
    writer.write_all("\n".as_bytes()).await.unwrap();
}

async fn print_vue_block(writer: &mut BufWriter<File>, vue: &VueBlock, indentation: u16) {
    print_indentation(writer, indentation).await;
    writer.write_all("{{ ".as_bytes()).await.unwrap();
    writer.write_all(vue.content.as_bytes()).await.unwrap();
    writer.write_all(" }}\n".as_bytes()).await.unwrap();
}

async fn print_twig_block(writer: &mut BufWriter<File>, twig: &TwigBlock, indentation: u16) {
    print_indentation(writer, indentation).await;
    writer.write_all("{% block ".as_bytes()).await.unwrap();
    writer.write_all(twig.name.as_bytes()).await.unwrap();
    writer.write_all(" %}\n".as_bytes()).await.unwrap();

    for child in &twig.children {
        print_node(writer, child, indentation + 1).await;
    }

    print_indentation(writer, indentation).await;
    writer
        .write_all("{% endblock %}\n".as_bytes())
        .await
        .unwrap();
}

async fn print_twig_parent_call(writer: &mut BufWriter<File>, indentation: u16) {
    print_indentation(writer, indentation).await;
    writer.write_all("{% parent %}\n".as_bytes()).await.unwrap();
}

async fn print_indentation(writer: &mut BufWriter<File>, indentation: u16) {
    for _ in 0..indentation {
        writer.write_all("    ".as_bytes()).await.unwrap();
    }
}

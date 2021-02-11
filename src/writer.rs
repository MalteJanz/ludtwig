use crate::process::FileContext;
use async_std::fs;
use async_std::fs::File;
use async_std::future::Future;
use async_std::io::prelude::*;
use async_std::io::{BufWriter, Write};
use async_std::path::{Path, PathBuf};
use async_std::sync::Arc;
use async_trait::async_trait;
use ludtwig_parser::ast::{
    HtmlComment, OutputExpression, Plain, SyntaxNode, Tag, TagAttribute, TwigApply, TwigBlock,
    TwigComment, TwigFor, TwigIf, TwigSetCapture, TwigStatement, TwigStructure,
};
use std::pin::Pin;

const MAX_LINE_LENGTH: usize = 120;
const MIN_TAG_NAME_LENGTH_FOR_CONTINUATION_INDENT: usize = 9;

/// Context for traversing the AST with printing in mind.
#[derive(Debug, Clone, PartialEq, Default)]
struct PrintingContext<'a> {
    previous_node: Option<&'a SyntaxNode>,
    after_node: Option<&'a SyntaxNode>,

    /// the last node in the list is the current node. everything before that is up in the hierarchy.
    parent_nodes: Vec<&'a SyntaxNode>,

    /// in spaces count (a tab is 4 spaces)
    indentation_spaces: u32,
}

impl<'a> PrintingContext<'a> {
    /// Clones the current context and returns a new one with the increased indentation.
    fn increase_indentation_by_tabs(&self, tabs: u32) -> Self {
        let mut copy = self.clone();
        copy.indentation_spaces += tabs * 4;
        copy
    }

    /// Clones the current context and returns a new one with the increased indentation.
    fn increase_indentation_by_spaces(&self, spaces: u32) -> Self {
        let mut copy = self.clone();
        copy.indentation_spaces += spaces;
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

    /// Returns the amount of tabs and spaces needed for the current indentation.
    /// (tabs, spaces)
    fn get_tabs_and_spaces(&self) -> (u32, u32) {
        let tabs = self.indentation_spaces / 4;
        let spaces = self.indentation_spaces % 4;

        (tabs, spaces)
    }
}

/// Trait that allows to print a generic list of children.
#[async_trait]
trait GenericChildPrinter<T: IsWhitespace> {
    async fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
        writer: &mut W,
        nodes: &[T],
        context: &PrintingContext<'_>,
    );

    fn is_whitespace_sensitive() -> bool {
        true
    }
}

/// Struct that implements the GenericChildPrinter Trait for different children types.
struct DynamicChildPrinter();

/// In case of [SyntaxNode]
#[async_trait]
impl GenericChildPrinter<SyntaxNode> for DynamicChildPrinter {
    async fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
        writer: &mut W,
        nodes: &[SyntaxNode],
        context: &PrintingContext<'_>,
    ) {
        print_node_list(writer, nodes, context).await;
    }
}

/// In case of [TagAttribute]
#[async_trait]
impl GenericChildPrinter<TagAttribute> for DynamicChildPrinter {
    async fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
        writer: &mut W,
        nodes: &[TagAttribute],
        context: &PrintingContext<'_>,
    ) {
        print_attribute_list(writer, nodes, context).await;
    }

    fn is_whitespace_sensitive() -> bool {
        false
    }
}

trait IsWhitespace {
    fn is_whitespace(&self) -> bool {
        false
    }
}

impl IsWhitespace for SyntaxNode {
    fn is_whitespace(&self) -> bool {
        if let SyntaxNode::Whitespace = self {
            return true;
        }

        false
    }
}

impl IsWhitespace for TagAttribute {}

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
        fs::create_dir_all(parent)
            .await
            .expect("can't create directory for output");
    }

    path
}

/// Print a single [SyntaxNode] from the AST, which can be anything.
fn print_node<'a, W: Write + Unpin + Send + ?Sized>(
    writer: &'a mut W,
    node: &'a SyntaxNode,
    context: &'a mut PrintingContext<'a>,
) -> Pin<Box<dyn Future<Output = ()> + 'a + Send>> {
    Box::pin(async move {
        context.parent_nodes.push(&node);

        match node {
            SyntaxNode::Whitespace => {
                print_whitespace(writer, context).await;
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
            SyntaxNode::TwigStructure(TwigStructure::TwigBlock(block)) => {
                print_twig_block::<_, SyntaxNode, DynamicChildPrinter>(writer, &block, context)
                    .await;
            }
            SyntaxNode::TwigStructure(TwigStructure::TwigFor(twig_for)) => {
                print_twig_for::<_, SyntaxNode, DynamicChildPrinter>(writer, &twig_for, context)
                    .await;
            }
            SyntaxNode::TwigStructure(TwigStructure::TwigIf(twig_if)) => {
                print_twig_if::<_, SyntaxNode, DynamicChildPrinter>(writer, &twig_if, context)
                    .await;
            }
            SyntaxNode::TwigStatement(statement) => {
                print_twig_statement(writer, &statement, context).await;
            }
            SyntaxNode::TwigComment(comment) => {
                print_twig_comment(writer, comment, context).await;
            }
            SyntaxNode::TwigStructure(TwigStructure::TwigApply(twig_apply)) => {
                print_twig_apply::<_, SyntaxNode, DynamicChildPrinter>(
                    writer,
                    &twig_apply,
                    context,
                )
                .await;
            }
            SyntaxNode::TwigStructure(TwigStructure::TwigSetCapture(twig_set_capture)) => {
                print_twig_set_capture::<_, SyntaxNode, DynamicChildPrinter>(
                    writer,
                    &twig_set_capture,
                    context,
                )
                .await;
            }
            SyntaxNode::Root(root) => {
                print_node_list(writer, &root, context).await;
            }
        }
    })
}

/// Print a list of [SyntaxNode]'s and prepare the context for each one.
async fn print_node_list<W: Write + Unpin + Send + ?Sized>(
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
            indentation_spaces: context.indentation_spaces,
        };

        print_node(writer, current, &mut context).await;
    }
}

/// Print a list of [TagAttribute]'s. this is generally called by [TwigStructure<TagAttribute>]
/// and not by an [SyntaxNode::Tag] directly (because it does not do any calculations for
/// inline and continuation mode).
async fn print_attribute_list<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    attributes: &[TagAttribute],
    context: &PrintingContext<'_>,
) {
    for attribute in attributes {
        writer.write_all(b"\n").await.unwrap();
        print_indentation(writer, &context).await;
        print_attribute(writer, attribute, &context).await;
    }
}

/// Print a single [TagAttribute].
async fn print_attribute<'a, W: Write + Unpin + Send + ?Sized>(
    writer: &'a mut W,
    attribute: &'a TagAttribute,
    context: &'a PrintingContext<'a>,
) {
    match attribute {
        TagAttribute::HtmlAttribute(attribute) => {
            writer.write_all(attribute.name.as_bytes()).await.unwrap();

            if let Some(value) = &attribute.value {
                writer.write_all(b"=\"").await.unwrap();
                writer.write_all(value.as_bytes()).await.unwrap();
                writer.write_all(b"\"").await.unwrap();
            }
        }
        TagAttribute::TwigComment(twig_comment) => {
            writer.write_all(b"{# ").await.unwrap();
            writer
                .write_all(twig_comment.content.as_bytes())
                .await
                .unwrap();
            writer.write_all(b" #}").await.unwrap();
        }
        TagAttribute::TwigStructure(twig_structure) => {
            match twig_structure {
                TwigStructure::TwigBlock(t) => {
                    print_twig_block::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                        .await
                }
                TwigStructure::TwigFor(t) => {
                    print_twig_for::<_, TagAttribute, DynamicChildPrinter>(writer, t, context).await
                }
                TwigStructure::TwigIf(t) => {
                    print_twig_if::<_, TagAttribute, DynamicChildPrinter>(writer, t, context).await
                }
                TwigStructure::TwigApply(t) => {
                    print_twig_apply::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                        .await
                }
                TwigStructure::TwigSetCapture(t) => {
                    print_twig_set_capture::<_, TagAttribute, DynamicChildPrinter>(
                        writer, t, context,
                    )
                    .await
                }
            };
        }
    }
}

/// print a complete html tag with its attributes and children.
async fn print_tag<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    tag: &Tag,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;

    writer.write_all(b"<").await.unwrap();
    writer.write_all(tag.name.as_bytes()).await.unwrap();

    print_tag_attributes(writer, tag, context).await;

    if tag.self_closed {
        writer.write_all(b"/>").await.unwrap();
    } else {
        writer.write_all(b">").await.unwrap();
        // only print children if tag is not self_closed!
        print_node_list(
            writer,
            &tag.children,
            &context.increase_indentation_by_tabs(1),
        )
        .await;
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

/// print all the attributes of an html tag.
/// It does some calculations to print them in inline or continuation mode.
async fn print_tag_attributes<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    tag: &Tag,
    context: &PrintingContext<'_>,
) {
    let inline_mode =
        tag.attributes.len() <= 2 && calculate_tag_line_length(tag, context) <= MAX_LINE_LENGTH;
    let continuation_indent_mode = tag.name.len() >= MIN_TAG_NAME_LENGTH_FOR_CONTINUATION_INDENT;

    let context = if continuation_indent_mode {
        context.increase_indentation_by_tabs(2)
    } else {
        context.increase_indentation_by_spaces(tag.name.len() as u32 + 2)
    };

    // attributes
    for (index, attribute) in tag.attributes.iter().enumerate() {
        if inline_mode {
            writer.write_all(b" ").await.unwrap();
        } else if continuation_indent_mode {
            writer.write_all(b"\n").await.unwrap();
            print_indentation(writer, &context).await;
        } else {
            // write attribute on first line (same as tag)
            if index == 0 {
                writer.write_all(b" ").await.unwrap();
            } else {
                writer.write_all(b"\n").await.unwrap();

                print_indentation(writer, &context).await;
            }
        }

        print_attribute(writer, attribute, &context).await;
    }
}

async fn print_plain<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    plain: &Plain,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(plain.plain.as_bytes()).await.unwrap();
}

async fn print_html_comment<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &HtmlComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"<!-- ").await.unwrap();
    writer.write_all(comment.content.as_bytes()).await.unwrap();
    writer.write_all(b" -->").await.unwrap();
}

async fn print_vue_block<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    vue: &OutputExpression,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{{ ").await.unwrap();
    writer.write_all(vue.content.as_bytes()).await.unwrap();
    writer.write_all(b" }}").await.unwrap();
}

async fn print_twig_block<W, C, P>(
    writer: &mut W,
    twig: &TwigBlock<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context).await;
    }

    writer.write_all(b"{% block ").await.unwrap();
    writer.write_all(twig.name.as_bytes()).await.unwrap();
    writer.write_all(b" %}").await.unwrap();

    P::generic_print_children(
        writer,
        &twig.children,
        &context.increase_indentation_by_tabs(1),
    )
    .await;

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context).await;
            }
        }
    } else {
        writer.write_all(b"\n").await.unwrap();
        print_indentation(writer, context).await;
    }

    writer.write_all(b"{% endblock %}").await.unwrap();
}

async fn print_twig_for<W, C, P>(
    writer: &mut W,
    twig_for: &TwigFor<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context).await;
    }

    writer.write_all(b"{% for ").await.unwrap();
    writer
        .write_all(twig_for.expression.as_bytes())
        .await
        .unwrap();
    writer.write_all(b" %}").await.unwrap();

    P::generic_print_children(
        writer,
        &twig_for.children,
        &context.increase_indentation_by_tabs(1),
    )
    .await;

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_for.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context).await;
            }
        }
    } else {
        writer.write_all(b"\n").await.unwrap();
        print_indentation(writer, context).await;
    }

    writer.write_all(b"{% endfor %}").await.unwrap();
}

async fn print_twig_if<W, C, P>(writer: &mut W, twig_if: &TwigIf<C>, context: &PrintingContext<'_>)
where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    for (index, arm) in twig_if.if_arms.iter().enumerate() {
        match (index, &arm.expression) {
            (0, Some(e)) => {
                if P::is_whitespace_sensitive() {
                    print_indentation_if_whitespace_exists_before(writer, context).await;
                }

                writer.write_all(b"{% if ").await.unwrap();
                writer.write_all(e.as_bytes()).await.unwrap();
            }
            (_, Some(e)) => {
                writer.write_all(b"{% elseif ").await.unwrap();
                writer.write_all(e.as_bytes()).await.unwrap();
            }
            (_, None) => {
                writer.write_all(b"{% else").await.unwrap();
            }
        }

        writer.write_all(b" %}").await.unwrap();

        P::generic_print_children(
            writer,
            &arm.children,
            &context.increase_indentation_by_tabs(1),
        )
        .await;

        if P::is_whitespace_sensitive() {
            if let Some(last) = arm.children.last() {
                if last.is_whitespace() {
                    print_indentation(writer, context).await;
                }
            }
        } else {
            writer.write_all(b"\n").await.unwrap();
            print_indentation(writer, context).await;
        }
    }

    writer.write_all(b"{% endif %}").await.unwrap();
}

async fn print_twig_apply<W, C, P>(
    writer: &mut W,
    twig_apply: &TwigApply<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context).await;
    }

    writer.write_all(b"{% apply ").await.unwrap();
    writer
        .write_all(twig_apply.expression.as_bytes())
        .await
        .unwrap();
    writer.write_all(b" %}").await.unwrap();

    P::generic_print_children(
        writer,
        &twig_apply.children,
        &context.increase_indentation_by_tabs(1),
    )
    .await;

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_apply.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context).await;
            }
        }
    } else {
        writer.write_all(b"\n").await.unwrap();
        print_indentation(writer, context).await;
    }

    writer.write_all(b"{% endapply %}").await.unwrap();
}

async fn print_twig_set_capture<W, C, P>(
    writer: &mut W,
    twig_set_capture: &TwigSetCapture<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context).await;
    }

    writer.write_all(b"{% set ").await.unwrap();
    writer
        .write_all(twig_set_capture.name.as_bytes())
        .await
        .unwrap();
    writer.write_all(b" %}").await.unwrap();

    P::generic_print_children(
        writer,
        &twig_set_capture.children,
        &context.increase_indentation_by_tabs(1),
    )
    .await;

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_set_capture.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context).await;
            }
        }
    } else {
        writer.write_all(b"\n").await.unwrap();
        print_indentation(writer, context).await;
    }

    writer.write_all(b"{% endset %}").await.unwrap();
}

async fn print_twig_statement<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    statement: &TwigStatement,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{% ").await.unwrap();
    match statement {
        TwigStatement::Raw(raw) => {
            writer.write_all(raw.as_bytes()).await.unwrap();
        }
    }
    writer.write_all(b" %}").await.unwrap();
}

async fn print_twig_comment<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &TwigComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context).await;
    writer.write_all(b"{# ").await.unwrap();
    writer.write_all(comment.content.as_bytes()).await.unwrap();
    writer.write_all(b" #}").await.unwrap();
}

async fn print_whitespace<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    writer.write_all(b"\r\n").await.unwrap();

    // decide if additional whitespaces are needed (for example before and after twig blocks)
    check_and_print_additional_whitespace_around_twig_block(writer, context).await;
}

async fn check_and_print_additional_whitespace_around_twig_block<
    W: Write + Unpin + Send + ?Sized,
>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    if let Some(SyntaxNode::TwigStructure(TwigStructure::TwigBlock(p))) = context.get_parent() {
        // check for each child that it is either a twig block or a whitespace
        let child_checker = |c: &SyntaxNode| {
            matches!(c, SyntaxNode::TwigStructure(TwigStructure::TwigBlock(_)))
                || matches!(c, SyntaxNode::Whitespace)
        };

        if p.children.len() <= 3 && p.children.iter().take(3).all(child_checker) {
            // found a block with only one nested block (ignoring the optional whitespace around it)
            // in this special case there should be no extra whitespace for twig blocks.
            return;
        }
    }

    // check if the after or previous node is a twig block. In that case an extra whitespace is needed.
    if let Some(SyntaxNode::TwigStructure(TwigStructure::TwigBlock(_))) = context.after_node {
        // print another whitespace.
        writer.write_all(b"\r\n").await.unwrap();
    } else if let Some(SyntaxNode::TwigStructure(TwigStructure::TwigBlock(_))) =
        context.previous_node
    {
        // print another whitespace.
        writer.write_all(b"\r\n").await.unwrap();
    }
}

async fn print_indentation<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    let (tabs, spaces) = context.get_tabs_and_spaces();

    for _ in 0..tabs {
        // 1 tab = 4 spaces at once
        writer.write_all(b"    ").await.unwrap();
    }

    for _ in 0..spaces {
        writer.write_all(b" ").await.unwrap();
    }
}

async fn print_indentation_if_whitespace_exists_before<W: Write + Unpin + Send + ?Sized>(
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
    context.indentation_spaces as usize
        + 1
        + tag.name.len()
        + tag
            .attributes
            .iter()
            .take(2)
            .map(|a| {
                match a {
                    TagAttribute::HtmlAttribute(a) => {
                        1 + a.name.len() + a.value.as_ref().map(|v| v.len() + 3).unwrap_or(0)
                    }
                    _ => 1000, // don't allow inline mode if the first two attributes contain something else than a normal HtmlAttribute
                }
            })
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
    use async_std::io::Cursor;
    use ludtwig_parser::ast::{HtmlAttribute, TwigIfArm};

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

    #[async_std::test]
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

    #[async_std::test]
    async fn test_write_simple_twig_block() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "some_twig_block".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Plain(Plain {
                    plain: "Hello world".to_string(),
                }),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block some_twig_block %}\r\n    Hello world\r\n{% endblock %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_nested_twig_block() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "this_is_a_test_three".to_string(),
                            children: vec![SyntaxNode::Whitespace],
                        })),
                        SyntaxNode::Whitespace,
                    ],
                })),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\r\n    {% block this_is_a_test_two %}\r\n        {% block this_is_a_test_three %}\r\n        {% endblock %}\r\n    {% endblock %}\r\n{% endblock %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_nested_twig_block_separation() {
        let tree = SyntaxNode::Tag(Tag {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStatement(TwigStatement::Raw("parent".to_string())),
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "Some content".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                })),
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_three".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "Some content".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                })),
                SyntaxNode::Whitespace,
            ],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<this_is_a_test_one>\r\n\r\n    {% block this_is_a_test_two %}\r\n        {% parent %}\r\n        Some content\r\n    {% endblock %}\r\n\r\n    {% block this_is_a_test_three %}\r\n        Some content\r\n    {% endblock %}\r\n\r\n</this_is_a_test_one>".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_nested_twig_block_separation_edge_case() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "some_content_block_1".to_string(),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Plain(Plain {
                                    plain: "content".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        })),
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "some_content_block_2".to_string(),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Plain(Plain {
                                    plain: "content".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        })),
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "some_content_block_3".to_string(),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Plain(Plain {
                                    plain: "content".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        })),
                        SyntaxNode::Whitespace,
                    ],
                })),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\r\n    {% block this_is_a_test_two %}\r\n\r\n        {% block some_content_block_1 %}\r\n            content\r\n        {% endblock %}\r\n\r\n        {% block some_content_block_2 %}\r\n            content\r\n        {% endblock %}\r\n\r\n        {% block some_content_block_3 %}\r\n            content\r\n        {% endblock %}\r\n\r\n    {% endblock %}\r\n{% endblock %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_empty_twig_block() {
        let tree = SyntaxNode::Tag(Tag {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![],
                })),
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

    #[async_std::test]
    async fn test_write_tag_and_twig_block_without_whitespace() {
        let tree = SyntaxNode::Tag(Tag {
            name: "slot".to_string(),
            children: vec![SyntaxNode::TwigStructure(TwigStructure::TwigBlock(
                TwigBlock {
                    name: "sw_grid_slot_pagination".to_string(),
                    children: vec![],
                },
            ))],
            attributes: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "name".to_string(),
                value: Some("pagination".to_string()),
            })],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}{% endblock %}</slot>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_and_twig_block_content_without_whitespace() {
        let tree = SyntaxNode::Tag(Tag {
            name: "slot".to_string(),
            children: vec![SyntaxNode::TwigStructure(TwigStructure::TwigBlock(
                TwigBlock {
                    name: "sw_grid_slot_pagination".to_string(),
                    children: vec![SyntaxNode::Plain(Plain {
                        plain: "Hello world".to_string(),
                    })],
                },
            ))],
            attributes: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                name: "name".to_string(),
                value: Some("pagination".to_string()),
            })],
            ..Default::default()
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}Hello world{% endblock %}</slot>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_for() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigFor(TwigFor {
            expression: "item in items".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::OutputExpression(OutputExpression {
                    content: "item".to_string(),
                }),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% for item in items %}\r\n    {{ item }}\r\n{% endfor %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_if() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
            if_arms: vec![TwigIfArm {
                expression: Some("a > b".to_string()),
                children: vec![
                    SyntaxNode::Whitespace,
                    SyntaxNode::OutputExpression(OutputExpression {
                        content: "a".to_string(),
                    }),
                    SyntaxNode::Whitespace,
                ],
            }],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% if a > b %}\r\n    {{ a }}\r\n{% endif %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_if_else() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
            if_arms: vec![
                TwigIfArm {
                    expression: Some("a > b".to_string()),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "a".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                },
                TwigIfArm {
                    expression: None,
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "b".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                },
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% if a > b %}\r\n    {{ a }}\r\n{% else %}\r\n    {{ b }}\r\n{% endif %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_if_elseif_else() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
            if_arms: vec![
                TwigIfArm {
                    expression: Some("a > b".to_string()),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "a".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                },
                TwigIfArm {
                    expression: Some("a == b".to_string()),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::OutputExpression(OutputExpression {
                            content: "b".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                },
                TwigIfArm {
                    expression: None,
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Plain(Plain {
                            plain: "TODO".to_string(),
                        }),
                        SyntaxNode::Whitespace,
                    ],
                },
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% if a > b %}\r\n    {{ a }}\r\n{% elseif a == b %}\r\n    {{ b }}\r\n{% else %}\r\n    TODO\r\n{% endif %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_if_elseif_else_in_twig_block() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "my_block".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("a > b".to_string()),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::OutputExpression(OutputExpression {
                                    content: "a".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        },
                        TwigIfArm {
                            expression: Some("a == b".to_string()),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::OutputExpression(OutputExpression {
                                    content: "b".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Plain(Plain {
                                    plain: "TODO".to_string(),
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        },
                    ],
                })),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% block my_block %}\r\n    {% if a > b %}\r\n        {{ a }}\r\n    {% elseif a == b %}\r\n        {{ b }}\r\n    {% else %}\r\n        TODO\r\n    {% endif %}\r\n{% endblock %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_apply() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigApply(TwigApply {
            expression: "upper".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Plain(Plain {
                    plain: "hello world".to_string(),
                }),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% apply upper %}\r\n    hello world\r\n{% endapply %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_twig_set_capture() {
        let tree = SyntaxNode::TwigStructure(TwigStructure::TwigSetCapture(TwigSetCapture {
            name: "myVariable".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Plain(Plain {
                    plain: "hello world".to_string(),
                }),
                SyntaxNode::Whitespace,
            ],
        }));

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "{% set myVariable %}\r\n    hello world\r\n{% endset %}".to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_if_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![TwigIfArm {
                        expression: Some("isDisabled".to_string()),
                        children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                            name: "disabled".to_string(),
                            value: None,
                        })],
                    }],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% endif %}></div>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_if_else_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("isDisabled".to_string()),
                            children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "disabled".to_string(),
                                value: None,
                            })],
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "focus".to_string(),
                                value: None,
                            })],
                        },
                    ],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% else %}\n         focus\n     {% endif %}></div>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_if_elseif_else_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("isDisabled".to_string()),
                            children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "disabled".to_string(),
                                value: None,
                            })],
                        },
                        TwigIfArm {
                            expression: Some("isFocus".to_string()),
                            children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "focus".to_string(),
                                value: None,
                            })],
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![TagAttribute::TwigComment(TwigComment {
                                content: "Nothing for now".to_string(),
                            })],
                        },
                    ],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% elseif isFocus %}\n         focus\n     {% else %}\n         {# Nothing for now #}\n     {% endif %}></div>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_nested_if_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                    if_arms: vec![
                        TwigIfArm {
                            expression: Some("isDisabled".to_string()),
                            children: vec![TagAttribute::TwigStructure(TwigStructure::TwigIf(
                                TwigIf {
                                    if_arms: vec![TwigIfArm {
                                        expression: Some("!isFocus".to_string()),
                                        children: vec![TagAttribute::HtmlAttribute(
                                            HtmlAttribute {
                                                name: "disabled".to_string(),
                                                value: None,
                                            },
                                        )],
                                    }],
                                },
                            ))],
                        },
                        TwigIfArm {
                            expression: None,
                            children: vec![TagAttribute::TwigComment(TwigComment {
                                content: "Nothing for now".to_string(),
                            })],
                        },
                    ],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         {% if !isFocus %}\n             disabled\n         {% endif %}\n     {% else %}\n         {# Nothing for now #}\n     {% endif %}></div>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_block_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "my_custom_attribute_block".to_string(),
                    children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                        name: "disabled".to_string(),
                        value: None,
                    })],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% block my_custom_attribute_block %}\n         disabled\n     {% endblock %}></div>"
                .to_string()
        );
    }

    #[async_std::test]
    async fn test_write_tag_with_twig_block_with_if_attribute() {
        let tree = SyntaxNode::Tag(Tag {
            name: "div".to_string(),
            self_closed: false,
            attributes: vec![
                TagAttribute::HtmlAttribute(HtmlAttribute {
                    name: "class".to_string(),
                    value: Some("hello".to_string()),
                }),
                TagAttribute::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "my_custom_attribute_block".to_string(),
                    children: vec![TagAttribute::TwigStructure(TwigStructure::TwigIf(TwigIf {
                        if_arms: vec![TwigIfArm {
                            expression: Some("isDisabled".to_string()),
                            children: vec![TagAttribute::HtmlAttribute(HtmlAttribute {
                                name: "disabled".to_string(),
                                value: None,
                            })],
                        }],
                    }))],
                })),
            ],
            children: vec![],
        });

        let res = convert_tree_into_written_string(tree).await;

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% block my_custom_attribute_block %}\n         {% if isDisabled %}\n             disabled\n         {% endif %}\n     {% endblock %}></div>"
                .to_string()
        );
    }
}

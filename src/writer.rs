use crate::config::{Config, IndentationMode, LineEnding};
use crate::process::FileContext;
use ludtwig_parser::ast::{
    HtmlComment, OutputExpression, Plain, SyntaxNode, Tag, TagAttribute, TwigApply, TwigBlock,
    TwigComment, TwigFor, TwigIf, TwigSetCapture, TwigStatement, TwigStructure,
};
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Context for traversing the AST with printing in mind.
#[derive(Debug, Clone)]
struct PrintingContext<'a> {
    previous_node: Option<&'a SyntaxNode>,
    after_node: Option<&'a SyntaxNode>,

    /// the last node in the list is the current node. everything before that is up in the hierarchy.
    parent_nodes: Vec<&'a SyntaxNode>,

    /// in 'tab' count (a tab can be configured in the config)
    indentation_tabs: u32,

    /// in spaces count (gets added to the 'tab' count for more precise placement in attributes for example)
    indentation_spaces: u32,

    /// reference to context
    file_context: &'a FileContext,
}

impl<'a> PrintingContext<'a> {
    /// Clones the current context and returns a new one with the increased indentation.
    fn increase_indentation_by_tabs(&self, tabs: u32) -> Self {
        let mut copy = self.clone();
        copy.indentation_tabs += tabs;
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
        (self.indentation_tabs, self.indentation_spaces)
    }

    fn get_config(&self) -> &Config {
        &self.file_context.cli_context.config
    }

    fn get_line_break_bytes(&self) -> &[u8] {
        const LF: &[u8; 1] = b"\n";
        const CRLF: &[u8; 2] = b"\r\n";

        match self.get_config().format.line_ending {
            LineEnding::UnixLF => LF,
            LineEnding::WindowsCRLF => CRLF,
        }
    }
}

/// Trait that allows to print a generic list of children.
trait GenericChildPrinter<T: IsWhitespace> {
    fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
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
impl GenericChildPrinter<SyntaxNode> for DynamicChildPrinter {
    fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
        writer: &mut W,
        nodes: &[SyntaxNode],
        context: &PrintingContext<'_>,
    ) {
        print_node_list(writer, nodes, context);
    }
}

/// In case of [TagAttribute]
impl GenericChildPrinter<TagAttribute> for DynamicChildPrinter {
    fn generic_print_children<W: Write + Unpin + Send + ?Sized>(
        writer: &mut W,
        nodes: &[TagAttribute],
        context: &PrintingContext<'_>,
    ) {
        print_attribute_list(writer, nodes, context);
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
pub fn write_tree(file_context: Arc<FileContext>) {
    let path = create_and_secure_output_path(&file_context);
    let file = File::create(path).expect("can't create file.");
    let mut writer = BufWriter::new(file);

    print_node(
        &mut writer,
        &file_context.tree,
        &mut PrintingContext {
            previous_node: None,
            after_node: None,
            parent_nodes: vec![],
            indentation_tabs: 0,
            indentation_spaces: 0,
            file_context: &file_context,
        },
    );

    writer.flush().unwrap();
}

/// Append the CLI output path if it is given and
/// create all directories if they do not exists.
/// Returns the full path which is secure to use.
fn create_and_secure_output_path(file_context: &FileContext) -> PathBuf {
    let base_path = match &file_context.cli_context.output_path {
        None => Path::new(""),
        Some(p) => p,
    };
    let path = base_path.join(&*file_context.file_path);

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("can't create directory for output");
    }

    path
}

/// Print a single [SyntaxNode] from the AST, which can be anything.
fn print_node<'a, W: Write + Unpin + Send + ?Sized>(
    writer: &'a mut W,
    node: &'a SyntaxNode,
    context: &'a mut PrintingContext<'a>,
) {
    context.parent_nodes.push(&node);

    match node {
        SyntaxNode::Whitespace => {
            print_whitespace(writer, context);
        }
        SyntaxNode::Tag(tag) => {
            print_tag(writer, &tag, context);
        }
        SyntaxNode::Plain(plain) => {
            print_plain(writer, &plain, context);
        }
        SyntaxNode::HtmlComment(comment) => {
            print_html_comment(writer, comment, context);
        }
        SyntaxNode::OutputExpression(vue) => {
            print_vue_block(writer, &vue, context);
        }
        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(block)) => {
            print_twig_block::<_, SyntaxNode, DynamicChildPrinter>(writer, &block, context);
        }
        SyntaxNode::TwigStructure(TwigStructure::TwigFor(twig_for)) => {
            print_twig_for::<_, SyntaxNode, DynamicChildPrinter>(writer, &twig_for, context);
        }
        SyntaxNode::TwigStructure(TwigStructure::TwigIf(twig_if)) => {
            print_twig_if::<_, SyntaxNode, DynamicChildPrinter>(writer, &twig_if, context);
        }
        SyntaxNode::TwigStatement(statement) => {
            print_twig_statement(writer, &statement, context);
        }
        SyntaxNode::TwigComment(comment) => {
            print_twig_comment(writer, comment, context);
        }
        SyntaxNode::TwigStructure(TwigStructure::TwigApply(twig_apply)) => {
            print_twig_apply::<_, SyntaxNode, DynamicChildPrinter>(writer, &twig_apply, context);
        }
        SyntaxNode::TwigStructure(TwigStructure::TwigSetCapture(twig_set_capture)) => {
            print_twig_set_capture::<_, SyntaxNode, DynamicChildPrinter>(
                writer,
                &twig_set_capture,
                context,
            );
        }
        SyntaxNode::Root(root) => {
            print_node_list(writer, &root, context);
        }
    }
}

/// Print a list of [SyntaxNode]'s and prepare the context for each one.
fn print_node_list<W: Write + Unpin + Send + ?Sized>(
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
            indentation_tabs: context.indentation_tabs,
            indentation_spaces: context.indentation_spaces,
            file_context: context.file_context,
        };

        print_node(writer, current, &mut context);
    }
}

/// Print a list of [TagAttribute]'s. this is generally called by [TwigStructure<TagAttribute>]
/// and not by an [SyntaxNode::Tag] directly (because it does not do any calculations for
/// inline and continuation mode).
fn print_attribute_list<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    attributes: &[TagAttribute],
    context: &PrintingContext<'_>,
) {
    for attribute in attributes {
        writer.write_all(context.get_line_break_bytes()).unwrap();
        print_indentation(writer, &context);
        print_attribute(writer, attribute, &context);
    }
}

/// Print a single [TagAttribute].
fn print_attribute<'a, W: Write + Unpin + Send + ?Sized>(
    writer: &'a mut W,
    attribute: &'a TagAttribute,
    context: &'a PrintingContext<'a>,
) {
    match attribute {
        TagAttribute::HtmlAttribute(attribute) => {
            writer.write_all(attribute.name.as_bytes()).unwrap();

            if let Some(value) = &attribute.value {
                writer.write_all(b"=\"").unwrap();
                writer.write_all(value.as_bytes()).unwrap();
                writer.write_all(b"\"").unwrap();
            }
        }
        TagAttribute::TwigComment(twig_comment) => {
            writer.write_all(b"{# ").unwrap();
            writer.write_all(twig_comment.content.as_bytes()).unwrap();
            writer.write_all(b" #}").unwrap();
        }
        TagAttribute::TwigStructure(twig_structure) => {
            match twig_structure {
                TwigStructure::TwigBlock(t) => {
                    print_twig_block::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                }
                TwigStructure::TwigFor(t) => {
                    print_twig_for::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                }
                TwigStructure::TwigIf(t) => {
                    print_twig_if::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                }
                TwigStructure::TwigApply(t) => {
                    print_twig_apply::<_, TagAttribute, DynamicChildPrinter>(writer, t, context)
                }
                TwigStructure::TwigSetCapture(t) => {
                    print_twig_set_capture::<_, TagAttribute, DynamicChildPrinter>(
                        writer, t, context,
                    )
                }
            };
        }
    }
}

/// print a complete html tag with its attributes and children.
fn print_tag<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    tag: &Tag,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);

    writer.write_all(b"<").unwrap();
    writer.write_all(tag.name.as_bytes()).unwrap();

    print_tag_attributes(writer, tag, context);

    if tag.self_closed {
        writer.write_all(b"/>").unwrap();
    } else {
        writer.write_all(b">").unwrap();
        // only print children if tag is not self_closed!
        print_node_list(
            writer,
            &tag.children,
            &context.increase_indentation_by_tabs(1),
        );
    }

    if let Some(SyntaxNode::Whitespace) = tag.children.last() {
        print_indentation(writer, context);
    }

    if !tag.self_closed {
        writer.write_all(b"</").unwrap();
        writer.write_all(tag.name.as_bytes()).unwrap();
        writer.write_all(b">").unwrap();
    }
}

/// print all the attributes of an html tag.
/// It does some calculations to print them in inline or continuation mode.
fn print_tag_attributes<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    tag: &Tag,
    context: &PrintingContext<'_>,
) {
    let inline_mode = tag.attributes.len()
        <= context.get_config().format.attribute_inline_max_count as usize
        && calculate_tag_line_length(tag, context)
            <= context.get_config().format.preferred_max_line_length as usize;
    let continuation_indent_length = match context.get_config().format.indentation_mode {
        IndentationMode::Space => context.get_config().format.indentation_count as usize * 2,
        IndentationMode::Tab => 4 * context.get_config().format.indentation_count as usize * 2,
    };
    let continuation_indent_mode = tag.name.len() > continuation_indent_length;

    let context = if continuation_indent_mode {
        context.increase_indentation_by_tabs(2)
    } else {
        context.increase_indentation_by_spaces(tag.name.len() as u32 + 2)
    };

    // attributes
    for (index, attribute) in tag.attributes.iter().enumerate() {
        if inline_mode {
            writer.write_all(b" ").unwrap();
        } else if continuation_indent_mode {
            writer.write_all(context.get_line_break_bytes()).unwrap();
            print_indentation(writer, &context);
        } else {
            // write attribute on first line (same as tag)
            if index == 0 {
                writer.write_all(b" ").unwrap();
            } else {
                writer.write_all(context.get_line_break_bytes()).unwrap();

                print_indentation(writer, &context);
            }
        }

        print_attribute(writer, attribute, &context);
    }
}

fn print_plain<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    plain: &Plain,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);
    writer.write_all(plain.plain.as_bytes()).unwrap();
}

fn print_html_comment<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &HtmlComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);
    writer.write_all(b"<!-- ").unwrap();
    writer.write_all(comment.content.as_bytes()).unwrap();
    writer.write_all(b" -->").unwrap();
}

fn print_vue_block<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    vue: &OutputExpression,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);
    writer.write_all(b"{{ ").unwrap();
    writer.write_all(vue.content.as_bytes()).unwrap();
    writer.write_all(b" }}").unwrap();
}

fn print_twig_block<W, C, P>(writer: &mut W, twig: &TwigBlock<C>, context: &PrintingContext<'_>)
where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context);
    }

    writer.write_all(b"{% block ").unwrap();
    writer.write_all(twig.name.as_bytes()).unwrap();
    writer.write_all(b" %}").unwrap();

    let child_context = match context.get_config().format.indent_children_of_blocks {
        true => context.increase_indentation_by_tabs(1),
        false => context.clone(),
    };

    P::generic_print_children(writer, &twig.children, &child_context);

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context);
            }
        }
    } else {
        writer.write_all(context.get_line_break_bytes()).unwrap();
        print_indentation(writer, context);
    }

    writer.write_all(b"{% endblock %}").unwrap();
}

fn print_twig_for<W, C, P>(writer: &mut W, twig_for: &TwigFor<C>, context: &PrintingContext<'_>)
where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context);
    }

    writer.write_all(b"{% for ").unwrap();
    writer.write_all(twig_for.expression.as_bytes()).unwrap();
    writer.write_all(b" %}").unwrap();

    P::generic_print_children(
        writer,
        &twig_for.children,
        &context.increase_indentation_by_tabs(1),
    );

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_for.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context);
            }
        }
    } else {
        writer.write_all(context.get_line_break_bytes()).unwrap();
        print_indentation(writer, context);
    }

    writer.write_all(b"{% endfor %}").unwrap();
}

fn print_twig_if<W, C, P>(writer: &mut W, twig_if: &TwigIf<C>, context: &PrintingContext<'_>)
where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    for (index, arm) in twig_if.if_arms.iter().enumerate() {
        match (index, &arm.expression) {
            (0, Some(e)) => {
                if P::is_whitespace_sensitive() {
                    print_indentation_if_whitespace_exists_before(writer, context);
                }

                writer.write_all(b"{% if ").unwrap();
                writer.write_all(e.as_bytes()).unwrap();
            }
            (_, Some(e)) => {
                writer.write_all(b"{% elseif ").unwrap();
                writer.write_all(e.as_bytes()).unwrap();
            }
            (_, None) => {
                writer.write_all(b"{% else").unwrap();
            }
        }

        writer.write_all(b" %}").unwrap();

        P::generic_print_children(
            writer,
            &arm.children,
            &context.increase_indentation_by_tabs(1),
        );

        if P::is_whitespace_sensitive() {
            if let Some(last) = arm.children.last() {
                if last.is_whitespace() {
                    print_indentation(writer, context);
                }
            }
        } else {
            writer.write_all(context.get_line_break_bytes()).unwrap();
            print_indentation(writer, context);
        }
    }

    writer.write_all(b"{% endif %}").unwrap();
}

fn print_twig_apply<W, C, P>(
    writer: &mut W,
    twig_apply: &TwigApply<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context);
    }

    writer.write_all(b"{% apply ").unwrap();
    writer.write_all(twig_apply.expression.as_bytes()).unwrap();
    writer.write_all(b" %}").unwrap();

    P::generic_print_children(
        writer,
        &twig_apply.children,
        &context.increase_indentation_by_tabs(1),
    );

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_apply.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context);
            }
        }
    } else {
        writer.write_all(context.get_line_break_bytes()).unwrap();
        print_indentation(writer, context);
    }

    writer.write_all(b"{% endapply %}").unwrap();
}

fn print_twig_set_capture<W, C, P>(
    writer: &mut W,
    twig_set_capture: &TwigSetCapture<C>,
    context: &PrintingContext<'_>,
) where
    W: Write + Unpin + Send + ?Sized,
    C: IsWhitespace,
    P: GenericChildPrinter<C>,
{
    if P::is_whitespace_sensitive() {
        print_indentation_if_whitespace_exists_before(writer, context);
    }

    writer.write_all(b"{% set ").unwrap();
    writer.write_all(twig_set_capture.name.as_bytes()).unwrap();
    writer.write_all(b" %}").unwrap();

    P::generic_print_children(
        writer,
        &twig_set_capture.children,
        &context.increase_indentation_by_tabs(1),
    );

    if P::is_whitespace_sensitive() {
        if let Some(last) = twig_set_capture.children.last() {
            if last.is_whitespace() {
                print_indentation(writer, context);
            }
        }
    } else {
        writer.write_all(context.get_line_break_bytes()).unwrap();
        print_indentation(writer, context);
    }

    writer.write_all(b"{% endset %}").unwrap();
}

fn print_twig_statement<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    statement: &TwigStatement,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);
    writer.write_all(b"{% ").unwrap();
    match statement {
        TwigStatement::Raw(raw) => {
            writer.write_all(raw.as_bytes()).unwrap();
        }
    }
    writer.write_all(b" %}").unwrap();
}

fn print_twig_comment<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    comment: &TwigComment,
    context: &PrintingContext<'_>,
) {
    print_indentation_if_whitespace_exists_before(writer, context);
    writer.write_all(b"{# ").unwrap();
    writer.write_all(comment.content.as_bytes()).unwrap();
    writer.write_all(b" #}").unwrap();
}

fn print_whitespace<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    writer.write_all(context.get_line_break_bytes()).unwrap();

    if context.get_config().format.linebreaks_around_blocks {
        // decide if additional whitespaces are needed (for example before and after twig blocks)
        check_and_print_additional_whitespace_around_twig_block(writer, context);
    }
}

fn check_and_print_additional_whitespace_around_twig_block<W: Write + Unpin + Send + ?Sized>(
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
        writer.write_all(context.get_line_break_bytes()).unwrap();
    } else if let Some(SyntaxNode::TwigStructure(TwigStructure::TwigBlock(_))) =
        context.previous_node
    {
        // print another whitespace.
        writer.write_all(context.get_line_break_bytes()).unwrap();
    }
}

fn print_indentation<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    let (tabs, spaces) = context.get_tabs_and_spaces();

    for _ in 0..tabs {
        for _ in 0..context.get_config().format.indentation_count {
            match context.get_config().format.indentation_mode {
                IndentationMode::Space => {
                    writer.write_all(b" ").unwrap();
                }
                IndentationMode::Tab => {
                    writer.write_all(b"\t").unwrap();
                }
            }
        }
    }

    for _ in 0..spaces {
        writer.write_all(b" ").unwrap();
    }
}

fn print_indentation_if_whitespace_exists_before<W: Write + Unpin + Send + ?Sized>(
    writer: &mut W,
    context: &PrintingContext<'_>,
) {
    if let Some(SyntaxNode::Whitespace) = context.previous_node {
        print_indentation(writer, context);
    }
}

/// Calculates the line length including indentation but only with a maximum of MAX attributes
/// (everything above that will be ignored and should result in not inlining anyways).
/// It is possible that this function returns a very high line length (>1000) if this is
/// a inline tag without whitespaces (like `<span>Hello</span>`)
fn calculate_tag_line_length(tag: &Tag, context: &PrintingContext) -> usize {
    4 * context.indentation_tabs as usize
        + context.indentation_spaces as usize
        + 1
        + tag.name.len()
        + tag
            .attributes
            .iter()
            .take(context.get_config().format.attribute_inline_max_count as usize) // only count the length of up to max attributes (otherwise it can't be written in one line anyways)
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
    use crate::config::Format;
    use crate::{config, CliContext};
    use ludtwig_parser::ast::{HtmlAttribute, TwigIfArm};
    use std::io::Cursor;
    use std::sync::mpsc;

    /*
    The input or output data for testing purposes is partially from the following sources and under copyright!
    It is not included in the built binaries. Keep the licenses in mind if you use these strings (MIT as of 12.12.2020)!

    Copyright (c) shopware AG (https://github.com/shopware/platform)
    Copyright (c) shopware AG (https://github.com/shopware/SwagMigrationAssistant)
     */

    fn create_default_config() -> Config {
        Config::new(config::DEFAULT_CONFIG_PATH).expect("can't create default config")
    }

    fn convert_tree_into_written_string(tree: SyntaxNode, config: &Config) -> String {
        let mut writer_raw: Cursor<Vec<u8>> = Cursor::new(Vec::new());

        let (tx, _) = mpsc::sync_channel(1);
        let mut context = PrintingContext {
            previous_node: None,
            after_node: None,
            parent_nodes: vec![],
            indentation_tabs: 0,
            indentation_spaces: 0,
            file_context: &FileContext {
                cli_context: Arc::new(CliContext {
                    output_tx: tx,
                    no_analysis: false,
                    no_writing: false,
                    output_path: None,
                    config: config.to_owned(),
                }),
                file_path: Default::default(),
                tree: SyntaxNode::Root(vec![]), // not used during write
            },
        };

        print_node(&mut writer_raw, &tree, &mut context);

        String::from_utf8(writer_raw.into_inner()).unwrap()
    }

    fn create_tree_for_config_tests() -> SyntaxNode {
        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
            name: "this_is_a_test_one".to_string(),
            children: vec![
                SyntaxNode::Whitespace,
                SyntaxNode::Tag(Tag {
                    name: "div".to_string(),
                    self_closed: false,
                    attributes: vec![
                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                            "id".to_string(),
                            Some("customized-thing".to_string()),
                        )),
                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                            "class".to_string(),
                            Some("thing".to_string()),
                        )),
                    ],
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::Tag(Tag {
                            name: "span".to_string(),
                            self_closed: false,
                            attributes: vec![],
                            children: vec![SyntaxNode::Plain(Plain::new(
                                "Whitespace sensitive".to_string(),
                            ))],
                        }),
                        SyntaxNode::Whitespace,
                    ],
                }),
                SyntaxNode::Whitespace,
                SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                    name: "this_is_a_test_two".to_string(),
                    children: vec![
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "some_content_block_1".to_string(),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Tag(Tag {
                                    name: "h2".to_string(),
                                    self_closed: false,
                                    attributes: vec![
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "class".to_string(),
                                            Some("headline".to_string()),
                                        )),
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "@click".to_string(),
                                            Some("onHeadlineClick".to_string()),
                                        )),
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "v-if".to_string(),
                                            Some("showHeadline".to_string()),
                                        )),
                                    ],
                                    children: vec![
                                        SyntaxNode::Whitespace,
                                        SyntaxNode::Plain(Plain {
                                            plain: "hello".to_string(),
                                        }),
                                        SyntaxNode::Whitespace,
                                    ],
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        })),
                        SyntaxNode::Whitespace,
                        SyntaxNode::TwigStructure(TwigStructure::TwigBlock(TwigBlock {
                            name: "some_content_block_2".to_string(),
                            children: vec![
                                SyntaxNode::Whitespace,
                                SyntaxNode::Tag(Tag {
                                    name: "my-custom-component-that-is-long".to_string(),
                                    self_closed: false,
                                    attributes: vec![
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "class".to_string(),
                                            Some("component".to_string()),
                                        )),
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "@click".to_string(),
                                            Some("onComponentClick".to_string()),
                                        )),
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "v-if".to_string(),
                                            Some("showComponent".to_string()),
                                        )),
                                        TagAttribute::HtmlAttribute(HtmlAttribute::new(
                                            "v-model".to_string(),
                                            Some("data".to_string()),
                                        )),
                                    ],
                                    children: vec![SyntaxNode::Whitespace],
                                }),
                                SyntaxNode::Whitespace,
                            ],
                        })),
                        SyntaxNode::Whitespace,
                    ],
                })),
                SyntaxNode::Whitespace,
            ],
        }))
    }

    #[test]
    fn test_write_empty_html_tag() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<this_is_a_test_one>\n    <this_is_a_test_two></this_is_a_test_two>\n</this_is_a_test_one>"
        );
    }

    #[test]
    fn test_write_simple_twig_block() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block some_twig_block %}\n    Hello world\n{% endblock %}"
        );
    }

    #[test]
    fn test_write_nested_twig_block() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n    {% block this_is_a_test_two %}\n        {% block this_is_a_test_three %}\n        {% endblock %}\n    {% endblock %}\n{% endblock %}"
        );
    }

    #[test]
    fn test_write_nested_twig_block_separation() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<this_is_a_test_one>\n\n    {% block this_is_a_test_two %}\n        {% parent %}\n        Some content\n    {% endblock %}\n\n    {% block this_is_a_test_three %}\n        Some content\n    {% endblock %}\n\n</this_is_a_test_one>"
        );
    }

    #[test]
    fn test_write_nested_twig_block_separation_edge_case() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n    {% block this_is_a_test_two %}\n\n        {% block some_content_block_1 %}\n            content\n        {% endblock %}\n\n        {% block some_content_block_2 %}\n            content\n        {% endblock %}\n\n        {% block some_content_block_3 %}\n            content\n        {% endblock %}\n\n    {% endblock %}\n{% endblock %}"
        );
    }

    #[test]
    fn test_write_empty_twig_block() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<this_is_a_test_one>\n\n    {% block this_is_a_test_two %}{% endblock %}\n\n</this_is_a_test_one>"
        );
    }

    #[test]
    fn test_write_tag_and_twig_block_without_whitespace() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}{% endblock %}</slot>"
        );
    }

    #[test]
    fn test_write_tag_and_twig_block_content_without_whitespace() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<slot name=\"pagination\">{% block sw_grid_slot_pagination %}Hello world{% endblock %}</slot>"
        );
    }

    #[test]
    fn test_write_twig_for() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(res, "{% for item in items %}\n    {{ item }}\n{% endfor %}");
    }

    #[test]
    fn test_write_twig_if() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(res, "{% if a > b %}\n    {{ a }}\n{% endif %}");
    }

    #[test]
    fn test_write_twig_if_else() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% if a > b %}\n    {{ a }}\n{% else %}\n    {{ b }}\n{% endif %}"
        );
    }

    #[test]
    fn test_write_twig_if_elseif_else() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% if a > b %}\n    {{ a }}\n{% elseif a == b %}\n    {{ b }}\n{% else %}\n    TODO\n{% endif %}"
        );
    }

    #[test]
    fn test_write_twig_if_elseif_else_in_twig_block() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block my_block %}\n    {% if a > b %}\n        {{ a }}\n    {% elseif a == b %}\n        {{ b }}\n    {% else %}\n        TODO\n    {% endif %}\n{% endblock %}"
        );
    }

    #[test]
    fn test_write_twig_apply() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(res, "{% apply upper %}\n    hello world\n{% endapply %}");
    }

    #[test]
    fn test_write_twig_set_capture() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(res, "{% set myVariable %}\n    hello world\n{% endset %}");
    }

    #[test]
    fn test_write_tag_with_twig_if_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% endif %}></div>"
        );
    }

    #[test]
    fn test_write_tag_with_twig_if_else_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% else %}\n         focus\n     {% endif %}></div>"
        );
    }

    #[test]
    fn test_write_tag_with_twig_if_elseif_else_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         disabled\n     {% elseif isFocus %}\n         focus\n     {% else %}\n         {# Nothing for now #}\n     {% endif %}></div>"
        );
    }

    #[test]
    fn test_write_tag_with_twig_nested_if_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% if isDisabled %}\n         {% if !isFocus %}\n             disabled\n         {% endif %}\n     {% else %}\n         {# Nothing for now #}\n     {% endif %}></div>"
        );
    }

    #[test]
    fn test_write_tag_with_twig_block_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% block my_custom_attribute_block %}\n         disabled\n     {% endblock %}></div>"
        );
    }

    #[test]
    fn test_write_tag_with_twig_block_with_if_attribute() {
        let config = create_default_config();
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

        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "<div class=\"hello\"\n     {% block my_custom_attribute_block %}\n         {% if isDisabled %}\n             disabled\n         {% endif %}\n     {% endblock %}></div>"
        );
    }

    #[test]
    fn test_config_line_ending_windows() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::WindowsCRLF,
                indentation_mode: IndentationMode::Space,
                indentation_count: 4,
                preferred_max_line_length: 120,
                attribute_inline_max_count: 2,
                indent_children_of_blocks: true,
                linebreaks_around_blocks: true,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\r\n    <div id=\"customized-thing\" class=\"thing\">\r\n        <span>Whitespace sensitive</span>\r\n    </div>\r\n\r\n    {% block this_is_a_test_two %}\r\n\r\n        {% block some_content_block_1 %}\r\n            <h2 class=\"headline\"\r\n                @click=\"onHeadlineClick\"\r\n                v-if=\"showHeadline\">\r\n                hello\r\n            </h2>\r\n        {% endblock %}\r\n\r\n        {% block some_content_block_2 %}\r\n            <my-custom-component-that-is-long\r\n                    class=\"component\"\r\n                    @click=\"onComponentClick\"\r\n                    v-if=\"showComponent\"\r\n                    v-model=\"data\">\r\n            </my-custom-component-that-is-long>\r\n        {% endblock %}\r\n\r\n    {% endblock %}\r\n\r\n{% endblock %}"
        );
    }

    #[test]
    fn test_config_indentation_mode_and_indentation_count_with_tabs() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::UnixLF,
                indentation_mode: IndentationMode::Tab,
                indentation_count: 1,
                preferred_max_line_length: 120,
                attribute_inline_max_count: 2,
                indent_children_of_blocks: true,
                linebreaks_around_blocks: true,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n\t<div id=\"customized-thing\" class=\"thing\">\n\t\t<span>Whitespace sensitive</span>\n\t</div>\n\n\t{% block this_is_a_test_two %}\n\n\t\t{% block some_content_block_1 %}\n\t\t\t<h2 class=\"headline\"\n\t\t\t    @click=\"onHeadlineClick\"\n\t\t\t    v-if=\"showHeadline\">\n\t\t\t\thello\n\t\t\t</h2>\n\t\t{% endblock %}\n\n\t\t{% block some_content_block_2 %}\n\t\t\t<my-custom-component-that-is-long\n\t\t\t\t\tclass=\"component\"\n\t\t\t\t\t@click=\"onComponentClick\"\n\t\t\t\t\tv-if=\"showComponent\"\n\t\t\t\t\tv-model=\"data\">\n\t\t\t</my-custom-component-that-is-long>\n\t\t{% endblock %}\n\n\t{% endblock %}\n\n{% endblock %}"
        );
    }

    #[test]
    fn test_config_preferred_max_line_length_small() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::UnixLF,
                indentation_mode: IndentationMode::Space,
                indentation_count: 4,
                preferred_max_line_length: 44,
                attribute_inline_max_count: 10,
                indent_children_of_blocks: true,
                linebreaks_around_blocks: true,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n    <div id=\"customized-thing\"\n         class=\"thing\">\n        <span>Whitespace sensitive</span>\n    </div>\n\n    {% block this_is_a_test_two %}\n\n        {% block some_content_block_1 %}\n            <h2 class=\"headline\"\n                @click=\"onHeadlineClick\"\n                v-if=\"showHeadline\">\n                hello\n            </h2>\n        {% endblock %}\n\n        {% block some_content_block_2 %}\n            <my-custom-component-that-is-long\n                    class=\"component\"\n                    @click=\"onComponentClick\"\n                    v-if=\"showComponent\"\n                    v-model=\"data\">\n            </my-custom-component-that-is-long>\n        {% endblock %}\n\n    {% endblock %}\n\n{% endblock %}"
        );
    }

    #[test]
    fn test_config_attribute_inline_max_count_large() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::UnixLF,
                indentation_mode: IndentationMode::Space,
                indentation_count: 4,
                preferred_max_line_length: 999,
                attribute_inline_max_count: 10,
                indent_children_of_blocks: true,
                linebreaks_around_blocks: true,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n    <div id=\"customized-thing\" class=\"thing\">\n        <span>Whitespace sensitive</span>\n    </div>\n\n    {% block this_is_a_test_two %}\n\n        {% block some_content_block_1 %}\n            <h2 class=\"headline\" @click=\"onHeadlineClick\" v-if=\"showHeadline\">\n                hello\n            </h2>\n        {% endblock %}\n\n        {% block some_content_block_2 %}\n            <my-custom-component-that-is-long class=\"component\" @click=\"onComponentClick\" v-if=\"showComponent\" v-model=\"data\">\n            </my-custom-component-that-is-long>\n        {% endblock %}\n\n    {% endblock %}\n\n{% endblock %}"
        );
    }

    #[test]
    fn test_config_indent_children_of_blocks_false() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::UnixLF,
                indentation_mode: IndentationMode::Space,
                indentation_count: 4,
                preferred_max_line_length: 120,
                attribute_inline_max_count: 2,
                indent_children_of_blocks: false,
                linebreaks_around_blocks: true,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n<div id=\"customized-thing\" class=\"thing\">\n    <span>Whitespace sensitive</span>\n</div>\n\n{% block this_is_a_test_two %}\n\n{% block some_content_block_1 %}\n<h2 class=\"headline\"\n    @click=\"onHeadlineClick\"\n    v-if=\"showHeadline\">\n    hello\n</h2>\n{% endblock %}\n\n{% block some_content_block_2 %}\n<my-custom-component-that-is-long\n        class=\"component\"\n        @click=\"onComponentClick\"\n        v-if=\"showComponent\"\n        v-model=\"data\">\n</my-custom-component-that-is-long>\n{% endblock %}\n\n{% endblock %}\n\n{% endblock %}"
        );
    }

    #[test]
    fn test_config_linebreaks_around_blocks_false() {
        let config = Config {
            format: Format {
                line_ending: LineEnding::UnixLF,
                indentation_mode: IndentationMode::Space,
                indentation_count: 4,
                preferred_max_line_length: 120,
                attribute_inline_max_count: 2,
                indent_children_of_blocks: true,
                linebreaks_around_blocks: false,
            },
        };
        let tree = create_tree_for_config_tests();
        let res = convert_tree_into_written_string(tree, &config);

        assert_eq!(
            res,
            "{% block this_is_a_test_one %}\n    <div id=\"customized-thing\" class=\"thing\">\n        <span>Whitespace sensitive</span>\n    </div>\n    {% block this_is_a_test_two %}\n        {% block some_content_block_1 %}\n            <h2 class=\"headline\"\n                @click=\"onHeadlineClick\"\n                v-if=\"showHeadline\">\n                hello\n            </h2>\n        {% endblock %}\n        {% block some_content_block_2 %}\n            <my-custom-component-that-is-long\n                    class=\"component\"\n                    @click=\"onComponentClick\"\n                    v-if=\"showComponent\"\n                    v-model=\"data\">\n            </my-custom-component-that-is-long>\n        {% endblock %}\n    {% endblock %}\n{% endblock %}"
        );
    }
}

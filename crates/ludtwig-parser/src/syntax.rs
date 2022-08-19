use rowan::{GreenNodeBuilder, NodeOrToken};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum SyntaxKind {
    WHITESPACE = 0,

    ADD,
    SUB,
    MUL,
    DIV,

    NUMBER,
    ERROR,
    OPERATION,
    ROOT,
}
use SyntaxKind::*;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

struct Parser<I: Iterator<Item = (SyntaxKind, String)>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
}
impl<I: Iterator<Item = (SyntaxKind, String)>> Parser<I> {
    fn peek(&mut self) -> Option<SyntaxKind> {
        while self.iter.peek().map(|&(t, _)| t == WHITESPACE).unwrap_or(false) {
            self.bump();
        }
        self.iter.peek().map(|&(t, _)| t)
    }
    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(token.into(), string.as_str());
        }
    }
    fn parse_val(&mut self) {
        match self.peek() {
            Some(NUMBER) => self.bump(),
            _ => {
                self.builder.start_node(ERROR.into());
                self.bump();
                self.builder.finish_node();
            }
        }
    }
    fn handle_operation(&mut self, tokens: &[SyntaxKind], next: fn(&mut Self)) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| tokens.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, OPERATION.into());
            self.bump();
            next(self);
            self.builder.finish_node();
        }
    }
    fn parse_mul(&mut self) {
        self.handle_operation(&[MUL, DIV], Self::parse_val)
    }
    fn parse_add(&mut self) {
        self.handle_operation(&[ADD, SUB], Self::parse_mul)
    }
    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(ROOT.into());
        self.parse_add();
        self.builder.finish_node();

        SyntaxNode::new_root(self.builder.finish())
    }
}

fn print(indent: usize, element: SyntaxElement) {
    let kind: SyntaxKind = element.kind();
    print!("{:indent$}", "", indent = indent);
    match element {
        NodeOrToken::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}



#[cfg(test)]
mod tests {
    use rowan::GreenToken;
    use super::*;

    #[test]
    fn it_works() {
        let parser = Parser {
            builder: GreenNodeBuilder::new(),
            iter: vec![
                // 1 + 2 * 3 + 4
                (NUMBER, "1".into()),
                (WHITESPACE, " ".into()),
                (ADD, "+".into()),
                (WHITESPACE, " ".into()),
                (NUMBER, "2".into()),
                (WHITESPACE, " ".into()),
                (MUL, "*".into()),
                (WHITESPACE, " ".into()),
                (NUMBER, "3".into()),
                (WHITESPACE, " ".into()),
                (ADD, "+".into()),
                (WHITESPACE, " ".into()),
                (NUMBER, "4".into()),
            ]
                .into_iter()
                .peekable(),
        };
        let ast = parser.parse();
        print(0, ast.clone().into());

        // test iteration
        /*
        for child in ast.descendants_with_tokens() {
            println!("{:?}", child);
        }
         */

        // test replacement
        let some_child = ast.descendants_with_tokens().find(|n| n.kind() == NUMBER && n.as_token().unwrap().text() == "3").unwrap();

        let some_child = some_child.as_token().unwrap();
        println!("{:?}", some_child);

        let replament_tree = some_child.replace_with(GreenToken::new(NUMBER.into(), "42"));
        let new_tree = SyntaxNode::new_root(replament_tree);
        println!("{:?}", new_tree);
        print(0, new_tree.clone().into());
        print(0, ast.clone().into());
    }
}
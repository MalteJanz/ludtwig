use crate::syntax::untyped::SyntaxKind;

/// Parsing event which describes an action, that the parser tells to do
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Event<'source> {
    StartNode {
        kind: SyntaxKind,
    },
    StartNodeAt {
        kind: SyntaxKind,
        checkpoint: usize,
    },
    AddToken {
        kind: SyntaxKind,
        text: &'source str,
    },
    FinishNode,
}

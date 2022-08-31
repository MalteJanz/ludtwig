use crate::syntax::untyped::SyntaxKind;

/// Parsing event which describes an action, that the parser tells to do
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Event {
    StartNode { kind: SyntaxKind },
    AddToken,
    FinishNode,
    Placeholder,
}

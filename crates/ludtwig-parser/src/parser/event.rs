use crate::parser::ParseError;
use crate::syntax::untyped::SyntaxKind;

/// Parsing event which describes an action for creating the syntax tree.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Error(ParseError),
    Placeholder,
}

/// A collection of parsing events which are ensured to be valid (right order, FinishNode for every StartNode, ...)
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct EventCollection {
    events: Vec<Event>,
    open_markers: Vec<usize>,
}

impl EventCollection {
    pub(super) fn new() -> Self {
        Self {
            events: vec![],
            open_markers: vec![],
        }
    }

    pub(super) fn add_token(&mut self) {
        self.events.push(Event::AddToken);
    }

    pub(super) fn add_error(&mut self, error: ParseError) {
        self.events.push(Event::Error(error));
    }

    pub(super) fn to_event_list(self) -> Vec<Event> {
        self.events
    }

    pub(super) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        // keep track of open markers to enforce closing them in the right order
        self.open_markers.push(pos);
        Marker::new(pos)
    }

    #[track_caller]
    pub(super) fn complete(&mut self, mut marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        marker.completed = true;

        // ensure marker completion order
        debug_assert_eq!(
            self.open_markers.last(),
            Some(&marker.pos),
            "Inner Markers must be closed before outer Markers!"
        );
        self.open_markers.pop();

        // replace placeholder
        let event_at_pos = &mut self.events[marker.pos];
        debug_assert_eq!(*event_at_pos, Event::Placeholder);
        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        // add finish node
        self.events.push(Event::FinishNode);

        CompletedMarker { pos: marker.pos }
    }

    pub(super) fn precede(&mut self, completed_marker: CompletedMarker) -> Marker {
        let new_m = self.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = self.events[completed_marker.pos]
        {
            *forward_parent = Some(new_m.pos - completed_marker.pos)
        } else {
            unreachable!();
        }

        new_m
    }
}

/// Marks the start of a node in the syntax tree and must be completed
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    fn new(pos: usize) -> Self {
        Self {
            pos,
            completed: false,
        }
    }

    #[track_caller]
    pub(crate) fn complete(
        self,
        event_collection: &mut EventCollection,
        kind: SyntaxKind,
    ) -> CompletedMarker {
        event_collection.complete(self, kind)
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed && !std::thread::panicking() {
            panic!("Markers need to be completed!");
        }
    }
}

/// After completing a marker (syntax node) it is possible to precede it by another marker
/// (wraps the syntax node in another one).
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(crate) fn precede(self, event_collection: &mut EventCollection) -> Marker {
        event_collection.precede(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn event_collection_markers() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token();
        let m_inner = collection.start();
        collection.add_token();
        m_inner.complete(&mut collection, SyntaxKind::BODY);
        collection.add_token();
        m_outer.complete(&mut collection, SyntaxKind::ROOT);

        assert_eq!(
            collection.to_event_list(),
            vec![
                Event::StartNode {
                    kind: SyntaxKind::ROOT,
                    forward_parent: None
                },
                Event::AddToken,
                Event::StartNode {
                    kind: SyntaxKind::BODY,
                    forward_parent: None
                },
                Event::AddToken,
                Event::FinishNode,
                Event::AddToken,
                Event::FinishNode
            ]
        );
    }

    #[test]
    fn event_collection_markers_precede() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token();
        let m_inner = collection.start();
        collection.add_token();
        let inner_wrapper_m = m_inner
            .complete(&mut collection, SyntaxKind::HTML_STRING)
            .precede(&mut collection);
        let inner_wrapper_wrapper_m = inner_wrapper_m
            .complete(&mut collection, SyntaxKind::BODY)
            .precede(&mut collection);
        collection.add_token();
        inner_wrapper_wrapper_m.complete(&mut collection, SyntaxKind::ERROR);

        m_outer.complete(&mut collection, SyntaxKind::ROOT);

        assert_eq!(
            collection.to_event_list(),
            vec![
                Event::StartNode {
                    kind: SyntaxKind::ROOT,
                    forward_parent: None,
                },
                Event::AddToken,
                Event::StartNode {
                    kind: SyntaxKind::HTML_STRING,
                    forward_parent: Some(3),
                },
                Event::AddToken,
                Event::FinishNode,
                Event::StartNode {
                    kind: SyntaxKind::BODY,
                    forward_parent: Some(2),
                },
                Event::FinishNode,
                Event::StartNode {
                    kind: SyntaxKind::ERROR,
                    forward_parent: None,
                },
                Event::AddToken,
                Event::FinishNode,
                Event::FinishNode,
            ]
        );
    }

    #[test]
    #[should_panic(expected = "Markers need to be completed!")]
    fn event_collection_markers_dropping_early_panic() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token();
        let m_inner = collection.start();
        collection.add_token();
        drop(m_outer); // early drop
        m_inner.complete(&mut collection, SyntaxKind::BODY);
        collection.add_token();
        // m_outer.complete(&mut collection, SyntaxKind::ROOT);
    }

    #[test]
    #[should_panic(expected = "Inner Markers must be closed before outer Markers!")]
    fn event_collection_inner_before_outer_markers_panic() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token();
        let m_inner = collection.start();
        collection.add_token();
        // this doesn't make sense (why closing root if the inner Body is still open?)
        // and this call should panic!
        m_outer.complete(&mut collection, SyntaxKind::ROOT);
        collection.add_token();
        collection.add_token();
        // same here
        m_inner.complete(&mut collection, SyntaxKind::BODY);
    }

    #[test]
    #[should_panic(expected = "Inner Markers must be closed before outer Markers!")]
    fn event_collection_inner_before_outer_preceding_markers_panic() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token();
        let m_inner = collection.start();
        collection.add_token();
        // this opens a new marker
        let m_inner_wrapper = m_inner
            .complete(&mut collection, SyntaxKind::ROOT)
            .precede(&mut collection);
        collection.add_token();
        collection.add_token();
        // this should trigger a panic, because there are open inner markers
        m_outer.complete(&mut collection, SyntaxKind::BODY);
    }
}

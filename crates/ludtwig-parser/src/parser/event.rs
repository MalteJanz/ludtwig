use crate::syntax::untyped::SyntaxKind;

/// Parsing event which describes an action for creating the syntax tree.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken {
        kind: SyntaxKind,
    },
    FinishNode,
    Placeholder,
}

/// A collection of parsing events which are ensured to be valid (right order, FinishNode for every StartNode, ...)
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct EventCollection {
    events: Vec<Event>,
    #[cfg(debug_assertions)]
    open_markers: Vec<usize>,
}

impl EventCollection {
    pub(super) fn new() -> Self {
        Self {
            events: vec![],
            #[cfg(debug_assertions)]
            open_markers: vec![],
        }
    }

    pub(super) fn add_token(&mut self, kind: SyntaxKind) {
        self.events.push(Event::AddToken { kind });
    }

    pub(super) fn into_event_list(self) -> Vec<Event> {
        self.events
    }

    pub(super) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        // keep track of open markers to enforce closing them in the right order
        #[cfg(debug_assertions)]
        self.open_markers.push(pos);
        Marker::new(pos)
    }

    #[track_caller]
    pub(super) fn complete(&mut self, mut marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        marker.completed = true;

        #[cfg(debug_assertions)]
        {
            // ensure marker completion order
            debug_assert_eq!(
                self.open_markers.last(),
                Some(&marker.pos),
                "Inner Markers must be closed before outer Markers!"
            );
            self.open_markers.pop();
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    /*
    #[test]
    fn event_size() {
        // should be small for performance reasons
        println!("event size {}", std::mem::size_of::<Event>());
        println!(
            "syntaxkind + usize size {}",
            std::mem::size_of::<(SyntaxKind, Option<usize>)>()
        );
    }
    */

    #[test]
    fn event_collection_markers() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        let m_inner = collection.start();
        collection.add_token(SyntaxKind::TK_AND);
        collection.complete(m_inner, SyntaxKind::BODY);
        collection.add_token(SyntaxKind::TK_APPLY);
        collection.complete(m_outer, SyntaxKind::ROOT);

        assert_eq!(
            collection.into_event_list(),
            vec![
                Event::StartNode {
                    kind: SyntaxKind::ROOT,
                    forward_parent: None
                },
                Event::AddToken {
                    kind: SyntaxKind::TK_WORD
                },
                Event::StartNode {
                    kind: SyntaxKind::BODY,
                    forward_parent: None
                },
                Event::AddToken {
                    kind: SyntaxKind::TK_AND
                },
                Event::FinishNode,
                Event::AddToken {
                    kind: SyntaxKind::TK_APPLY
                },
                Event::FinishNode
            ]
        );
    }

    #[test]
    fn event_collection_markers_precede() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        let m_inner = collection.start();
        collection.add_token(SyntaxKind::TK_AND);
        let inner_completed = collection.complete(m_inner, SyntaxKind::HTML_STRING);
        let inner_wrapper_m = collection.precede(inner_completed);
        let inner_wrapper_completed = collection.complete(inner_wrapper_m, SyntaxKind::BODY);
        let inner_wrapper_wrapper_m = collection.precede(inner_wrapper_completed);
        collection.add_token(SyntaxKind::TK_APPLY);
        collection.complete(inner_wrapper_wrapper_m, SyntaxKind::ERROR);

        collection.complete(m_outer, SyntaxKind::ROOT);

        assert_eq!(
            collection.into_event_list(),
            vec![
                Event::StartNode {
                    kind: SyntaxKind::ROOT,
                    forward_parent: None,
                },
                Event::AddToken {
                    kind: SyntaxKind::TK_WORD
                },
                Event::StartNode {
                    kind: SyntaxKind::HTML_STRING,
                    forward_parent: Some(3),
                },
                Event::AddToken {
                    kind: SyntaxKind::TK_AND
                },
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
                Event::AddToken {
                    kind: SyntaxKind::TK_APPLY
                },
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
        collection.add_token(SyntaxKind::TK_WORD);
        let m_inner = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        drop(m_outer); // early drop
        collection.complete(m_inner, SyntaxKind::BODY);
        collection.add_token(SyntaxKind::TK_WORD);
    }

    #[test]
    #[should_panic(expected = "Inner Markers must be closed before outer Markers!")]
    fn event_collection_inner_before_outer_markers_panic() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        let m_inner = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        // this doesn't make sense (why closing root if the inner Body is still open?)
        // and this call should panic!
        collection.complete(m_outer, SyntaxKind::ROOT);
        collection.add_token(SyntaxKind::TK_WORD);
        collection.add_token(SyntaxKind::TK_WORD);
        // same here
        collection.complete(m_inner, SyntaxKind::BODY);
    }

    #[test]
    #[should_panic(expected = "Inner Markers must be closed before outer Markers!")]
    fn event_collection_inner_before_outer_preceding_markers_panic() {
        let mut collection = EventCollection::new();
        let m_outer = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        let m_inner = collection.start();
        collection.add_token(SyntaxKind::TK_WORD);
        // this opens a new marker
        let inner_completed = collection.complete(m_inner, SyntaxKind::ROOT);
        let _m_inner_wrapper = collection.precede(inner_completed);
        collection.add_token(SyntaxKind::TK_WORD);
        collection.add_token(SyntaxKind::TK_WORD);
        // this should trigger a panic, because there are open inner markers
        collection.complete(m_outer, SyntaxKind::BODY);
    }
}

use crate::parser::event::Event;
use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;

pub(super) struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            completed: false,
        }
    }

    pub(super) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;

        let event_at_pos = &mut parser.events[self.pos];
        debug_assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode { kind };
        parser.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Markers need to be completed!");
        }
    }
}

pub(super) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(super) fn precede(self, parser: &mut Parser) -> Marker {
        let new_m = parser.start();

        // TODO: finish implementing me
        todo!()
    }
}

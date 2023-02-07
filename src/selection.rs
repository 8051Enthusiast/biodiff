use std::array::from_fn;

use crate::cursor::CursorActive;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum SelectionStatus {
    #[default]
    None,
    Start,
    Mid,
    End,
    Only,
}

impl SelectionStatus {
    pub fn is_active(self) -> bool {
        self != Self::None
    }

    pub fn continues_on_right(self, rtl: bool) -> bool {
        self == Self::Mid || !rtl && self == Self::Start || rtl && self == Self::End
    }
}

#[derive(Default)]
pub struct Selections {
    start: [Option<isize>; 2],
    current: [isize; 2],
}

impl Selections {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn start(&mut self, offset: [isize; 2], cursor_act: CursorActive) {
        if cursor_act.is_first() {
            self.start[0] = Some(offset[0]);
            self.current[0] = offset[0];
        }
        if cursor_act.is_second() {
            self.start[1] = Some(offset[1]);
            self.current[1] = offset[1];
        }
    }

    pub fn clear(&mut self, cursor_act: CursorActive) -> bool {
        let mut ret = false;
        if cursor_act.is_first() {
            ret |= self.start[0].is_some();
            self.start[0] = None;
        }
        if cursor_act.is_second() {
            ret |= self.start[1].is_some();
            self.start[1] = None;
        }
        ret
    }

    pub fn update(&mut self, offset: [isize; 2], cursor_act: CursorActive) {
        if cursor_act.is_first() {
            self.current[0] = offset[0];
        }
        if cursor_act.is_second() {
            self.current[1] = offset[1];
        }
    }

    pub fn selection_status(&self, offset: [isize; 2]) -> [SelectionStatus; 2] {
        from_fn(|i| match (self.start[i], self.current[i], offset[i]) {
            (Some(start), current, offset) => {
                let begin = start.min(current);
                let end = start.max(current);
                if begin == end && offset == begin {
                    return SelectionStatus::Only;
                } else if offset == begin {
                    SelectionStatus::Start
                } else if offset == end {
                    SelectionStatus::End
                } else if (begin..=end).contains(&offset) {
                    SelectionStatus::Mid
                } else {
                    SelectionStatus::None
                }
            }
            _ => SelectionStatus::None,
        })
    }

    pub fn ranges(&self, cursor_act: CursorActive) -> [Option<[isize; 2]>; 2] {
        let active = cursor_act.is_active();
        from_fn(|i| match (self.start[i], self.current[i], active[i]) {
            (Some(start), current, true) => {
                let begin = start.min(current);
                let end = start.max(current);
                Some([begin, end])
            }
            _ => None,
        })
    }

    pub fn is_active(&self) -> bool {
        self.start.iter().any(|x| x.is_some())
    }
}

use std::{ops::Range, sync::mpsc::Sender};

use cursive::{Vec2, View};

use crate::{
    align::{AlignAlgorithm, AlignElement},
    backend::{Action, Backend, Cursiv},
    datastruct::{CompVec, DoubleVec, SignedArray},
    drawer::{CursorActive, DoubleHexContext, DoubleHexLine, Move},
    utils::{FileContent, PointedFile},
};

/// An unaligned view that is just two files next to each other
pub struct Unaligned {
    data: CompVec,
    filenames: (String, String),
    index: isize,
    pub dh: DoubleHexContext,
    cursor_act: CursorActive,
}

impl Unaligned {
    /// Creates a new view, with the indexes in the files at the cursor
    pub fn new(first: PointedFile, second: PointedFile, dh: DoubleHexContext) -> Self {
        let mut index = -(dh.cursor.get_index() as isize);
        let mut data = CompVec::new(first.content, second.content);
        index += data.add_first_shift(-(first.index as isize));
        index += data.add_second_shift(-(second.index as isize));
        Unaligned {
            data,
            filenames: (first.name, second.name),
            index,
            dh,
            cursor_act: CursorActive::Both,
        }
    }
    /// Resizes the view without drawing it, returning if anything changed
    pub fn resize(&mut self, dimensions: (usize, usize)) -> bool {
        let (columns, rows) = dimensions;
        let old_dimensions = (self.dh.cursor.get_size_x(), self.dh.cursor.get_size_y());
        let new_dimensions = self.dh.style.get_doublehex_dims(columns, rows);
        self.index += self.dh.cursor.resize(new_dimensions);
        (old_dimensions) != new_dimensions
    }
    /// Redraws without checking for resize.
    /// clear indicates whether the screen should be cleared before.
    pub fn redraw<B: Backend>(&self, printer: &mut B, clear: bool) {
        if clear {
            printer.clear();
        }
        let content = self.get_content();
        self.dh.print_doublehex_screen(&content, printer);
        self.set_cursor(printer, self.cursor_act);
        self.print_bars(printer);
        printer.refresh();
    }
    /// Resizes and redraws.
    pub fn refresh<B: Backend>(&mut self, printer: &mut B) {
        let changed = self.resize(printer.size());
        self.redraw(printer, changed);
    }
    /// Paints the cursor at the current position
    fn set_cursor<B: Backend>(&self, printer: &mut B, cursor_act: CursorActive) {
        let cursor_index = self.cursor_index();
        let addr = (
            self.data.get_first_addr(cursor_index),
            self.data.get_second_addr(cursor_index),
        );
        self.dh
            .set_doublehex_cursor(printer, cursor_act, self.data.get(cursor_index), addr);
    }
    fn change_active_cursor<B: Backend>(&mut self, printer: &mut B, cursor_act: CursorActive) {
        self.cursor_act = cursor_act;
        self.move_back_into_bounds(printer);
        self.set_cursor(printer, cursor_act);
        printer.refresh()
    }
    /// Converts the content of the CompVec into DoubleHexLines so they can be displayed
    fn get_content(&self) -> Vec<DoubleHexLine> {
        let mut content = Vec::new();
        for x in 0..self.dh.cursor.get_size_y() {
            // address of the nth line
            let base_addr = (x * self.dh.cursor.get_size_x()) as isize + self.index;
            let address = (
                self.data.get_first_addr(base_addr),
                self.data.get_second_addr(base_addr),
            );
            let bytes = self
                .data
                .get_range(base_addr..base_addr + self.dh.cursor.get_size_x() as isize);
            content.push(DoubleHexLine { address, bytes });
        }
        content
    }
    /// Prints the top and bottom bar
    fn print_bars<B: Backend>(&self, printer: &mut B) {
        self.dh
            .print_title_line(printer, " unaligned", &self.filenames.0, &self.filenames.1);
        let cursor_index = self.cursor_index();
        let addr = (
            self.data.get_first_addr(cursor_index),
            self.data.get_second_addr(cursor_index),
        );
        self.dh.print_bottom_line(printer, addr);
    }

    fn active_data_bounds(&self) -> Range<isize> {
        match self.cursor_act {
            CursorActive::Both | CursorActive::None => self.data.bounds(),
            CursorActive::First => 0..self.data.get_data().0.len() as isize,
            CursorActive::Second => {
                self.data.shift..self.data.shift + self.data.get_data().1.len() as isize
            }
        }
    }
    fn cursor_index(&self) -> isize {
        self.index + self.dh.cursor.get_index() as isize
    }
    pub fn move_back_into_bounds<B: Backend>(&mut self, printer: &mut B) {
        let old_active = self.cursor_act;
        let bounds = self.active_data_bounds();
        self.cursor_act = CursorActive::Both;
        let cursor_pos = self.cursor_index();
        let new_index = if cursor_pos < bounds.start {
            bounds.start
        } else if cursor_pos >= bounds.end {
            bounds.end - 1
        } else {
            self.cursor_act = old_active;
            return;
        };
        self.goto_index(printer, new_index);
        self.cursor_act = old_active;
    }
    /// moves the cursor xdiff down and ydiff to the right,
    /// redrawing/scrolling if necessary
    pub fn move_around<B: Backend>(&mut self, printer: &mut B, movement: Move) {
        self.set_cursor(printer, CursorActive::None);
        let bounds = self.active_data_bounds();
        let relative_bounds = (bounds.start - self.index)..(bounds.end - self.index);
        let diff = self.dh.cursor.mov(movement, relative_bounds);
        // update the compvec in case the views are moved independently
        let index_diff = match self.cursor_act {
            CursorActive::Both => diff,
            CursorActive::First => self.data.add_first_shift(-diff),
            CursorActive::Second => self.data.add_second_shift(-diff),
            CursorActive::None => diff,
        };
        self.index += index_diff;
        // if they are moved independently, we cannot scroll
        if !matches!(self.cursor_act, CursorActive::Both) {
            self.redraw(printer, false);
        } else if let Some(scroll_amount) = self.dh.cursor.full_row_move(index_diff) {
            // scroll if we can
            let content = self.get_content();
            self.dh
                .print_doublehex_scrolled(&content, printer, scroll_amount);
            self.set_cursor(printer, self.cursor_act);
            if scroll_amount != 0 {
                self.print_bars(printer);
            }
            printer.refresh();
        } else {
            self.redraw(printer, false);
        }
    }
    /// Function that processes only the move events
    pub fn process_move<B: Backend>(&mut self, printer: &mut B, action: Action) {
        match action {
            Action::Down => self.move_around(printer, Move::CursorY(1)),
            Action::DownAlt => self.move_around(printer, Move::ViewY(1)),
            Action::Up => self.move_around(printer, Move::CursorY(-1)),
            Action::UpAlt => self.move_around(printer, Move::ViewY(-1)),
            Action::Left => self.move_around(printer, Move::CursorX(-1)),
            Action::LeftAlt => self.move_around(printer, Move::ViewX(-1)),
            Action::Right => self.move_around(printer, Move::CursorX(1)),
            Action::RightAlt => self.move_around(printer, Move::ViewX(1)),
            Action::PgDown => self.move_around(
                printer,
                Move::ViewY(self.dh.cursor.get_size_y() as isize / 2),
            ),
            Action::PgUp => self.move_around(
                printer,
                Move::ViewY(-(self.dh.cursor.get_size_y() as isize) / 2),
            ),
            Action::NextDifference => self.jump_next_difference(printer),
            Action::Top => self.jump_start(printer),
            Action::Bottom => self.jump_end(printer),
            _ => (),
        }
    }
    /// Process a single action/event
    pub fn process_action<B: Backend>(&mut self, printer: &mut B, action: Action) {
        match action {
            Action::Refresh => self.refresh(printer),
            Action::CursorFirst => self.change_active_cursor(printer, CursorActive::First),
            Action::CursorBoth => self.change_active_cursor(printer, CursorActive::Both),
            Action::CursorSecond => self.change_active_cursor(printer, CursorActive::Second),
            otherwise => self.process_move(printer, otherwise),
        }
    }
    pub fn goto_index<B: Backend>(&mut self, printer: &mut B, index: isize) {
        let address_diff = index - self.cursor_index();
        let (col, row) = self.dh.cursor.jump(address_diff);
        self.move_around(printer, Move::Unbounded(col, row));
    }
    /// Go to the address in `pos`, right is true if on the second view, else the first view is used.
    /// Returns true if the address exists.
    pub fn goto<B: Backend>(
        &mut self,
        printer: &mut B,
        right: bool,
        pos: usize,
    ) -> Result<(), String> {
        if !right && !self.cursor_act.is_first() {
            return Err(
                "Attempting to search on first view, but current cursor is on second view".into(),
            );
        } else if right && !self.cursor_act.is_second() {
            return Err(
                "Attempting to search on second view, but current cursor is on first view".into(),
            );
        }
        self.goto_index(
            printer,
            pos as isize - if right { -self.data.shift } else { 0 },
        );
        Ok(())
    }
    pub fn jump_next_difference<B: Backend>(&mut self, printer: &mut B) {
        // skip half a page
        let first_address = (self.cursor_index()
            + (self.dh.cursor.get_size_y() / 2) as isize * self.dh.cursor.get_size_x() as isize)
            .clamp(self.data.bounds().start, self.data.bounds().end - 1);
        let mut target_address = None;
        for i in first_address..self.data.bounds().end {
            let current = self.data.get(i);
            if current.0 != current.1 || (current.0.is_none() && current.1.is_none()) {
                target_address = Some(i);
                break;
            }
        }
        self.goto_index(
            printer,
            target_address.unwrap_or(self.data.bounds().end - 1),
        );
    }
    /// Go to the first position of the file
    pub fn jump_start<B: Backend>(&mut self, printer: &mut B) {
        let index = self.active_data_bounds().start;
        self.goto_index(printer, index)
    }
    /// Go to the last position of the file
    pub fn jump_end<B: Backend>(&mut self, printer: &mut B) {
        let index = self.active_data_bounds().end - 1;
        self.goto_index(printer, index)
    }
    /// Turns the view into most of its parts
    pub fn destruct(self) -> Result<(PointedFile, PointedFile, DoubleHexContext), Self> {
        let cursor_index = self.cursor_index();
        // for now we only return if the cursor is at a positions where both indexes are actually
        // inside the file
        if let (Some(laddr), Some(raddr)) = (
            self.data.get_first_addr(cursor_index),
            self.data.get_second_addr(cursor_index),
        ) {
            let (lvec, rvec) = self.data.get_data();
            Ok((
                PointedFile {
                    name: self.filenames.0,
                    content: lvec,
                    index: laddr,
                },
                PointedFile {
                    name: self.filenames.1,
                    content: rvec,
                    index: raddr,
                },
                self.dh,
            ))
        } else {
            Err(self)
        }
    }
}

/// Enum that containts events but also allows
/// messages for appending/prepending data to the Aligned view.
pub enum AlignedMessage {
    UserEvent(Action),
    Append(Vec<AlignElement>),
    Prepend(Vec<AlignElement>),
}

impl From<Action> for AlignedMessage {
    fn from(action: Action) -> Self {
        AlignedMessage::UserEvent(action)
    }
}

/// A view that dynamically displays aligned files
pub struct Aligned {
    data: DoubleVec<AlignElement>,
    filenames: (String, String),
    original: (FileContent, FileContent),
    index: isize,
    pub dh: DoubleHexContext,
}

impl Aligned {
    /// Creates a new aligned view, starting a new thread that sends new aligned
    /// data of `algo` over the sender.
    /// Note that receiving events and sending them to the view has to be handled by
    /// the caller for unknown reasons.
    pub fn new(
        first: PointedFile,
        second: PointedFile,
        dh: DoubleHexContext,
        algo: &AlignAlgorithm,
        sender: Sender<AlignedMessage>,
    ) -> Self {
        let index = -(dh.cursor.get_index() as isize);
        let data = DoubleVec::new();
        let first_arc = first.content.clone();
        let second_arc = second.content.clone();
        algo.start_align(first_arc, second_arc, (first.index, second.index), sender);
        Aligned {
            data,
            filenames: (first.name, second.name),
            original: (first.content, second.content),
            index,
            dh,
        }
    }
    /// Checks whether a given range of indexes overlaps with the indexes currently visible.
    fn is_in_view(&self, range: Range<isize>) -> bool {
        let self_range = self.index
            ..self.index + (self.dh.cursor.get_size_x() * self.dh.cursor.get_size_y()) as isize;
        !(self_range.start >= range.end || self_range.end <= range.start)
    }
    /// Gets a useful form of the information contained in the alignement data for printing.
    fn get_content(&self) -> Vec<DoubleHexLine> {
        let mut content = Vec::new();
        for x in 0..self.dh.cursor.get_size_y() {
            // address of current line to be converted
            let base_addr = (x * self.dh.cursor.get_size_x()) as isize + self.index;
            let address = self
                .data
                .get(base_addr)
                .map(|alignel| (Some(alignel.xaddr), Some(alignel.yaddr)))
                .unwrap_or_default();
            let bytes = self
                .data
                .get_range(base_addr..base_addr + self.dh.cursor.get_size_x() as isize)
                .iter()
                .map(|malignel| {
                    malignel
                        .map(|alignel| (alignel.xbyte, alignel.ybyte))
                        .unwrap_or_default()
                })
                .collect();
            content.push(DoubleHexLine { address, bytes });
        }
        content
    }
    fn cursor_index(&self) -> isize {
        self.index + self.dh.cursor.get_index() as isize
    }
    /// Paints the cursor at the current position
    fn set_cursor<B: Backend>(&self, printer: &mut B, cursor_act: CursorActive) {
        let cursor_index = self.cursor_index();
        let bytes = self
            .data
            .get(cursor_index)
            .map(|alignel| (alignel.xbyte, alignel.ybyte))
            .unwrap_or_default();
        let addresses = self
            .data
            .get(cursor_index)
            .map(|alignel| (Some(alignel.xaddr), Some(alignel.yaddr)))
            .unwrap_or_default();
        self.dh
            .set_doublehex_cursor(printer, cursor_act, bytes, addresses);
    }

    /// Prints the top and bottom bar.
    fn print_bars<B: Backend>(&self, printer: &mut B) {
        self.dh
            .print_title_line(printer, " aligned", &self.filenames.0, &self.filenames.1);
        let cursor_index = self.cursor_index();
        let addresses = self
            .data
            .get(cursor_index)
            .map(|alignel| (Some(alignel.xaddr), Some(alignel.yaddr)))
            .unwrap_or_default();
        self.dh.print_bottom_line(printer, addresses);
    }

    /// Moves the cursor xdiff down and ydiff to the right,
    /// redrawing/scrolling if necessary.
    pub fn move_around<B: Backend>(&mut self, printer: &mut B, movement: Move) {
        self.set_cursor(printer, CursorActive::None);
        let relative_bounds =
            (self.data.bounds().start - self.index)..(self.data.bounds().end - self.index);
        let index_diff = self.dh.cursor.mov(movement, relative_bounds);
        self.index += index_diff;
        if let Some(scroll_amount) = self.dh.cursor.full_row_move(index_diff) {
            let content = self.get_content();
            self.dh
                .print_doublehex_scrolled(&content, printer, scroll_amount);
            self.set_cursor(printer, CursorActive::Both);
            if scroll_amount != 0 {
                self.print_bars(printer);
            }
            printer.refresh();
        } else {
            self.redraw(printer, false);
        }
    }
    /// Appends alignment data to the underlying DoubleVec.
    /// Returns true if something in view changed.
    pub fn append(&mut self, vec: Vec<AlignElement>) -> bool {
        let extend_range = self.data.bounds().end..self.data.bounds().end + vec.len() as isize;
        self.data.extend_end(&vec);
        self.is_in_view(extend_range)
    }
    /// Prepends alignment data to the underlying DoubleVec.
    /// Returns true if something in view changed.
    pub fn prepend(&mut self, vec: Vec<AlignElement>) -> bool {
        let extend_range = self.data.bounds().start - vec.len() as isize..self.data.bounds().start;
        self.data.extend_front(&vec);
        self.is_in_view(extend_range)
    }
    /// Resize the view without printing it, returns whether redrawing is necessary.
    pub fn resize(&mut self, dimensions: (usize, usize)) -> bool {
        let (columns, rows) = dimensions;
        let old_dimensions = (self.dh.cursor.get_size_x(), self.dh.cursor.get_size_y());
        let new_dimensions = self.dh.style.get_doublehex_dims(columns, rows);
        self.index += self.dh.cursor.resize(new_dimensions);
        (old_dimensions) != new_dimensions
    }
    /// Redraws the current view without checking and updating the view for changes.
    pub fn redraw<B: Backend>(&self, printer: &mut B, clear: bool) {
        if clear {
            printer.clear();
        }
        let content = self.get_content();
        self.dh.print_doublehex_screen(&content, printer);
        self.set_cursor(printer, CursorActive::Both);
        self.print_bars(printer);
        printer.refresh();
    }
    /// Updates the view and draws it.
    pub fn refresh<B: Backend>(&mut self, printer: &mut B) {
        let changed = self.resize(printer.size());
        self.redraw(printer, changed);
    }
    pub fn goto_index<B: Backend>(&mut self, printer: &mut B, index: isize) {
        let address_diff = index - self.cursor_index();
        let (col, row) = self.dh.cursor.jump(address_diff);
        self.move_around(printer, Move::Unbounded(col, row));
    }
    /// Go to the address in `pos`, right is true if on the second view, else the first view is used.
    /// Returns true if the address exists.
    pub fn goto<B: Backend>(
        &mut self,
        printer: &mut B,
        right: bool,
        pos: usize,
    ) -> Result<(), String> {
        let address_index = self
            .data
            .binary_search(&pos, |pos, el| {
                Some(*pos).cmp(&el.map(|a| if right { a.yaddr } else { a.xaddr }))
            })
            .map_err(|_| "Address does not (yet) exist")?;
        self.goto_index(printer, address_index);
        Ok(())
    }
    pub fn jump_next_difference<B: Backend>(&mut self, printer: &mut B) {
        // skip half a page
        let first_address = (self.cursor_index()
            + (self.dh.cursor.get_size_y() / 2) as isize * self.dh.cursor.get_size_x() as isize)
            .clamp(self.data.bounds().start, self.data.bounds().end - 1);
        let mut target_address = None;
        for i in first_address..=self.data.bounds().end {
            let current = self.data.get(i);
            if current.map(|x| x.xbyte != x.ybyte).unwrap_or(true) {
                target_address = Some(i);
                break;
            }
        }
        self.goto_index(
            printer,
            target_address.unwrap_or(self.data.bounds().end - 1),
        );
    }
    /// Go to the first position of the file
    pub fn jump_start<B: Backend>(&mut self, printer: &mut B) {
        self.goto_index(printer, self.data.bounds().start)
    }
    /// Go to the last position of the file
    pub fn jump_end<B: Backend>(&mut self, printer: &mut B) {
        self.goto_index(printer, self.data.bounds().end - 1)
    }
    /// Process move events
    pub fn process_move<B: Backend>(&mut self, printer: &mut B, action: Action) {
        match action {
            Action::Down => self.move_around(printer, Move::CursorY(1)),
            Action::DownAlt => self.move_around(printer, Move::ViewY(1)),
            Action::Up => self.move_around(printer, Move::CursorY(-1)),
            Action::UpAlt => self.move_around(printer, Move::ViewY(-1)),
            Action::Left => self.move_around(printer, Move::CursorX(-1)),
            Action::LeftAlt => self.move_around(printer, Move::ViewX(-1)),
            Action::Right => self.move_around(printer, Move::CursorX(1)),
            Action::RightAlt => self.move_around(printer, Move::ViewX(1)),
            Action::PgDown => self.move_around(
                printer,
                Move::ViewY(self.dh.cursor.get_size_y() as isize / 2),
            ),
            Action::PgUp => self.move_around(
                printer,
                Move::ViewY(-(self.dh.cursor.get_size_y() as isize) / 2),
            ),
            Action::NextDifference => self.jump_next_difference(printer),
            Action::Top => self.jump_start(printer),
            Action::Bottom => self.jump_end(printer),
            _ => (),
        }
    }
    /// Process events
    pub fn process_action<B: Backend>(&mut self, printer: &mut B, action: AlignedMessage) {
        match match action {
            AlignedMessage::UserEvent(ev) => ev,
            AlignedMessage::Append(vec) => {
                if self.append(vec) {
                    self.refresh(printer);
                }
                return;
            }
            AlignedMessage::Prepend(vec) => {
                if self.prepend(vec) {
                    self.refresh(printer);
                }
                return;
            }
        } {
            Action::Refresh => self.refresh(printer),
            otherwise => self.process_move(printer, otherwise),
        }
    }
    /// Turn an Aligned view into its part, including information on where it points
    pub fn destruct(self) -> Result<(PointedFile, PointedFile, DoubleHexContext), Self> {
        // we return the original view in case the cursor is outside the files
        match (self.data.get(self.cursor_index())).map(|a| (a.xaddr, a.yaddr)) {
            Some((xaddr, yaddr)) => Ok((
                PointedFile {
                    name: self.filenames.0,
                    content: self.original.0,
                    index: xaddr,
                },
                PointedFile {
                    name: self.filenames.1,
                    content: self.original.1,
                    index: yaddr,
                },
                self.dh,
            )),
            None => Err(self),
        }
    }
}

// view implementations for cursive
impl View for Unaligned {
    fn draw(&self, printer: &cursive::Printer) {
        let mut backend = Cursiv::from_printer(printer);
        self.redraw(&mut backend, true);
    }
    fn layout(&mut self, size: Vec2) {
        self.resize((size.x, size.y));
    }
}

impl View for Aligned {
    fn draw(&self, printer: &cursive::Printer) {
        let mut backend = Cursiv::from_printer(printer);
        self.redraw(&mut backend, true);
    }
    fn layout(&mut self, size: Vec2) {
        self.resize((size.x, size.y));
    }
}

use std::{
    iter::Peekable,
    ops::Range,
    sync::{atomic::AtomicBool, mpsc::Sender, Arc},
};

use cursive::{Vec2, View};

use crate::{
    align::{AlignAlgorithm, AlignElement},
    backend::{Action, Backend, Cursiv},
    datastruct::{CompVec, DoubleVec, SignedArray},
    drawer::{ByteData, CursorActive, DoubleHexContext, DoubleHexLine, Move},
    file::{FileContent, FileState},
    search::{Query, SearchContext, SearchResults},
};

/// An unaligned view that is just two files next to each other
pub struct Unaligned {
    data: CompVec,
    filenames: (String, String),
    searches: (Option<SearchResults>, Option<SearchResults>),
    index: isize,
    pub dh: DoubleHexContext,
    cursor_act: CursorActive,
}

impl Unaligned {
    /// Creates a new view, with the indexes in the files at the cursor
    pub fn new(first: FileState, second: FileState, dh: DoubleHexContext) -> Self {
        let mut index = -(dh.cursor.get_index() as isize);
        let mut data = CompVec::new(first.content, second.content);
        index += data.add_first_shift(-(first.index as isize));
        index += data.add_second_shift(-(second.index as isize));
        Unaligned {
            data,
            filenames: (first.name, second.name),
            searches: (first.search, second.search),
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
        let (a, b) = self.data.get(cursor_index);
        let [a, b] = [(&self.searches.0, addr.0, a), (&self.searches.1, addr.1, b)].map(
            |(search, addr, byte)| {
                let is_search_result = search.as_ref().map_or(false, |s| s.is_in_result(addr));
                ByteData::maybe_new(byte, is_search_result)
            },
        );
        self.dh
            .set_doublehex_cursor(printer, cursor_act, (a, b), addr);
    }
    fn change_active_cursor<B: Backend>(&mut self, printer: &mut B, cursor_act: CursorActive) {
        self.cursor_act = cursor_act;
        self.move_back_into_bounds(printer);
        self.set_cursor(printer, cursor_act);
        printer.refresh()
    }
    fn search_ranges(&self) -> [Vec<(isize, isize)>; 2] {
        let intersect_range =
            |a: Range<isize>, b: Range<isize>| a.start.max(b.start)..a.end.min(b.end);
        let view_range = self.index..self.index + self.dh.cursor.get_size() as isize;
        let first_range = intersect_range(self.data.first_bound(), view_range.clone());
        let second_range = intersect_range(self.data.second_bound(), view_range);
        let first_start = self.data.get_first_addr(first_range.start);
        let first_end = self.data.get_first_addr(first_range.end - 1);
        let second_start = self.data.get_second_addr(second_range.start);
        let second_end = self.data.get_second_addr(second_range.end - 1);
        let first = if let (Some(start), Some(end), Some(search)) =
            (first_start, first_end, &self.searches.0)
        {
            search
                .lookup_results(start..end + 1)
                .into_iter()
                .map(|(a, b)| (a as isize, b as isize))
                .collect()
        } else {
            vec![]
        };
        let second = if let (Some(start), Some(end), Some(search)) =
            (second_start, second_end, &self.searches.1)
        {
            search
                .lookup_results(start..end + 1)
                .into_iter()
                .map(|(a, b)| (self.data.shift + a as isize, self.data.shift + b as isize))
                .collect()
        } else {
            vec![]
        };
        [first, second]
    }
    /// Converts the content of the CompVec into DoubleHexLines so they can be displayed
    fn get_content(&self) -> Vec<DoubleHexLine> {
        let mut content = Vec::new();
        let [first_range, second_range] = self.search_ranges();
        let mut next_first = first_range.into_iter().peekable();
        let mut next_second = second_range.into_iter().peekable();
        for x in 0..self.dh.cursor.get_size_y() {
            // address of the nth line
            let base_addr = (x * self.dh.cursor.get_size_x()) as isize + self.index;
            let address = (
                self.data.get_first_addr(base_addr),
                self.data.get_second_addr(base_addr),
            );
            let mut bytes = Vec::new();
            for (i, (byte_a, byte_b)) in self
                .data
                .get_range(base_addr..base_addr + self.dh.cursor.get_size_x() as isize)
                .into_iter()
                .enumerate()
            {
                let current_addr = base_addr + i as isize;
                let [first_is_result, second_is_result] = [&mut next_first, &mut next_second]
                    .map(|x| is_next_search_result(x, current_addr));
                bytes.push((
                    byte_a.map(|byte| ByteData {
                        byte,
                        is_search_result: first_is_result,
                    }),
                    byte_b.map(|byte| ByteData {
                        byte,
                        is_search_result: second_is_result,
                    }),
                ));
            }

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
            CursorActive::First => self.data.first_bound(),
            CursorActive::Second => self.data.second_bound(),
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
        let movement = if self.dh.style.right_to_left {
            movement.reflect_rtl()
        } else {
            movement
        };
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
            Action::NextSearch => self.jump_next_search_result(printer),
            Action::PrevSearch => self.jump_prev_search_result(printer),
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
        let bounds = if !right {
            0..self.data.get_data().0.len()
        } else {
            0..self.data.get_data().1.len()
        };
        if !bounds.contains(&pos) {
            return Err(format!(
                "Target address {:#x} is not in bounds (< {:#x})",
                pos, bounds.end
            ));
        }
        self.goto_index(
            printer,
            pos as isize - if right { -self.data.shift } else { 0 },
        );
        Ok(())
    }
    fn search_data(&self) -> Vec<(&Option<SearchResults>, usize, bool)> {
        self.data
            .get_first_addr(self.cursor_index())
            .map(|x| (&self.searches.0, x, false))
            .filter(|_| self.cursor_act.is_first())
            .iter()
            .chain(
                self.data
                    .get_second_addr(self.cursor_index())
                    .map(|x| (&self.searches.1, x, true))
                    .filter(|_| self.cursor_act.is_second())
                    .iter(),
            )
            .copied()
            .collect()
    }
    fn index_address(&self, right: bool, addr: usize) -> isize {
        if right {
            self.data.shift + addr as isize
        } else {
            addr as isize
        }
    }
    pub fn jump_next_search_result<B: Backend>(&mut self, printer: &mut B) {
        let search_data = self.search_data();
        let next = match SearchResults::nearest_next_result(&search_data, |addr, right| {
            Some(self.index_address(right, addr))
        }) {
            Some(x) => x,
            None => return,
        };
        self.goto_index(printer, next)
    }
    pub fn jump_prev_search_result<B: Backend>(&mut self, printer: &mut B) {
        let search_data = self.search_data();
        let next = match SearchResults::nearest_prev_result(&search_data, |addr, right| {
            Some(self.index_address(right, addr))
        }) {
            Some(x) => x,
            None => return,
        };
        self.goto_index(printer, next)
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
    pub fn add_search_results(
        &mut self,
        query: Query,
        results: Vec<Option<Range<usize>>>,
        first: bool,
    ) {
        let search = if first {
            &mut self.searches.0
        } else {
            &mut self.searches.1
        };
        let search = match search {
            Some(s) if s.query() == &query => s,
            _ => return,
        };
        for result in results.iter().flatten() {
            search.add_match(result.clone())
        }
    }
    pub fn clear_search(&mut self) {
        if self.cursor_act.is_first() {
            self.searches.0 = None
        }
        if self.cursor_act.is_second() {
            self.searches.1 = None
        }
    }
    pub fn setup_search(
        &mut self,
        query: Query,
    ) -> (
        (SearchContext, FileContent),
        Option<(SearchContext, FileContent)>,
    ) {
        let is_running = Arc::new(AtomicBool::new(true));
        match self.cursor_act {
            CursorActive::None | CursorActive::Both => {
                self.searches.0 = Some(SearchResults::new(query.clone()));
                self.searches.1 = Some(SearchResults::new(query.clone()));
                (
                    (
                        SearchContext {
                            first: true,
                            query: query.clone(),
                            is_running: is_running.clone(),
                        },
                        self.data.get_data().0,
                    ),
                    Some((
                        SearchContext {
                            first: false,
                            query,
                            is_running,
                        },
                        self.data.get_data().1,
                    )),
                )
            }
            CursorActive::First => {
                self.searches.0 = Some(SearchResults::new(query.clone()));
                (
                    (
                        SearchContext {
                            first: true,
                            query,
                            is_running,
                        },
                        self.data.get_data().0,
                    ),
                    None,
                )
            }
            CursorActive::Second => {
                self.searches.1 = Some(SearchResults::new(query.clone()));
                (
                    (
                        SearchContext {
                            first: false,
                            query,
                            is_running,
                        },
                        self.data.get_data().1,
                    ),
                    None,
                )
            }
        }
    }
    pub fn current_search_query(&self) -> Option<&Query> {
        if self.cursor_act.is_first() {
            [&self.searches.0, &self.searches.1]
        } else {
            [&self.searches.1, &self.searches.0]
        }
        .iter()
        .copied()
        .flatten()
        .map(|x| x.query())
        .next()
    }
    /// Turns the view into most of its parts
    pub fn destruct(self) -> Result<(FileState, FileState, DoubleHexContext), Self> {
        let cursor_index = self.cursor_index();
        // for now we only return if the cursor is at a positions where both indexes are actually
        // inside the file
        if let (Some(laddr), Some(raddr)) = (
            self.data.get_first_addr(cursor_index),
            self.data.get_second_addr(cursor_index),
        ) {
            let (lvec, rvec) = self.data.get_data();
            Ok((
                FileState {
                    name: self.filenames.0,
                    content: lvec,
                    index: laddr,
                    search: self.searches.0,
                },
                FileState {
                    name: self.filenames.1,
                    content: rvec,
                    index: raddr,
                    search: self.searches.1,
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
    searches: (Option<SearchResults>, Option<SearchResults>),
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
        first: FileState,
        second: FileState,
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
            searches: (first.search, second.search),
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
    fn search_ranges(&self) -> [Vec<(usize, usize)>; 2] {
        let intersect_range =
            |a: Range<isize>, b: Range<isize>| a.start.max(b.start)..a.end.min(b.end);
        let view_bounds = intersect_range(
            self.data.bounds(),
            self.index..self.index + self.dh.cursor.get_size() as isize,
        );
        let starts = self.data.get(view_bounds.start).map(|x| [x.xaddr, x.yaddr]);
        let ends = self
            .data
            .get(view_bounds.end - 1)
            .map(|x| [x.xaddr, x.yaddr]);
        if let ((Some(search1), Some(search2)), Some(starts), Some(ends)) =
            (&self.searches, starts, ends)
        {
            let ret: Vec<Vec<(usize, usize)>> = [search1, search2]
                .iter()
                .zip(starts.iter().zip(ends))
                .map(|(search, (start, end))| {
                    search.lookup_results(*start..end + 1).into_iter().collect()
                })
                .collect();
            [ret[0].clone(), ret[1].clone()]
        } else {
            [vec![], vec![]]
        }
    }
    /// Gets a useful form of the information contained in the alignement data for printing.
    fn get_content(&self) -> Vec<DoubleHexLine> {
        let mut content = Vec::new();
        let [first_range, second_range] = self.search_ranges();
        let mut next_first = first_range.into_iter().peekable();
        let mut next_second = second_range.into_iter().peekable();
        for x in 0..self.dh.cursor.get_size_y() {
            // address of current line to be converted
            let base_addr = (x * self.dh.cursor.get_size_x()) as isize + self.index;
            let mut bytes = Vec::new();
            for alignel in self
                .data
                .get_range(base_addr..base_addr + self.dh.cursor.get_size_x() as isize)
            {
                let malignel = match alignel {
                    Some(x) => x,
                    None => {
                        bytes.push((None, None));
                        continue;
                    }
                };
                let is_first_result = is_next_search_result(&mut next_first, malignel.xaddr);
                let is_second_result = is_next_search_result(&mut next_second, malignel.yaddr);
                let first = ByteData::maybe_new(malignel.xbyte, is_first_result);
                let second = ByteData::maybe_new(malignel.ybyte, is_second_result);
                bytes.push((first, second));
            }
            let address = self
                .data
                .get(base_addr)
                .map(|alignel| (Some(alignel.xaddr), Some(alignel.yaddr)))
                .unwrap_or_default();
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
        let (a, b) = self
            .data
            .get(cursor_index)
            .map(|alignel| (alignel.xbyte, alignel.ybyte))
            .unwrap_or_default();
        let addresses = self
            .data
            .get(cursor_index)
            .map(|alignel| (Some(alignel.xaddr), Some(alignel.yaddr)))
            .unwrap_or_default();
        let [a, b] = [
            (&self.searches.0, addresses.0, a),
            (&self.searches.1, addresses.1, b),
        ]
        .map(|(search, addr, byte)| {
            let is_search_result = search.as_ref().map_or(false, |s| s.is_in_result(addr));
            ByteData::maybe_new(byte, is_search_result)
        });
        self.dh
            .set_doublehex_cursor(printer, cursor_act, (a, b), addresses);
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
        let movement = if self.dh.style.right_to_left {
            movement.reflect_rtl()
        } else {
            movement
        };
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
    fn index_address(&self, right: bool, pos: usize) -> Result<isize, isize> {
        self.data.binary_search(&pos, |pos, el| {
            Some(*pos).cmp(&el.map(|a| if right { a.yaddr } else { a.xaddr }))
        })
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
            .index_address(right, pos)
            .map_err(|_| "Address does not (yet) exist")?;
        self.goto_index(printer, address_index);
        Ok(())
    }
    fn current_cursor_addresses(&self) -> Option<[usize; 2]> {
        self.data
            .get(self.cursor_index())
            .map(|x| [x.xaddr, x.yaddr])
    }
    pub fn jump_next_search_result<B: Backend>(&mut self, printer: &mut B) {
        let [first, second] = self
            .current_cursor_addresses()
            .or_else(|| self.data.first().map(|x| [x.xaddr, x.yaddr]))
            .unwrap_or([0, 0]);
        let next = match SearchResults::nearest_next_result(
            &[
                (&self.searches.0, first, false),
                (&self.searches.1, second, true),
            ],
            |addr, right| self.index_address(right, addr).ok(),
        ) {
            Some(x) => x,
            None => return,
        };
        self.goto_index(printer, next)
    }
    pub fn jump_prev_search_result<B: Backend>(&mut self, printer: &mut B) {
        let [first, second] = match self
            .current_cursor_addresses()
            .or_else(|| self.data.last().map(|x| [x.xaddr, x.yaddr]))
        {
            Some(x) => x,
            None => return,
        };
        let next = match SearchResults::nearest_prev_result(
            &[
                (&self.searches.0, first, false),
                (&self.searches.1, second, true),
            ],
            |addr, right| self.index_address(right, addr).ok(),
        ) {
            Some(x) => x,
            None => return,
        };
        self.goto_index(printer, next)
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
    pub fn add_search_results(
        &mut self,
        query: Query,
        results: Vec<Option<Range<usize>>>,
        first: bool,
    ) {
        let search = if first {
            &mut self.searches.0
        } else {
            &mut self.searches.1
        };
        let search = match search {
            Some(s) if s.query() == &query => s,
            _ => return,
        };
        for result in results.iter().flatten() {
            search.add_match(result.clone())
        }
    }
    pub fn clear_search(&mut self) {
        self.searches = (None, None);
    }
    pub fn setup_search(
        &mut self,
        query: Query,
    ) -> (
        (SearchContext, FileContent),
        Option<(SearchContext, FileContent)>,
    ) {
        let is_running = Arc::new(AtomicBool::new(true));
        self.searches.0 = Some(SearchResults::new(query.clone()));
        self.searches.1 = Some(SearchResults::new(query.clone()));
        (
            (
                SearchContext {
                    first: true,
                    query: query.clone(),
                    is_running: is_running.clone(),
                },
                self.original.0.clone(),
            ),
            Some((
                SearchContext {
                    first: false,
                    query,
                    is_running,
                },
                self.original.1.clone(),
            )),
        )
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
            Action::NextSearch => self.jump_next_search_result(printer),
            Action::PrevSearch => self.jump_prev_search_result(printer),
            _ => (),
        }
    }
    pub fn current_search_query(&self) -> Option<&Query> {
        [&self.searches.0, &self.searches.1]
            .iter()
            .copied()
            .flatten()
            .map(|x| x.query())
            .next()
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
    pub fn destruct(self) -> Result<(FileState, FileState, DoubleHexContext), Self> {
        // we return the original view in case the cursor is outside the files
        match (self.data.get(self.cursor_index())).map(|a| (a.xaddr, a.yaddr)) {
            Some((xaddr, yaddr)) => Ok((
                FileState {
                    name: self.filenames.0,
                    content: self.original.0,
                    index: xaddr,
                    search: self.searches.0,
                },
                FileState {
                    name: self.filenames.1,
                    content: self.original.1,
                    index: yaddr,
                    search: self.searches.1,
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

fn is_next_search_result<O: Ord + Copy>(
    iter: &mut Peekable<impl Iterator<Item = (O, O)>>,
    addr: O,
) -> bool {
    iter.peek()
        .filter(|(_, end)| end > &addr)
        .copied()
        .or_else(|| {
            iter.next();
            iter.peek().copied()
        })
        .map_or(false, |(start, end)| (start..end).contains(&addr))
}

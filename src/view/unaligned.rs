use std::{array::from_fn, ops::Range};

use cursive::{Vec2, View};

use crate::{
    backend::{Action, Backend, Cursiv},
    cursor::{CursorActive, Move},
    datastruct::{CompVec, SignedArray},
    doublehex::{DoubleHexContext, DoubleHexLine},
    file::{FileContent, FileState},
    search::{Query, SearchContext, SearchPair, SearchResults},
    selection::Selections,
    style::{ByteData, ColumnSetting},
};

use super::next_difference;
/// An unaligned view that is just two files next to each other
pub struct Unaligned {
    pub data: CompVec,
    filenames: (String, String),
    searches: SearchPair,
    selection: Selections,
    index: isize,
    pub dh: DoubleHexContext,
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
            searches: SearchPair(first.search, second.search),
            selection: Selections::new(),
            index,
            dh,
        }
    }
    /// Resizes the view without drawing it, returning if anything changed
    pub fn resize(&mut self, dimensions: (usize, usize)) -> bool {
        let (columns, rows) = dimensions;
        let old_dimensions = (self.dh.cursor.get_size_x(), self.dh.cursor.get_size_y());
        let (new_dimensions, bytes_per_row) = self.dh.style.get_doublehex_dims(columns, rows);
        self.index += self.dh.cursor.resize(new_dimensions, bytes_per_row);
        old_dimensions != new_dimensions
    }
    /// Redraws without checking for resize.
    /// clear indicates whether the screen should be cleared before.
    pub fn redraw<B: Backend>(&self, printer: &mut B, clear: bool) {
        if clear {
            printer.clear();
        }
        let content = self.get_content();
        self.dh.print_doublehex_screen(&content, printer);
        self.set_cursor(printer, self.dh.cursor_act);
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
        let idx = self.cursor_index();
        let addrs @ [addr0, addr1] = self.current_cursor_addresses();
        let [sel0, sel1] = self
            .selection
            .selection_status([idx, idx - self.data.shift]);

        let (a, b) = self.data.get(idx);
        let [a, b] = [
            (&self.searches.0, addr0, sel0, a),
            (&self.searches.1, addr1, sel1, b),
        ]
        .map(|(search, addr, sel, byte)| {
            let is_search_result = search.as_ref().map_or(false, |s| s.is_in_result(addr));
            ByteData::new(byte, is_search_result, sel)
        });
        self.dh
            .set_doublehex_cursor(printer, cursor_act, (a, b), addrs);
    }
    /// changes the active cursor to be cursor_act and moves back into bounds if the active cursor is outside bounds
    fn change_active_cursor<B: Backend>(&mut self, printer: &mut B, cursor_act: CursorActive) {
        self.dh.cursor_act = cursor_act;
        self.move_back_into_bounds(printer);
        self.set_cursor(printer, cursor_act);
        if self.selection.is_active() {
            self.redraw(printer, false)
        } else {
            printer.refresh()
        }
    }
    /// Converts the content of the CompVec into DoubleHexLines so they can be displayed
    fn get_content(&self) -> Vec<DoubleHexLine> {
        let mut content = Vec::new();
        for x in 0..self.dh.cursor.get_size_y() {
            // address of the nth line
            let base_addr = (x * self.dh.cursor.bytes_per_row()) as isize + self.index;
            let address = [
                self.data.get_first_addr(base_addr),
                self.data.get_second_addr(base_addr),
            ];
            let mut bytes = Vec::new();
            for (i, (byte_a, byte_b)) in self
                .data
                .get_range(base_addr..base_addr + self.dh.cursor.get_size_x() as isize)
                .into_iter()
                .enumerate()
            {
                let current_index = base_addr + i as isize;
                let addresses = [
                    self.data.get_first_addr(current_index),
                    self.data.get_second_addr(current_index),
                ];
                let [is_first_result, is_second_result] = self.searches.is_in_result(addresses);
                let [is_first_selected, is_second_selected] = self
                    .selection
                    .selection_status([current_index, current_index - self.data.shift]);
                bytes.push((
                    ByteData::new(byte_a, is_first_result, is_first_selected),
                    ByteData::new(byte_b, is_second_result, is_second_selected),
                ));
            }

            content.push(DoubleHexLine { address, bytes });
        }
        content
    }
    fn bytes_in_view(&self) -> [Vec<u8>; 2] {
        let mut ret = [vec![], vec![]];
        for (first, second) in self
            .data
            .get_range(self.index..self.index + self.dh.cursor.get_size() as isize)
        {
            if let Some(f) = first {
                ret[0].push(f);
            }
            if let Some(s) = second {
                ret[1].push(s);
            }
        }
        ret
    }
    pub fn set_shift(&mut self, shift: isize) {
        self.data.shift = shift;
    }
    /// Prints the top and bottom bar
    fn print_bars<B: Backend>(&self, printer: &mut B) {
        self.dh
            .print_title_line(printer, " unaligned", &self.filenames.0, &self.filenames.1);
        let addr = self.current_cursor_addresses();
        self.dh.print_bottom_line(printer, addr);
    }
    /// returns the bound of the index of the currently active cursor(s)
    fn active_data_bounds(&self) -> Range<isize> {
        match self.dh.cursor_act {
            CursorActive::Both | CursorActive::None => self.data.bounds(),
            CursorActive::First => self.data.first_bound(),
            CursorActive::Second => self.data.second_bound(),
        }
    }
    /// returns the current index of the cursor into the data
    fn cursor_index(&self) -> isize {
        self.index + self.dh.cursor.get_index() as isize
    }
    /// when the currently active cursor is outside of bounds, move it back
    /// into bounds
    pub fn move_back_into_bounds<B: Backend>(&mut self, printer: &mut B) {
        let old_active = self.dh.cursor_act;
        let bounds = self.active_data_bounds();
        self.dh.cursor_act = CursorActive::Both;
        let cursor_pos = self.cursor_index();
        let new_index = if cursor_pos < bounds.start {
            bounds.start
        } else if cursor_pos >= bounds.end {
            bounds.end - 1
        } else {
            self.dh.cursor_act = old_active;
            return;
        };
        self.goto_index(printer, new_index);
        self.dh.cursor_act = old_active;
    }
    /// go to an index on both sides, regardless of currently active cursor
    pub fn goto_index_both<B: Backend>(&mut self, printer: &mut B, index: isize) {
        let old_active = self.dh.cursor_act;
        self.dh.cursor_act = CursorActive::Both;
        self.goto_index(printer, index);
        self.dh.cursor_act = old_active;
    }
    /// set the current shift in the data to `shift` and jump to the common
    /// sequence of highest entropy length
    pub fn align_custom<B: Backend>(&mut self, printer: &mut B, shift: isize) {
        self.set_shift(shift);
        let hi_idx = self.data.highest_common_entropy();
        self.goto_index_both(printer, hi_idx);
    }
    /// align the starts of the data and jump to them
    pub fn align_start<B: Backend>(&mut self, printer: &mut B) {
        self.set_shift(0);
        self.goto_index_both(printer, 0);
    }
    /// align the ends of the data and jump to them
    pub fn align_end<B: Backend>(&mut self, printer: &mut B) {
        let diff = self.data.xvec.len() as isize - self.data.yvec.len() as isize;
        self.set_shift(diff);
        self.goto_index_both(printer, self.data.xvec.len() as isize - 1);
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
        let index_diff = match self.dh.cursor_act {
            CursorActive::Both => diff,
            CursorActive::First => self.data.add_first_shift(-diff),
            CursorActive::Second => self.data.add_second_shift(-diff),
            CursorActive::None => diff,
        };
        self.index += index_diff;
        let idx = self.cursor_index();
        self.selection
            .update([idx, idx - self.data.shift], self.dh.cursor_act);
        // if they are moved independently, we cannot scroll
        if !matches!(self.dh.cursor_act, CursorActive::Both) || self.selection.is_active() {
            self.redraw(printer, false);
        } else if let Some(scroll_amount) = self.dh.cursor.full_row_move(index_diff) {
            // scroll if we can
            let content = self.get_content();
            self.dh
                .print_doublehex_scrolled(&content, printer, scroll_amount);
            self.set_cursor(printer, self.dh.cursor_act);
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
            Action::NextDifference => self.jump_next_difference(printer, true, false),
            Action::NextInsertion => self.jump_next_difference(printer, true, true),
            Action::PrevDifference => self.jump_next_difference(printer, false, false),
            Action::PrevInsertion => self.jump_next_difference(printer, false, true),
            Action::Top => self.jump_start(printer),
            Action::Bottom => self.jump_end(printer),
            Action::NextSearch => self.jump_next_search_result(printer),
            Action::PrevSearch => self.jump_prev_search_result(printer),
            _ => (),
        }
    }
    /// Inreases the column count by one and refreshes the view
    pub fn add_column<B: Backend>(&mut self, printer: &mut B) {
        self.dh.inc_columns();
        self.refresh(printer);
    }
    /// Decreases the column count by one and refreshes the view
    pub fn remove_column<B: Backend>(&mut self, printer: &mut B) {
        self.dh.dec_columns();
        self.refresh(printer);
    }
    /// Sets the column count to the peak of the autocorrelation of
    /// the bytes in the current view and refreshes the view
    pub fn auto_column<B: Backend>(&mut self, printer: &mut B) {
        let selection = self.selection_file_ranges();
        let data = self.data.get_data();
        let [mut first, mut second] = if selection.iter().any(|x| x.is_some()) {
            self.clear_selection(printer);
            from_fn(|i| Some(data[i][selection[i].clone()?].to_vec()))
                .map(|x| x.unwrap_or_default())
        } else {
            self.bytes_in_view()
        };
        // set vectors to be empty if the cursor is not active
        if !self.dh.cursor_act.is_first() {
            first = Vec::new();
        }
        if !self.dh.cursor_act.is_second() {
            second = Vec::new();
        }
        self.dh.auto_columns([&first, &second]);
        self.refresh(printer);
    }
    pub fn start_selection<B: Backend>(&mut self, printer: &mut B) {
        let idx = self.cursor_index();
        self.selection
            .start([idx, idx - self.data.shift], self.dh.cursor_act);
        self.redraw(printer, false);
    }
    /// clears the selection with the currently active cursors
    pub fn clear_selection<B: Backend>(&mut self, printer: &mut B) {
        self.selection.clear(self.dh.cursor_act);
        self.redraw(printer, false);
    }
    /// Process a single action/event
    pub fn process_action<B: Backend>(&mut self, printer: &mut B, action: Action) {
        match action {
            Action::Refresh => self.refresh(printer),
            Action::CursorFirst => self.change_active_cursor(printer, CursorActive::First),
            Action::CursorBoth => self.change_active_cursor(printer, CursorActive::Both),
            Action::CursorSecond => self.change_active_cursor(printer, CursorActive::Second),
            Action::AddColumn => self.add_column(printer),
            Action::RemoveColumn => self.remove_column(printer),
            Action::AutoColumn => self.auto_column(printer),
            Action::StartSelection => self.start_selection(printer),
            Action::ClearSelection => self.clear_selection(printer),
            Action::ResetColumn => {
                self.dh.style.column_count = ColumnSetting::Fit;
                self.refresh(printer);
            }
            otherwise => self.process_move(printer, otherwise),
        }
    }
    /// jump to a given index with the currently active cursor
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
        if !right && !self.dh.cursor_act.is_first() {
            return Err(
                "Attempting to search on first view, but current cursor is on second view".into(),
            );
        } else if right && !self.dh.cursor_act.is_second() {
            return Err(
                "Attempting to search on second view, but current cursor is on first view".into(),
            );
        }
        let bounds = if !right {
            0..self.data.get_data()[0].len()
        } else {
            0..self.data.get_data()[1].len()
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
    /// get the file addresses of the current cursors
    fn current_cursor_addresses(&self) -> [Option<usize>; 2] {
        [
            self.data.get_first_addr(self.cursor_index()),
            self.data.get_second_addr(self.cursor_index()),
        ]
    }

    fn current_cursor_addresses_clamped(&self) -> [usize; 2] {
        let idx = [self.cursor_index(), self.cursor_index() - self.data.shift];
        let lens = self.data.get_data().map(|x| x.len());
        from_fn(|i| {
            let len = lens[i];
            if len == 0 {
                return 0;
            }
            idx[i].clamp(0, len as isize - 1) as usize
        })
    }

    pub fn selection_file_ranges(&self) -> [Option<Range<usize>>; 2] {
        let ranges = self.selection.ranges(self.dh.cursor_act);
        from_fn(|i| {
            let len = self.data.get_data().map(|x| x.len())[i];
            ranges[i].map(|[start, end]| {
                let [start, end] = [start, end + 1].map(|x| (x as usize).clamp(0, len));
                start..end
            })
        })
    }

    /// get the search results and positions of all active cursors
    fn search_data(&self) -> Vec<(&Option<SearchResults>, usize, bool)> {
        let [first, second] = self.current_cursor_addresses();
        first
            .map(|x| (&self.searches.0, x, false))
            .filter(|_| self.dh.cursor_act.is_first())
            .iter()
            .chain(
                second
                    .map(|x| (&self.searches.1, x, true))
                    .filter(|_| self.dh.cursor_act.is_second())
                    .iter(),
            )
            .copied()
            .collect()
    }
    /// get the file address of the current index, with the side given by `right`
    fn index_address(&self, right: bool, addr: usize) -> isize {
        if right {
            self.data.shift + addr as isize
        } else {
            addr as isize
        }
    }
    /// Jump to the next search result on either active cursor after the current index
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
    /// Jump to the previous search reult on either active cursor before the current index
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
    /// Jump to the index where the next bytes are different
    pub fn jump_next_difference<B: Backend>(
        &mut self,
        printer: &mut B,
        forward: bool,
        insertion: bool,
    ) {
        let target_address = next_difference(
            self.cursor_index(),
            self.data.bounds(),
            forward,
            |i| match self.data.get(i) {
                (None | Some(_), None) | (None, Some(_)) => true,
                (Some(a), Some(b)) => a != b && !insertion,
            },
        );
        self.goto_index(printer, target_address);
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
    /// Adds a batch of search results to the current ones if they are of the same query.
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
    /// Clears the search results of the currently active cursors
    pub fn clear_search(&mut self) {
        self.searches.clear(self.dh.cursor_act)
    }
    /// Initializes the empty search results for the search query
    /// on the currently active cursors
    pub fn setup_search(
        &mut self,
        query: Query,
    ) -> (
        (SearchContext, FileContent),
        Option<(SearchContext, FileContent)>,
    ) {
        let [first, second] = self.data.get_data();
        self.searches
            .setup_search(query, self.dh.cursor_act, [first, second])
    }
    /// Returns the active search query for one of the currently cursors
    pub fn current_search_query(&self) -> Option<&Query> {
        self.searches.current_search_query(self.dh.cursor_act)
    }
    /// Turns the view into most of its parts
    pub fn destruct(self) -> Result<(FileState, FileState, DoubleHexContext), Self> {
        // for now we only return if the cursor is at a positions where both indexes are actually
        // inside the file
        let [laddr, raddr] = self.current_cursor_addresses_clamped();
        let [lvec, rvec] = self.data.get_data();
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
    }
}

impl View for Unaligned {
    fn draw(&self, printer: &cursive::Printer) {
        let mut backend = Cursiv::from_printer(printer);
        self.redraw(&mut backend, true);
    }
    fn layout(&mut self, size: Vec2) {
        self.resize((size.x, size.y));
    }
}

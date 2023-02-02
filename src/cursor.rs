use std::ops::Range;

const VERTICAL_CURSOR_PAD: usize = 2;

/// Keeps track of display dimensions and cursor position
#[derive(Debug, Clone)]
pub struct CursorState {
    // (columns, rows)
    size: (usize, usize),
    cursor_pos: (usize, usize),
    bytes_per_row: usize,
}

impl CursorState {
    pub fn new(size: (usize, usize)) -> Self {
        Self {
            size,
            cursor_pos: (0, VERTICAL_CURSOR_PAD),
            bytes_per_row: size.0,
        }
    }
    /// Updates the screen size, changing the cursor position if neccessary.
    /// Returns the difference of the base address of the cursor view.
    pub fn resize(&mut self, size: (usize, usize), bytes_per_row: usize) -> isize {
        // refuse to resize too small and just keep the old size and draw nonsense instead
        if size.0 < 1 || size.1 < 2 * VERTICAL_CURSOR_PAD + 1 {
            return 0;
        }
        let prev_index = self.get_index();
        self.size = size;
        // modulo is a nice operation for truncating this, since this
        // will keep addresses mostly aligned with 8 (or 4 for smaller sizes)
        self.cursor_pos.0 %= self.get_size_x();
        self.cursor_pos.1 = self.get_y().clamp(self.min_row(), self.max_row());
        self.bytes_per_row = bytes_per_row;
        prev_index as isize - self.get_index() as isize
    }
    /// Jump the curser relative to current position.
    /// Returns relative change in (column, rows)
    pub fn jump(&self, diff: isize) -> (isize, isize) {
        let front_column = self.cursor_pos.0 as isize + diff;
        let row_change = front_column.div_euclid(self.bytes_per_row as isize);
        let column_change =
            front_column.rem_euclid(self.bytes_per_row as isize) - self.cursor_pos.0 as isize;
        (column_change, row_change)
    }
    /// gets the column of the cursor
    pub fn get_x(&self) -> usize {
        self.cursor_pos.0
    }
    /// gets the row of the cursor
    pub fn get_y(&self) -> usize {
        self.cursor_pos.1
    }
    /// gets the number of columns of the view rectangle
    pub fn get_size_x(&self) -> usize {
        self.size.0
    }
    /// gets the number of rows of the view rectangle
    pub fn get_size_y(&self) -> usize {
        self.size.1
    }
    /// gets the index of the cursor within the view rectangle
    pub fn get_index(&self) -> usize {
        self.get_y() * self.bytes_per_row + self.get_x()
    }
    /// gets the size of the view
    pub fn get_size(&self) -> usize {
        self.bytes_per_row * self.size.1
    }
    /// gets the number of bytes in a row
    /// this is different from the number of columns in case
    /// where the bytes would not all fit in a row
    pub fn bytes_per_row(&self) -> usize {
        self.bytes_per_row
    }
    /// Moves the cursor xdiff columns and ydiff rows
    /// Returns the change of position of the underlying view into
    /// the grid
    pub fn move_cursor_unbounded(&mut self, xdiff: isize, ydiff: isize) -> isize {
        let new_x = self.get_x() as isize + xdiff;
        let new_y = self.get_y() as isize + ydiff;
        let actual_x = new_x.clamp(0, self.size.0 as isize - 1);
        let actual_y = new_y.clamp(self.min_row() as isize, self.max_row() as isize);

        self.cursor_pos = (actual_x as usize, actual_y as usize);

        let dev_x = new_x - actual_x;
        let dev_y = new_y - actual_y;

        dev_y * self.bytes_per_row as isize + dev_x
    }
    /// when trying to move xdiff in the x direction,
    /// this returns the actual amount you can move without
    /// going out of bounds
    fn restrict_xdiff(&self, xdiff: isize, bounds: Range<isize>) -> isize {
        let cursor_pos = self.get_index() as isize;
        let xdiff_bounds = (bounds.start - cursor_pos)..(bounds.end - cursor_pos);
        xdiff.clamp(xdiff_bounds.start, xdiff_bounds.end - 1)
    }
    /// when trying to move ydiff in the y direction,
    /// this returns the actual amount you can move without
    /// going out of bounds
    fn restrict_ydiff(&self, ydiff: isize, bounds: Range<isize>) -> isize {
        let cursor_pos = self.get_index() as isize;
        let width = self.bytes_per_row as isize;
        let ydiff_bounds =
            ((bounds.start - cursor_pos) / width)..((bounds.end - cursor_pos - 1) / width);
        ydiff.clamp(ydiff_bounds.start, ydiff_bounds.end)
    }
    /// move cursor in x direction without going out of bounds
    pub fn move_cursor_x_bounded(&mut self, xdiff: isize, bounds: Range<isize>) -> isize {
        if bounds.is_empty() {
            return 0;
        }
        self.move_cursor_unbounded(self.restrict_xdiff(xdiff, bounds), 0)
    }
    /// move cursor in y direction without going out of bounds
    pub fn move_cursor_y_bounded(&mut self, ydiff: isize, bounds: Range<isize>) -> isize {
        if bounds.is_empty() {
            return 0;
        }
        self.move_cursor_unbounded(0, self.restrict_ydiff(ydiff, bounds))
    }
    /// move the view in x direction without going out of bounds
    pub fn move_view_x_bounded(&mut self, xdiff: isize, bounds: Range<isize>) -> isize {
        if bounds.is_empty() {
            return 0;
        }
        let width = self.get_size_x() as isize;
        let old_cursor_x = self.get_x() as isize;
        let new_cursor_x = (old_cursor_x - xdiff).clamp(0, width - 1);

        let actual_xdiff = self.restrict_xdiff(xdiff - old_cursor_x + new_cursor_x, bounds)
            + old_cursor_x
            - new_cursor_x;
        self.cursor_pos = (new_cursor_x as usize, self.get_y());

        actual_xdiff
    }
    /// move the view in y direction without going out of bounds
    pub fn move_view_y_bounded(&mut self, ydiff: isize, bounds: Range<isize>) -> isize {
        if bounds.is_empty() {
            return 0;
        }
        let old_cursor_y = self.get_y() as isize;
        let new_cursor_y =
            (old_cursor_y - ydiff).clamp(self.min_row() as isize, self.max_row() as isize);

        let actual_ydiff = self.restrict_ydiff(ydiff - old_cursor_y + new_cursor_y, bounds)
            + old_cursor_y
            - new_cursor_y;
        self.cursor_pos = (self.get_x(), new_cursor_y as usize);

        actual_ydiff * self.bytes_per_row as isize
    }
    /// moves according to the information in the move struct without going
    /// out of bounds
    pub fn mov(&mut self, movement: Move, bounds: Range<isize>) -> isize {
        match movement {
            Move::Unbounded(xdiff, ydiff) => self.move_cursor_unbounded(xdiff, ydiff),
            Move::CursorX(xdiff) => self.move_cursor_x_bounded(xdiff, bounds),
            Move::CursorY(ydiff) => self.move_cursor_y_bounded(ydiff, bounds),
            Move::ViewX(xdiff) => self.move_view_x_bounded(xdiff, bounds),
            Move::ViewY(ydiff) => self.move_view_y_bounded(ydiff, bounds),
        }
    }
    /// returns Some(amount of rows) if the difference given as argument
    /// is a multiple of the number of columns, otherwise None
    pub fn full_row_move(&self, diff: isize) -> Option<isize> {
        if diff % self.bytes_per_row as isize == 0 {
            Some(diff / self.bytes_per_row as isize)
        } else {
            None
        }
    }
    /// returns the maximum row the cursor may be at
    fn max_row(&self) -> usize {
        self.size.1.saturating_sub(VERTICAL_CURSOR_PAD + 1)
    }
    /// returns the minimum row the cursor may be at
    fn min_row(&self) -> usize {
        VERTICAL_CURSOR_PAD
    }
}

/// An enum for keeping track which views a cursor is enabled in
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorActive {
    Both,
    First,
    Second,
    None,
}
impl CursorActive {
    /// Cursor is enabled on the first view
    pub fn is_first(&self) -> bool {
        match self {
            Self::Both | Self::First => true,
            Self::Second | Self::None => false,
        }
    }
    /// Cursor is enabled on the second view
    pub fn is_second(&self) -> bool {
        match self {
            Self::Both | Self::Second => true,
            Self::First | Self::None => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Move {
    Unbounded(isize, isize),
    CursorX(isize),
    CursorY(isize),
    ViewX(isize),
    ViewY(isize),
}

impl Move {
    /// reflects the x direction in case right-to-left mode
    /// is enabled, since we do not actually want to mirror
    /// the x movements so we mirror them back
    pub fn reflect_rtl(self) -> Self {
        match self {
            // unbounded are used by internal functions,
            // who do not have a sense of right or left
            // so we do not have to invert x in this case
            Move::Unbounded(_, _) | Move::CursorY(_) | Move::ViewY(_) => self,
            Move::CursorX(x) => Move::CursorX(-x),
            Move::ViewX(x) => Move::ViewX(-x),
        }
    }
}

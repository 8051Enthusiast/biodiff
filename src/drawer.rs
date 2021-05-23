use crate::backend::{Backend, Color, Effect};

const MIDDLE_PAD: &str = "|";
const ADDR_SIZE: usize = 11;
const SIZE_PER_BYTE: usize = 3;
const CONSTANT_OVERHEAD: usize = ADDR_SIZE * 2 + MIDDLE_PAD.len();

/// Contains 8 digits of an address, with a space in between and at the front and end
fn disp_addr(maddr: Option<usize>) -> String {
    match maddr {
        Some(addr) => {
            let lo = addr & 0xffff;
            let hi = (addr >> 16) & 0xffff;
            format!(" {:04x} {:04x} ", hi, lo)
        }
        None => String::from("           "),
    }
}

/// Contains two hex digits of a byte and a space behind it, or just three spaces for None
fn disp_hex(h: Option<u8>) -> String {
    match h {
        Some(hex) => format!("{:02x} ", hex),
        None => String::from("   "),
    }
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_bytes(a: Option<u8>, b: Option<u8>) -> Color {
    match (a, b) {
        (Some(a), Some(b)) if a == b => Color::HexSame,
        (Some(_), Some(_)) => Color::HexDiff,
        (None, _) | (_, None) => Color::HexOneside,
    }
}

/// A line that can be printed using a backend for two hex views next to each other
#[derive(Debug, Clone)]
pub struct DoubleHexLine {
    pub address: (Option<usize>, Option<usize>),
    pub bytes: Vec<(Option<u8>, Option<u8>)>,
}

impl DoubleHexLine {
    /// Prints the DoubleHexLine using the given backend at the line given in `line`
    fn print<B: Backend>(&self, printer: &mut B, line: usize) {
        printer.set_line(line);
        printer.append_text(&disp_addr(self.address.0), Color::Unimportant, Effect::None);
        for (a, b) in &self.bytes {
            let s = disp_hex(*a);
            let color = color_from_bytes(*a, *b);
            printer.append_text(&s, color, Effect::None);
        }

        printer.append_text(MIDDLE_PAD, Color::Unimportant, Effect::None);
        printer.append_text(&disp_addr(self.address.1), Color::Unimportant, Effect::None);
        for (a, b) in &self.bytes {
            let s = disp_hex(*b);
            let color = color_from_bytes(*b, *a);
            printer.append_text(&s, color, Effect::None);
        }
    }
}

/// Prints a whole screen of hex data
pub fn print_doublehex_screen<B: Backend>(content: &[DoubleHexLine], backend: &mut B) {
    for (i, line) in content.iter().enumerate() {
        // we offset because of the title bar
        line.print(backend, i + 1);
    }
}

/// Scrolls the hex view and rewrites the missing content, which should be more efficient
pub fn print_doublehex_scrolled<B: Backend>(
    content: &[DoubleHexLine],
    backend: &mut B,
    rows: usize,
    scroll_amount: isize,
) {
    if scroll_amount == 0 {
        return;
    }
    if !backend.can_scroll() || scroll_amount.abs() as usize > content.len() {
        return print_doublehex_screen(content, backend);
    }
    backend.scroll(scroll_amount);
    for line in if scroll_amount > 0 {
        (rows - scroll_amount as usize)..rows
    } else {
        0..(-scroll_amount) as usize
    } {
        content[line].print(backend, line + 1);
    }
}

/// returns the number of columns that are displayed on a given display width
/// Goes in steps of 8 above 24, steps of 4 for 8 - 24 and steps of 1 for < 8
pub fn get_doublehex_columns(columns: usize) -> usize {
    if columns <= CONSTANT_OVERHEAD {
        return 0;
    }
    let max_col = (columns - CONSTANT_OVERHEAD) / (SIZE_PER_BYTE * 2);
    if max_col < 8 {
        max_col
    } else if max_col < 24 {
        max_col / 4 * 4
    } else {
        max_col / 8 * 8
    }
}

const VERTICAL_CURSOR_PAD: isize = 2;

/// Keeps track of display dimensions and cursor position
#[derive(Debug, Clone)]
pub struct CursorState {
    // (columns, rows)
    size: (usize, usize),
    cursor_pos: (usize, usize),
}

impl CursorState {
    pub fn new(size: (usize, usize)) -> Self {
        Self {
            size,
            cursor_pos: (0, VERTICAL_CURSOR_PAD as usize),
        }
    }
    /// Updates the screen size, changing the cursor position if neccessary.
    /// Returns the difference of the base address of the cursor view.
    pub fn resize(&mut self, size: (usize, usize)) -> isize {
        // refuse to resize too small and just keep the old size and draw nonsense instead
        if size.0 < 1 || size.1 < 2 * VERTICAL_CURSOR_PAD as usize + 1 {
            return 0;
        }
        let prev_index = self.get_index();
        self.size = size;
        // modulo is a nice operation for truncating this, since this
        // will keep addresses mostly aligned with 8 (or 4 for smaller sizes)
        self.cursor_pos.0 %= self.size.0;
        self.cursor_pos.1 = self.cursor_pos.1.clamp(
            VERTICAL_CURSOR_PAD as usize,
            self.size.1 - 1 - VERTICAL_CURSOR_PAD as usize,
        );
        prev_index as isize - self.get_index() as isize
    }
    /// Jump the curser relative to current position.
    /// Returns relative change in (column, rows)
    pub fn jump(&self, diff: isize) -> (isize, isize) {
        eprintln!("{}", diff);
        let front_column = self.cursor_pos.0 as isize + diff;
        eprintln!("{}", self.cursor_pos.0);
        let row_change = front_column.div_euclid(self.size.0 as isize);
        let column_change = front_column.rem_euclid(self.size.0 as isize) - self.cursor_pos.0 as isize;
        eprintln!("{} {}", column_change, row_change);
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
        self.get_y() * self.size.0 + self.get_x()
    }
    /// Moves the cursor xdiff columns and ydiff rows
    /// Returns the change of position of the underlying view into
    /// the grid
    pub fn move_cursor(&mut self, xdiff: isize, ydiff: isize) -> isize {
        let new_x = self.get_x() as isize + xdiff;
        let new_y = self.get_y() as isize + ydiff;
        let actual_x = new_x.clamp(0, self.size.0 as isize - 1);
        let actual_y = new_y.clamp(
            VERTICAL_CURSOR_PAD,
            self.size.1 as isize - 1 - VERTICAL_CURSOR_PAD,
        );

        self.cursor_pos = (actual_x as usize, actual_y as usize);

        let dev_x = new_x - actual_x;
        let dev_y = new_y - actual_y;

        dev_y * self.size.0 as isize + dev_x
    }
    /// returns Some(amount of rows) if the difference given as argument
    /// is a multiple of the number of columns, otherwise None
    pub fn full_row_move(&self, diff: isize) -> Option<isize> {
        if diff % self.size.0 as isize == 0 {
            Some(diff / self.size.0 as isize)
        } else {
            None
        }
    }
}

/// An enum for keeping track which views a cursor is enabled in
#[derive(Debug, Clone, Copy)]
pub enum CursorActive {
    Both,
    Left,
    Right,
    None,
}
impl CursorActive {
    /// Cursor is enabled on the left
    pub fn is_left(&self) -> bool {
        match self {
            Self::Both | Self::Left => true,
            Self::Right | Self::None => false,
        }
    }
    /// Cursor is enabled on the right
    pub fn is_right(&self) -> bool {
        match self {
            Self::Both | Self::Right => true,
            Self::Left | Self::None => false,
        }
    }
}

/// Refreshes the cursor position of a DoubleHex view.
/// at_cursor contains the bytes at the cursor
///
/// Note: The old cursor needs to be deleted first
/// by calling this function with CursorActive::None
/// with the old position.
pub fn set_doublehex_cursor<B: Backend>(
    backend: &mut B,
    cursor: &CursorState,
    active: CursorActive,
    at_cursor: (Option<u8>, Option<u8>),
) {
    // the cursor is displayed with reverse video
    let effect = |is_active| {
        if is_active {
            Effect::Inverted
        } else {
            Effect::None
        }
    };

    // left cursor
    let left_x = ADDR_SIZE + cursor.get_x() * SIZE_PER_BYTE;
    let left_effect = effect(active.is_left());
    let left_color = color_from_bytes(at_cursor.0, at_cursor.1);
    // note again that the title bar is skipped
    backend.set_pos(left_x, cursor.get_y() + 1);
    // we cut of the last byte of the disp_hex so that the space is not reverse video'd
    backend.append_text(&disp_hex(at_cursor.0)[..2], left_color, left_effect);

    // right cursor
    let right_x =
        2 * ADDR_SIZE + (cursor.get_x() + cursor.get_size_x()) * SIZE_PER_BYTE + MIDDLE_PAD.len();
    let right_effect = effect(active.is_right());
    let right_color = color_from_bytes(at_cursor.1, at_cursor.0);
    backend.set_pos(right_x, cursor.get_y() + 1);
    backend.append_text(&disp_hex(at_cursor.1)[..2], right_color, right_effect);
}

/// prints the line at the top containing the filenames and status
pub fn print_title_line<B: Backend>(
    printer: &mut B,
    title: &str,
    left: &str,
    right: &str,
    hexcols: usize,
) {
    // function for truncating the string on the left when it is too long
    // also inserts an < to indicate that it was truncated
    let shorten = |s: &str| -> String {
        if s.len() > SIZE_PER_BYTE * hexcols {
            s.chars()
                .rev()
                .take(SIZE_PER_BYTE * hexcols - 2)
                .collect::<Vec<_>>()
                .into_iter()
                .chain(std::iter::once('<'))
                .rev()
                .collect()
        } else {
            s.to_string()
        }
    };
    // the title is displayed above the addresses
    let short_title = title.chars().take(ADDR_SIZE - 1).collect::<String>();
    let short_left = shorten(left);
    let short_right = shorten(right);

    let titlebar = format!(
        "{:addrsize$} {:>hexsize$} {}{:addrsize$} {:>hexsize$} ",
        short_title,
        short_left,
        MIDDLE_PAD,
        " ",
        short_right,
        addrsize = ADDR_SIZE - 1,
        hexsize = hexcols * SIZE_PER_BYTE - 1
    );
    printer.set_line(0);
    printer.append_text(&titlebar, Color::HexSame, Effect::Inverted);
}

/// Prints the bottom text containing key information
pub fn print_bottom_line<B: Backend>(printer: &mut B, hexcols: usize, line: usize) {
    const BOTTOM_TEXT: &str = "F1: Help     F2: Unalign   F3: Align    F4: Settings F6: Goto";
    printer.set_line(line);
    printer.append_text(
        &format!(
            "{:width$}",
            BOTTOM_TEXT,
            width = hexcols * 2 * SIZE_PER_BYTE + CONSTANT_OVERHEAD
        ),
        Color::HexSame,
        Effect::Inverted,
    );
}

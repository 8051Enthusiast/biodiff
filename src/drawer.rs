use unicode_width::UnicodeWidthStr;

use crate::backend::{Backend, Color, Effect};

const MIDDLE_PAD: &str = "|";
const ADDR_SIZE: usize = 11;

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

fn disp_braille(h: Option<u8>) -> String {
    match h {
        Some(byte) => {
            let rbyte = byte.reverse_bits();
            let reordered_byte = rbyte & 0x87 | rbyte >> 1 & 0x38 | rbyte << 3 & 0x40;
            let braille_char = char::from_u32(0x2800u32 + reordered_byte as u32)
                .expect("Could not convert to braille codepoint");
            format!("│{}", braille_char)
        }
        None => String::from("  "),
    }
}

fn disp_mixed(h: Option<u8>) -> String {
    match h {
        Some(c @ b'!'..=b'~') => format!("{} ", char::from_u32(c as u32 + 0xFF00 - 0x20).unwrap()),
        Some(b' ') => String::from(".. "),
        Some(b'\n') => String::from("\\n "),
        Some(b'\t') => String::from("\\t "),
        Some(b'\r') => String::from("\\r "),
        Some(c) => format!("{:02x} ", c),
        None => String::from("   "),
    }
}

fn disp_ascii(h: Option<u8>) -> String {
    match h {
        Some(c @ b'!'..=b'~') => c as char,
        Some(_) => '.',
        None => ' ',
    }
    .into()
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_bytes(a: Option<u8>, b: Option<u8>) -> Color {
    match (a, b) {
        (Some(a), Some(b)) if a == b => Color::HexSame,
        (Some(_), Some(_)) => Color::HexDiff,
        (None, _) | (_, None) => Color::HexOneside,
    }
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_secondary_from_bytes(a: Option<u8>, b: Option<u8>) -> Color {
    match (a, b) {
        (Some(a), Some(b)) if a == b => Color::HexSameSecondary,
        (Some(_), Some(_)) => Color::HexDiffSecondary,
        (None, _) | (_, None) => Color::HexOnesideSecondary,
    }
}
/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_mixed_bytes(a: Option<u8>, b: Option<u8>) -> Color {
    if matches!(a, Some(b'!'..=b'~' | b' ' | b'\n' | b'\t' | b'\r')) {
        color_from_bytes(a, b)
    } else {
        color_secondary_from_bytes(a, b)
    }
}

#[repr(usize)]
#[derive(Clone, Copy, Debug)]
pub enum DisplayMode {
    HexOnly,
    HexAsciiMix,
    Braille,
}

impl DisplayMode {
    fn size_per_byte(&self) -> usize {
        match self {
            Self::HexOnly | Self::HexAsciiMix => 3,
            Self::Braille => 2,
        }
    }
    fn color(&self, a: Option<u8>, b: Option<u8>, row: usize) -> Color {
        match self {
            Self::HexOnly => color_from_bytes(a, b),
            Self::HexAsciiMix => color_from_mixed_bytes(a, b),
            Self::Braille => {
                if row % 2 == 0 {
                    color_from_bytes(a, b)
                } else {
                    color_secondary_from_bytes(a, b)
                }
            }
        }
    }
    fn disp(&self, a: Option<u8>, short: bool) -> String {
        let mut out = match self {
            Self::HexOnly => disp_hex(a),
            Self::HexAsciiMix => disp_mixed(a),
            Self::Braille => disp_braille(a),
        };
        if short && matches!(self, Self::HexOnly | Self::HexAsciiMix) {
            let _ = out.pop();
        }
        out
    }
}
/// A line that can be printed using a backend for two hex views next to each other
#[derive(Debug, Clone)]
pub struct DoubleHexLine {
    pub address: (Option<usize>, Option<usize>),
    pub bytes: Vec<(Option<u8>, Option<u8>)>,
}

impl DoubleHexLine {
    /// Prints one side of the line
    fn print_half<B>(&self, printer: &mut B, line: usize, style: Style, first: bool)
    where
        B: Backend,
    {
        let address = if first {
            self.address.0
        } else {
            self.address.1
        };
        let bytes: Vec<_> = self
            .bytes
            .iter()
            .map(|(a, b)| if first { (*a, *b) } else { (*b, *a) })
            .collect();
        printer.append_text(&disp_addr(address), Color::Unimportant, Effect::None);
        for (a, b) in &bytes {
            let s = style.mode.disp(*a, false);
            let color = style.mode.color(*a, *b, line);
            printer.append_text(&s, color, Effect::None);
        }
        if style.ascii_col {
            printer.append_text(MIDDLE_PAD, Color::Unimportant, Effect::None);
            for (a, b) in &bytes {
                let s = disp_ascii(*a);
                let color = style.mode.color(*a, *b, line);
                printer.append_text(&s, color, Effect::None);
            }
        }
    }
    /// Prints the DoubleHexLine using the given backend at the line given in `line`
    fn print<B>(&self, printer: &mut B, line: usize, style: Style)
    where
        B: Backend,
    {
        printer.set_line(line);
        self.print_half(printer, line, style, true);

        printer.append_text(MIDDLE_PAD, Color::Unimportant, Effect::None);
        self.print_half(printer, line, style, false);
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
        let front_column = self.cursor_pos.0 as isize + diff;
        let row_change = front_column.div_euclid(self.size.0 as isize);
        let column_change =
            front_column.rem_euclid(self.size.0 as isize) - self.cursor_pos.0 as isize;
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

#[derive(Clone, Copy, Debug)]
pub struct Style {
    pub mode: DisplayMode,
    pub ascii_col: bool,
}

impl Style {
    fn size_per_byte(&self) -> usize {
        self.mode.size_per_byte() + self.ascii_col as usize
    }
    fn const_overhead(&self) -> usize {
        ADDR_SIZE * 2 + MIDDLE_PAD.width() + if self.ascii_col { 2 } else { 0 }
    }
    /// returns the number of columns that are displayed on a given display width
    /// Goes in steps of 8 above 24, steps of 4 for 8 - 24 and steps of 1 for < 8
    pub fn get_doublehex_columns(&self, columns: usize) -> usize {
        if columns <= self.const_overhead() {
            return 0;
        }
        let max_col = (columns - self.const_overhead()) / (self.size_per_byte() * 2);
        if max_col < 8 {
            max_col
        } else if max_col < 24 {
            max_col / 4 * 4
        } else {
            max_col / 8 * 8
        }
    }
}

impl Default for Style {
    fn default() -> Self {
        Style {
            mode: DisplayMode::HexOnly,
            ascii_col: false,
        }
    }
}

pub struct DoubleHexContext {
    pub cursor: CursorState,
    pub style: Style,
}

impl DoubleHexContext {
    pub fn new(size: (usize, usize)) -> Self {
        let cursor = CursorState::new(size);
        DoubleHexContext {
            cursor,
            style: Style::default(),
        }
    }
    fn half_width(&self) -> usize {
        ADDR_SIZE
            + self.style.size_per_byte() * self.cursor.get_size_x()
            + if self.style.ascii_col { 2 } else { 1 } * MIDDLE_PAD.width()
    }
    fn full_width(&self) -> usize {
        self.style.const_overhead() + 2 * self.style.size_per_byte() * self.cursor.get_size_x()
    }
    /// Prints a whole screen of hex data
    pub fn print_doublehex_screen<B: Backend>(&self, content: &[DoubleHexLine], backend: &mut B) {
        for (i, line) in content.iter().enumerate() {
            // we offset because of the title bar
            line.print(backend, i + 1, self.style);
        }
    }

    /// Scrolls the hex view and rewrites the missing content, which should be more efficient
    pub fn print_doublehex_scrolled<B: Backend>(
        &self,
        content: &[DoubleHexLine],
        backend: &mut B,
        scroll_amount: isize,
    ) {
        let rows = self.cursor.get_size_y();
        if scroll_amount == 0 {
            return;
        }
        if !backend.can_scroll() || scroll_amount.abs() as usize > content.len() {
            return self.print_doublehex_screen(content, backend);
        }
        backend.scroll(scroll_amount);
        for line in if scroll_amount > 0 {
            (rows - scroll_amount as usize)..rows
        } else {
            0..(-scroll_amount) as usize
        } {
            content[line].print(backend, line + 1, self.style)
        }
    }

    /// Refreshes the cursor position of a DoubleHex view.
    /// at_cursor contains the bytes at the cursor
    ///
    /// Note: The old cursor needs to be deleted first
    /// by calling this function with CursorActive::None
    /// with the old position.
    pub fn set_doublehex_cursor<B: Backend>(
        &self,
        backend: &mut B,
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

        let cursor = &self.cursor;
        // left cursor
        let left_x = ADDR_SIZE + cursor.get_x() * self.style.mode.size_per_byte();
        let left_effect = effect(active.is_left());
        let left_color = self
            .style
            .mode
            .color(at_cursor.0, at_cursor.1, self.cursor.get_y() + 1);
        let left_text = self.style.mode.disp(at_cursor.0, true);
        // note again that the title bar is skipped
        backend.set_pos(left_x, cursor.get_y() + 1);
        // we cut of the last byte of the disp_hex so that the space is not reverse video'd
        backend.append_text(&left_text, left_color, left_effect);

        // right cursor
        let right_x = left_x + self.half_width();
        let right_effect = effect(active.is_right());
        let right_color = self
            .style
            .mode
            .color(at_cursor.1, at_cursor.0, self.cursor.get_y() + 1);
        let right_text = self.style.mode.disp(at_cursor.1, true);
        backend.set_pos(right_x, cursor.get_y() + 1);
        backend.append_text(&right_text, right_color, right_effect);
        if self.style.ascii_col {
            let left_ascii_x = ADDR_SIZE
                + MIDDLE_PAD.width()
                + cursor.get_x()
                + cursor.get_size_x() * self.style.mode.size_per_byte();
            backend.set_pos(left_ascii_x, cursor.get_y() + 1);
            backend.append_text(&disp_ascii(at_cursor.0), left_color, left_effect);
            let right_ascii_x = left_ascii_x + self.half_width();
            backend.set_pos(right_ascii_x, cursor.get_y() + 1);
            backend.append_text(&disp_ascii(at_cursor.1), right_color, right_effect);
        }
    }

    /// prints the line at the top containing the filenames and status
    pub fn print_title_line<B: Backend>(
        &self,
        printer: &mut B,
        title: &str,
        left: &str,
        right: &str,
    ) {
        // function for truncating the string on the left when it is too long
        // also inserts an < to indicate that it was truncated
        let shorten = |s: &str| -> String {
            if s.len() > self.style.size_per_byte() * self.cursor.get_size_x() {
                s.chars()
                    .rev()
                    .take(self.style.size_per_byte() * self.cursor.get_size_x() - 2)
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
            hexsize = self.cursor.get_size_x() * self.style.size_per_byte()
                - if self.style.ascii_col { 0 } else { 1 }
        );
        printer.set_line(0);
        printer.append_text(&titlebar, Color::HexSame, Effect::Inverted);
    }

    /// Prints the bottom text containing key information
    pub fn print_bottom_line<B: Backend>(&self, printer: &mut B, line: usize) {
        const BOTTOM_TEXT: &str = "F1: Help     F2: Unalign   F3: Align    F4: Settings F6: Goto";
        printer.set_line(line);
        printer.append_text(
            &format!("{:width$}", BOTTOM_TEXT, width = self.full_width()),
            Color::HexSame,
            Effect::Inverted,
        );
    }
}

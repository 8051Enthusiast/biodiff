use std::ops::Range;

use crate::backend::{Backend, Color, Effect};
use serde::{Deserialize, Serialize};
use unicode_width::UnicodeWidthStr;

const FRONT_PAD: &str = " ";
const MIDDLE_PAD: &str = " |";
const ADDR_SIZE: usize = 10;
const SPACER_PERIOD: usize = 8;

#[derive(Debug, Clone, Copy)]
pub struct ByteData {
    pub byte: u8,
    pub is_search_result: bool,
}

impl ByteData {
    pub fn maybe_new(byte: Option<u8>, is_search_result: bool) -> Option<Self> {
        Some(ByteData {
            byte: byte?,
            is_search_result,
        })
    }
}

fn byte(data: Option<ByteData>) -> Option<u8> {
    data.map(|x| x.byte)
}

/// Contains 8 digits of an address, with a space in between and at the front and end
fn disp_addr(maddr: Option<usize>) -> String {
    match maddr {
        Some(addr) => {
            let lo = addr & 0xffff;
            let hi = (addr >> 16) & 0xffff;
            format!("{:04x} {:04x} ", hi, lo)
        }
        None => String::from("          "),
    }
}

/// Formats the addresses that get displayed on the lower right of the screen
fn disp_bottom_addr(addresses: (Option<usize>, Option<usize>)) -> String {
    let formatted = |addr: Option<usize>| {
        addr.map(|x| format!("{:08x}", x))
            .unwrap_or_else(|| String::from("        "))
    };
    format!(" {}|{} ", formatted(addresses.0), formatted(addresses.1))
}

/// Contains two hex digits of a byte and a space behind it, or just three spaces for None
fn disp_hex(h: Option<u8>) -> String {
    match h {
        Some(hex) => format!("{:02x} ", hex),
        None => String::from("   "),
    }
}

fn disp_binary(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{:08b} ", bin),
        None => String::from("         "),
    }
}

fn disp_octal(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{:03o} ", bin),
        None => String::from("    "),
    }
}

fn disp_decimal(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{:>3} ", bin),
        None => String::from("    "),
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
        Some(b' ') => String::from("﹍ "),
        Some(b'\n') => String::from("\\n "),
        Some(b'\t') => String::from("\\t "),
        Some(b'\r') => String::from("\\r "),
        Some(c) => format!("{:02x} ", c),
        None => String::from("   "),
    }
}

fn disp_ascii(h: Option<u8>) -> String {
    match h {
        Some(c @ b' '..=b'~') => c as char,
        Some(_) => '.',
        None => ' ',
    }
    .into()
}

fn disp_column_blocks(h: Option<u8>) -> String {
    match h {
        Some(0) => String::from(" "),
        Some(c @ 1..=255) => format!("{}", char::from_u32((c as u32 + 31) / 32 + 0x2580).unwrap()),
        None => String::from("░"),
    }
}

fn disp_roman(h: Option<u8>) -> String {
    const ONES: [&str; 10] = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"];
    const TENS: [&str; 10] = ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"];
    const HUNDREDS: [&str; 3] = ["", "C", "CC"];
    let s = match h {
        Some(0) => String::from("N"),
        Some(n) => {
            let one = n % 10;
            let ten = (n / 10) % 10;
            let hundred = n / 100;
            format!(
                "{}{}{}",
                HUNDREDS[hundred as usize], TENS[ten as usize], ONES[one as usize]
            )
        }
        None => String::new(),
    };
    format!("{:>9} ", s)
}

fn byte_effect(x: Option<ByteData>) -> Effect {
    x.filter(|x| x.is_search_result)
        .map_or(Effect::None, |_| Effect::Bold)
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_bytes(a: Option<ByteData>, b: Option<ByteData>) -> Color {
    match (a, b) {
        (Some(a), Some(b)) if a.byte == b.byte => Color::HexSame,
        (Some(_), Some(_)) => Color::HexDiff,
        (None, _) | (_, None) => Color::HexOneside,
    }
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_secondary_from_bytes(a: Option<ByteData>, b: Option<ByteData>) -> Color {
    match (a, b) {
        (Some(a), Some(b)) if a.byte == b.byte => Color::HexSameSecondary,
        (Some(_), Some(_)) => Color::HexDiffSecondary,
        (None, _) | (_, None) => Color::HexOnesideSecondary,
    }
}
/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_mixed_bytes(a: Option<ByteData>, b: Option<ByteData>) -> Color {
    if matches!(byte(a), Some(b'!'..=b'~' | b' ' | b'\n' | b'\t' | b'\r')) {
        color_from_bytes(a, b)
    } else {
        color_secondary_from_bytes(a, b)
    }
}

#[repr(usize)]
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum DisplayMode {
    Hex = 0,
    Binary = 1,
    Decimal = 2,
    Octal = 3,
    HexAsciiMix = 4,
    Braille = 5,
    Roman = 6,
}

impl Default for DisplayMode {
    fn default() -> Self {
        DisplayMode::Hex
    }
}

impl DisplayMode {
    fn size_per_byte(&self) -> usize {
        match self {
            Self::Roman => 10,
            Self::Binary => 9,
            Self::Decimal | Self::Octal => 4,
            Self::Hex | Self::HexAsciiMix => 3,
            Self::Braille => 2,
        }
    }
    fn color(&self, a: Option<ByteData>, b: Option<ByteData>, row: usize) -> Color {
        match self {
            Self::HexAsciiMix => color_from_mixed_bytes(a, b),
            Self::Braille => {
                if row % 2 == 0 {
                    color_from_bytes(a, b)
                } else {
                    color_secondary_from_bytes(a, b)
                }
            }
            _otherwise => color_from_bytes(a, b),
        }
    }
    fn disp(&self, a: Option<u8>, short: bool) -> String {
        let mut out = match self {
            Self::Hex => disp_hex(a),
            Self::Binary => disp_binary(a),
            Self::Decimal => disp_decimal(a),
            Self::Octal => disp_octal(a),
            Self::HexAsciiMix => disp_mixed(a),
            Self::Braille => disp_braille(a),
            Self::Roman => disp_roman(a),
        };
        if short && matches!(self, Self::Hex | Self::HexAsciiMix) {
            let _ = out.pop();
        }
        out
    }
    fn can_scroll(&self) -> bool {
        !matches!(self, Self::Braille)
    }
}
/// A line that can be printed using a backend for two hex views next to each other
#[derive(Debug, Clone)]
pub struct DoubleHexLine {
    pub address: (Option<usize>, Option<usize>),
    pub bytes: Vec<(Option<ByteData>, Option<ByteData>)>,
}

impl DoubleHexLine {
    /// Prints one side of the line
    fn print_half<B>(&self, printer: &mut B, line: usize, style: Style, first: bool)
    where
        B: Backend,
    {
        printer.append_text(FRONT_PAD, Color::Unimportant, Effect::None);
        let address = if first {
            self.address.0
        } else {
            self.address.1
        };
        let mut bytes = vec![(None, None); self.bytes.len()];
        for (i, (a, b)) in self.bytes.iter().enumerate() {
            let target = if style.right_to_left {
                &mut bytes[self.bytes.len() - 1 - i]
            } else {
                &mut bytes[i]
            };
            *target = if first { (*a, *b) } else { (*b, *a) }
        }
        if !style.right_to_left {
            printer.append_text(&disp_addr(address), Color::Unimportant, Effect::None);
        }
        let width = self.bytes.len();
        for (i, (a, b)) in bytes.iter().enumerate() {
            let s = style.mode.disp(byte(*a), false);
            let color = style.mode.color(*a, *b, line);
            let effect = byte_effect(*a);
            printer.append_text(&s, color, effect);
            if style.spacer && i + 1 != width && i % 8 == 7 {
                printer.append_text(" ", color, effect);
            }
        }
        if style.right_to_left {
            printer.append_text(&disp_addr(address), Color::Unimportant, Effect::None);
        }
        for col_disp in [
            (style.ascii_col, disp_ascii as fn(_) -> _),
            (style.bars_col, disp_column_blocks),
        ]
        .iter()
        .filter_map(|(c, d)| c.then(|| d))
        {
            printer.append_text(MIDDLE_PAD, Color::Unimportant, Effect::None);
            for (a, b) in &bytes {
                let s = col_disp(byte(*a));
                let color = style.mode.color(*a, *b, line);
                let effect = byte_effect(*a);
                printer.append_text(&s, color, effect);
            }
        }
    }
    /// Prints the DoubleHexLine using the given backend at the line given in `line`
    /// with the views being on the left and right
    fn print_hor<B: Backend>(&self, printer: &mut B, line: usize, style: Style) {
        printer.set_line(line);
        self.print_half(printer, line, style, true);

        printer.append_text(MIDDLE_PAD, Color::Unimportant, Effect::None);
        self.print_half(printer, line, style, false);
    }

    fn print_vert<B: Backend>(&self, printer: &mut B, lines: [usize; 2], style: Style) {
        printer.set_line(lines[0]);
        self.print_half(printer, lines[0], style, true);

        printer.set_line(lines[1]);
        self.print_half(printer, lines[1], style, false);
    }
}

const VERTICAL_CURSOR_PAD: usize = 2;

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
            cursor_pos: (0, VERTICAL_CURSOR_PAD),
        }
    }
    /// Updates the screen size, changing the cursor position if neccessary.
    /// Returns the difference of the base address of the cursor view.
    pub fn resize(&mut self, size: (usize, usize)) -> isize {
        // refuse to resize too small and just keep the old size and draw nonsense instead
        if size.0 < 1 || size.1 < 2 * VERTICAL_CURSOR_PAD + 1 {
            return 0;
        }
        let prev_index = self.get_index();
        self.size = size;
        // modulo is a nice operation for truncating this, since this
        // will keep addresses mostly aligned with 8 (or 4 for smaller sizes)
        self.cursor_pos.0 %= self.size.0;
        self.cursor_pos.1 = self.cursor_pos.1.clamp(self.min_row(), self.max_row());
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
    /// gets the size of the view
    pub fn get_size(&self) -> usize {
        self.size.0 * self.size.1
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

        dev_y * self.size.0 as isize + dev_x
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
        let width = self.get_size_x() as isize;
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
        let width = self.get_size_x() as isize;
        let old_cursor_y = self.get_y() as isize;
        let new_cursor_y =
            (old_cursor_y - ydiff).clamp(self.min_row() as isize, self.max_row() as isize);

        let actual_ydiff = self.restrict_ydiff(ydiff - old_cursor_y + new_cursor_y, bounds)
            + old_cursor_y
            - new_cursor_y;
        self.cursor_pos = (self.get_x(), new_cursor_y as usize);

        actual_ydiff * width
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
        if diff % self.size.0 as isize == 0 {
            Some(diff / self.size.0 as isize)
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
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct Style {
    pub mode: DisplayMode,
    pub ascii_col: bool,
    pub bars_col: bool,
    pub vertical: bool,
    pub spacer: bool,
    pub right_to_left: bool,
}

impl Style {
    fn size_per_byte(&self) -> usize {
        self.mode.size_per_byte() + self.ascii_col as usize + self.bars_col as usize
    }
    /// width of n columns
    ///
    /// (note that this is differnt than calling nth_column_pos
    /// and adding an cursor offset because rtl could be enabled)
    pub fn n_column_width(&self, n: usize) -> usize {
        if n == 0 {
            return 0;
        }
        self.mode.size_per_byte() * n
            + if self.spacer {
                (n + SPACER_PERIOD - 1) / SPACER_PERIOD - 1
            } else {
                0
            }
    }
    /// width of one ascii column
    pub fn ascii_width(&self, n: usize) -> usize {
        if self.ascii_col {
            MIDDLE_PAD.width() + n
        } else {
            0
        }
    }
    /// physical column of the start of the ascii column, if it exists
    pub fn ascii_start(&self, n: usize) -> Option<usize> {
        if self.ascii_col {
            Some(ADDR_SIZE + FRONT_PAD.width() + self.n_column_width(n) + MIDDLE_PAD.width())
        } else {
            None
        }
    }
    /// width of one bars column
    pub fn bars_width(&self, n: usize) -> usize {
        if self.bars_col {
            MIDDLE_PAD.width() + n
        } else {
            0
        }
    }
    /// physical column of the start of the bars column, if it exists
    pub fn bars_start(&self, n: usize) -> Option<usize> {
        if self.bars_col {
            Some(
                ADDR_SIZE
                    + FRONT_PAD.width()
                    + self.n_column_width(n)
                    + self.ascii_width(n)
                    + MIDDLE_PAD.width(),
            )
        } else {
            None
        }
    }
    /// width of one of the two hex screens
    pub fn half_width(&self, n: usize) -> usize {
        ADDR_SIZE
            + FRONT_PAD.width()
            + self.ascii_width(n)
            + self.bars_width(n)
            + self.n_column_width(n)
    }
    /// the position of the first character of the nth hex column
    pub fn nth_column_pos(&self, n: usize) -> usize {
        self.mode.size_per_byte() * n
            + if self.spacer { n / SPACER_PERIOD } else { 0 }
            + if self.right_to_left { 0 } else { ADDR_SIZE }
            + FRONT_PAD.width()
    }
    /// the amount of characters that are on a line regardless of column number
    fn const_overhead(&self) -> usize {
        let single_overhead = ADDR_SIZE
            + FRONT_PAD.width()
            + if self.ascii_col {
                MIDDLE_PAD.width()
            } else {
                0
            }
            + if self.bars_col { MIDDLE_PAD.width() } else { 0 };
        if self.vertical {
            single_overhead
        } else {
            2 * single_overhead + MIDDLE_PAD.width()
        }
    }
    /// returns the number of columns that are displayed on a given display width
    /// Goes in steps of 8 above 24, steps of 4 for 8 - 24 and steps of 1 for < 8
    pub fn get_doublehex_dims(&self, columns: usize, rows: usize) -> (usize, usize) {
        let y = if self.vertical {
            rows.saturating_sub(3) / 2
        } else {
            rows.saturating_sub(2)
        };
        let x = if columns <= self.const_overhead() {
            1
        } else {
            let available_col = columns - self.const_overhead();
            let multiplicity = if self.vertical { 1 } else { 2 };
            let unit_width = self.size_per_byte() * multiplicity;
            // take out one space from the available columns for each 8 units
            let without_spacer = if self.spacer {
                available_col
                    - available_col / ((self.size_per_byte() * SPACER_PERIOD + 1) * multiplicity)
                        * multiplicity
            } else {
                available_col
            };
            let max_col = without_spacer / unit_width;
            if max_col < 8 {
                max_col
            } else if max_col < 24 {
                max_col / 4 * 4
            } else {
                max_col / 8 * 8
            }
        };
        (x, y)
    }
}

impl Default for Style {
    fn default() -> Self {
        Style {
            mode: DisplayMode::Hex,
            ascii_col: false,
            bars_col: false,
            vertical: false,
            spacer: false,
            right_to_left: false,
        }
    }
}

pub struct DoubleHexContext {
    pub cursor: CursorState,
    pub style: Style,
}

impl DoubleHexContext {
    /// Creates a new DoubleHexContext with default style
    pub fn new(size: (usize, usize)) -> Self {
        let cursor = CursorState::new(size);
        DoubleHexContext {
            cursor,
            style: Style::default(),
        }
    }
    /// width of a screen half when in horizontal split
    fn hor_half_width(&self) -> usize {
        self.style.half_width(self.cursor.get_size_x())
    }
    /// height of a screen half when in vertical split
    fn vert_half_height(&self) -> usize {
        self.cursor.get_size_y() + 1
    }
    fn full_width(&self) -> usize {
        if self.style.vertical {
            self.hor_half_width()
        } else {
            2 * self.hor_half_width() + MIDDLE_PAD.width()
        }
    }
    fn full_height(&self) -> usize {
        if self.style.vertical {
            self.cursor.get_size_y() * 2 + 3
        } else {
            self.cursor.get_size_y() + 2
        }
    }
    /// Prints a whole screen of hex data
    pub fn print_doublehex_screen<B: Backend>(&self, content: &[DoubleHexLine], backend: &mut B) {
        for (i, line) in content.iter().enumerate() {
            if self.style.vertical {
                line.print_vert(
                    backend,
                    [i + 1, self.vert_half_height() + i + 1],
                    self.style,
                );
            } else {
                // we offset because of the title bar
                line.print_hor(backend, i + 1, self.style);
            }
        }
    }
    /// returns the logical column of the cursor
    fn col(&self) -> usize {
        if self.style.right_to_left {
            self.cursor.get_size_x() - 1 - self.cursor.get_x()
        } else {
            self.cursor.get_x()
        }
    }
    /// returns the position of the first cursor on the hex view
    fn first_cursor(&self) -> (usize, usize) {
        let ret_x = self.style.nth_column_pos(self.col());
        let ret_y = self.cursor.get_y() + 1;
        (ret_x, ret_y)
    }

    /// returns the position of the first cursor on the ascii view
    fn first_cursor_ascii(&self) -> Option<(usize, usize)> {
        let pos = self.style.ascii_start(self.cursor.get_size_x())?;
        let ret_x = pos + self.col();
        let ret_y = self.cursor.get_y() + 1;
        Some((ret_x, ret_y))
    }
    /// returns the position of the first cursor on the bars view
    fn first_cursor_bars(&self) -> Option<(usize, usize)> {
        let pos = self.style.bars_start(self.cursor.get_size_x())?;
        let ret_x = pos + self.col();
        let ret_y = self.cursor.get_y() + 1;
        Some((ret_x, ret_y))
    }
    /// converts a position in the first half into one of the second half
    fn shift_to_second(&self, pos: (usize, usize)) -> (usize, usize) {
        if self.style.vertical {
            (pos.0, pos.1 + self.vert_half_height())
        } else {
            (pos.0 + self.hor_half_width() + MIDDLE_PAD.width(), pos.1)
        }
    }
    /// returns the position of the second cursor on the hex view
    fn second_cursor(&self) -> (usize, usize) {
        let first = self.first_cursor();
        self.shift_to_second(first)
    }

    /// returns the position of the second cursor on the ascii view
    fn second_cursor_ascii(&self) -> Option<(usize, usize)> {
        let first = self.first_cursor_ascii()?;
        Some(self.shift_to_second(first))
    }

    /// returns the position of the second cursor on the bars view
    fn second_cursor_bars(&self) -> Option<(usize, usize)> {
        let first = self.first_cursor_bars()?;
        Some(self.shift_to_second(first))
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
        if !backend.can_scroll()
            || !self.style.mode.can_scroll()
            || scroll_amount.abs() as usize > content.len()
        {
            return self.print_doublehex_screen(content, backend);
        }
        backend.scroll(scroll_amount);
        for line in if scroll_amount > 0 {
            (rows - scroll_amount as usize)..rows
        } else {
            0..(-scroll_amount) as usize
        } {
            if self.style.vertical {
                content[line].print_vert(
                    backend,
                    [line + 1, self.vert_half_height() + line + 1],
                    self.style,
                )
            } else {
                content[line].print_hor(backend, line + 1, self.style)
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
        &self,
        backend: &mut B,
        active: CursorActive,
        at_cursor: (Option<ByteData>, Option<ByteData>),
        cursor_addr: (Option<usize>, Option<usize>),
    ) {
        // the cursor is displayed with reverse video
        let effect = |is_active, byte: Option<ByteData>| {
            if is_active {
                Effect::Inverted
            } else {
                match byte {
                    Some(ByteData {
                        is_search_result: true,
                        ..
                    }) => Effect::Bold,
                    _ => Effect::None,
                }
            }
        };

        // first cursor
        let (first_x, first_y) = self.first_cursor();
        let first_effect = effect(active.is_first(), at_cursor.0);
        let first_color = self.style.mode.color(at_cursor.0, at_cursor.1, first_y);
        let first_text = self.style.mode.disp(byte(at_cursor.0), true);
        // note again that the title bar is skipped
        backend.set_pos(first_x, first_y);
        // we cut of the last byte of the disp_hex so that the space is not reverse video'd
        backend.append_text(&first_text, first_color, first_effect);
        // first ascii and bars column
        for (fx, fy, disp_col) in [
            (self.first_cursor_ascii(), disp_ascii as fn(_) -> _),
            (self.first_cursor_bars(), disp_column_blocks),
        ]
        .iter()
        .filter_map(|(a, b)| a.map(|(a0, a1)| (a0, a1, b)))
        {
            backend.set_pos(fx, fy);
            backend.append_text(&disp_col(byte(at_cursor.0)), first_color, first_effect);
        }

        // second cursor
        let (second_x, second_y) = self.second_cursor();
        let second_effect = effect(active.is_second(), at_cursor.1);
        let second_color = self.style.mode.color(at_cursor.1, at_cursor.0, second_y);
        let second_text = self.style.mode.disp(byte(at_cursor.1), true);
        backend.set_pos(second_x, second_y);
        backend.append_text(&second_text, second_color, second_effect);
        // second ascii and bars column
        for (sx, sy, disp_col) in [
            (self.second_cursor_ascii(), disp_ascii as fn(_) -> _),
            (self.second_cursor_bars(), disp_column_blocks),
        ]
        .iter()
        .filter_map(|(a, b)| a.map(|(a0, a1)| (a0, a1, b)))
        {
            backend.set_pos(sx, sy);
            backend.append_text(&disp_col(byte(at_cursor.1)), second_color, second_effect);
        }

        // status bar address
        let addr_print = disp_bottom_addr(cursor_addr);
        if self.style.right_to_left {
            backend.set_pos(0, self.full_height() - 1);
        } else {
            backend.set_pos(
                self.full_width().saturating_sub(addr_print.chars().count()),
                self.full_height() - 1,
            );
        }
        backend.append_text(&addr_print, Color::HexSame, Effect::Inverted);
    }

    /// prints the line at the top containing the filenames and status
    pub fn print_title_line<B: Backend>(
        &self,
        printer: &mut B,
        title: &str,
        first: &str,
        second: &str,
    ) {
        // function for truncating the string on the left when it is too long
        // also inserts an < to indicate that it was truncated
        let shorten = |s: &str| -> String {
            if s.len() > self.style.n_column_width(self.cursor.get_size_x()) {
                s.chars()
                    .rev()
                    .take(self.style.n_column_width(self.cursor.get_size_x()) - 2)
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
        let addrsize = ADDR_SIZE;
        let short_title = title.chars().take(addrsize).collect::<String>();
        let short_first = shorten(first);
        let short_second = shorten(second);

        let hexsize = self.hor_half_width() - ADDR_SIZE - 2;
        let format_title = |text| {
            if self.style.right_to_left {
                format!(
                    "{:<hexsize$} {:addrsize$} ",
                    text,
                    short_title,
                    addrsize = addrsize,
                    hexsize = hexsize
                )
            } else {
                format!(
                    "{:addrsize$} {:>hexsize$} ",
                    short_title,
                    text,
                    addrsize = addrsize,
                    hexsize = hexsize
                )
            }
        };
        let first_title = format_title(short_first);
        printer.set_line(0);
        printer.append_text(&first_title, Color::HexSame, Effect::Inverted);
        if self.style.vertical {
            printer.set_line(self.vert_half_height())
        } else {
            printer.append_text(MIDDLE_PAD, Color::HexSame, Effect::Inverted);
        }
        let second_title = format_title(short_second);
        printer.append_text(&second_title, Color::HexSame, Effect::Inverted);
    }

    /// Prints the bottom text containing key information
    pub fn print_bottom_line<B: Backend>(
        &self,
        printer: &mut B,
        addresses: (Option<usize>, Option<usize>),
    ) {
        const BOTTOM_TEXT: &str =
            "F1: Help     F2: Unalign   F3: Align    F4: Settings F6: Goto     F7: Search ";
        let print_addr = disp_bottom_addr(addresses);
        let info_width = self.full_width().saturating_sub(print_addr.chars().count());
        let bottom_text = BOTTOM_TEXT.chars().take(info_width).collect::<String>();
        let info_text = if self.style.right_to_left {
            format!(
                "{}{:>info_width$}",
                print_addr,
                bottom_text,
                info_width = info_width
            )
        } else {
            format!(
                "{:<info_width$}{}",
                bottom_text,
                print_addr,
                info_width = info_width
            )
        };
        let line = self.full_height() - 1;
        printer.set_line(line);
        printer.append_text(&info_text, Color::HexSame, Effect::Inverted);
        for line in self.full_height()..printer.size().1 {
            printer.set_line(line);
            printer.append_text(
                &format!("{:width$}", " ", width = self.full_width()),
                Color::HexSame,
                Effect::None,
            );
        }
    }
}

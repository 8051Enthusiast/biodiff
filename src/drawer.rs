use crate::backend::{Backend, Color, Effect};
use serde::{Deserialize, Serialize};
use unicode_width::UnicodeWidthStr;

const MIDDLE_PAD: &str = " |";
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
        Some(c @ b'!'..=b'~') => c as char,
        Some(_) => '.',
        None => ' ',
    }
    .into()
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
    fn color(&self, a: Option<u8>, b: Option<u8>, row: usize) -> Color {
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

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Style {
    pub mode: DisplayMode,
    pub ascii_col: bool,
    pub vertical: bool,
}

impl Style {
    fn size_per_byte(&self) -> usize {
        self.mode.size_per_byte() + self.ascii_col as usize
    }
    fn const_overhead(&self) -> usize {
        let single_overhead = ADDR_SIZE
            + if self.ascii_col {
                MIDDLE_PAD.width()
            } else {
                0
            };
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
            (rows - 3) / 2
        } else {
            rows - 2
        };
        if columns <= self.const_overhead() {
            return (1, y);
        }
        let max_col = (columns - self.const_overhead())
            / (self.size_per_byte() * if self.vertical { 1 } else { 2 });
        let x = if max_col < 8 {
            max_col
        } else if max_col < 24 {
            max_col / 4 * 4
        } else {
            max_col / 8 * 8
        };
        (x, y)
    }
}

impl Default for Style {
    fn default() -> Self {
        Style {
            mode: DisplayMode::Hex,
            ascii_col: false,
            vertical: false,
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
    fn hor_half_width(&self) -> usize {
        ADDR_SIZE
            + self.style.size_per_byte() * self.cursor.get_size_x()
            + if self.style.ascii_col { 2 } else { 1 } * MIDDLE_PAD.width()
    }
    fn vert_half_height(&self) -> usize {
        self.cursor.get_size_y() + 1
    }
    fn full_width(&self) -> usize {
        if self.style.vertical {
            self.style.const_overhead() + self.style.size_per_byte() * self.cursor.get_size_x()
        } else {
            self.style.const_overhead() + 2 * self.style.size_per_byte() * self.cursor.get_size_x()
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

    fn first_cursor(&self) -> (usize, usize) {
        let ret_x = ADDR_SIZE + self.cursor.get_x() * self.style.mode.size_per_byte();
        let ret_y = self.cursor.get_y() + 1;
        (ret_x, ret_y)
    }

    fn first_cursor_ascii(&self) -> Option<(usize, usize)> {
        if !self.style.ascii_col {
            return None;
        }
        let ret_x = ADDR_SIZE
            + MIDDLE_PAD.width()
            + self.cursor.get_x()
            + self.cursor.get_size_x() * self.style.mode.size_per_byte();
        let ret_y = self.cursor.get_y() + 1;
        Some((ret_x, ret_y))
    }

    fn second_cursor(&self) -> (usize, usize) {
        let (first_x, first_y) = self.first_cursor();
        if self.style.vertical {
            (first_x, first_y + self.vert_half_height())
        } else {
            (first_x + self.hor_half_width(), first_y)
        }
    }

    fn second_cursor_ascii(&self) -> Option<(usize, usize)> {
        let (first_x, first_y) = self.first_cursor_ascii()?;
        if self.style.vertical {
            Some((first_x, first_y + self.vert_half_height()))
        } else {
            Some((first_x + self.hor_half_width(), first_y))
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

        // first cursor
        let (first_x, first_y) = self.first_cursor();
        let first_effect = effect(active.is_first());
        let first_color = self.style.mode.color(at_cursor.0, at_cursor.1, first_y);
        let first_text = self.style.mode.disp(at_cursor.0, true);
        // note again that the title bar is skipped
        backend.set_pos(first_x, first_y);
        // we cut of the last byte of the disp_hex so that the space is not reverse video'd
        backend.append_text(&first_text, first_color, first_effect);
        if let Some((fx, fy)) = self.first_cursor_ascii() {
            backend.set_pos(fx, fy);
            backend.append_text(&disp_ascii(at_cursor.0), first_color, first_effect);
        }

        // right cursor
        let (second_x, second_y) = self.second_cursor();
        let second_effect = effect(active.is_second());
        let second_color = self.style.mode.color(at_cursor.1, at_cursor.0, second_y);
        let second_text = self.style.mode.disp(at_cursor.1, true);
        backend.set_pos(second_x, second_y);
        backend.append_text(&second_text, second_color, second_effect);
        if let Some((sx, sy)) = self.second_cursor_ascii() {
            backend.set_pos(sx, sy);
            backend.append_text(&disp_ascii(at_cursor.1), second_color, second_effect);
        }
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
        let short_first = shorten(first);
        let short_second = shorten(second);

        let addrsize = ADDR_SIZE - 1;
        let hexsize = self.cursor.get_size_x() * self.style.size_per_byte() - 1
            + if self.style.ascii_col {
                MIDDLE_PAD.width()
            } else {
                0
            };
        let first_title = format!(
            "{:addrsize$} {:>hexsize$} ",
            short_title,
            short_first,
            addrsize = addrsize,
            hexsize = hexsize
        );
        printer.set_line(0);
        printer.append_text(&first_title, Color::HexSame, Effect::Inverted);
        if self.style.vertical {
            printer.set_line(self.vert_half_height())
        } else {
            printer.append_text(MIDDLE_PAD, Color::HexSame, Effect::Inverted);
        }
        let second_title = format!(
            "{:addrsize$} {:>hexsize$} ",
            short_title,
            short_second,
            addrsize = addrsize,
            hexsize = hexsize
        );
        printer.append_text(&second_title, Color::HexSame, Effect::Inverted);
    }

    /// Prints the bottom text containing key information
    pub fn print_bottom_line<B: Backend>(&self, printer: &mut B) {
        const BOTTOM_TEXT: &str = "F1: Help     F2: Unalign   F3: Align    F4: Settings F6: Goto";
        let line = self.full_height() - 1;
        printer.set_line(line);
        printer.append_text(
            &format!("{:width$}", BOTTOM_TEXT, width = self.full_width()),
            Color::HexSame,
            Effect::Inverted,
        );
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

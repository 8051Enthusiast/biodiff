use std::iter::repeat;

use crate::{
    backend::{Backend, Color, Effect},
    cursor::{CursorActive, CursorState},
    style::{
        byte, byte_effect, disp_addr, disp_ascii, disp_bottom_addr, disp_column_blocks, ByteData,
        ColumnSetting, Style, FRONT_PAD, MIDDLE_PAD,
    },
    util::autocorrelation,
};
use unicode_width::UnicodeWidthStr;

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
            printer.append_text(
                &disp_addr(address, style.addr_width),
                Color::Unimportant,
                Effect::None,
            );
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
            printer.append_text(
                &disp_addr(address, style.addr_width),
                Color::Unimportant,
                Effect::None,
            );
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
            || scroll_amount.unsigned_abs() > content.len()
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
        let addr_print = disp_bottom_addr(cursor_addr, self.style.addr_width);
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
        let namewidth = self.hor_half_width().saturating_sub(title.width() + 2);
        // function for truncating the string on the left when it is too long
        // also inserts an < to indicate that it was truncated
        let shorten = |s: &str| -> String {
            if namewidth < 2 {
                String::new()
            } else if s.width() > namewidth {
                s.chars()
                    .rev()
                    .take(namewidth - 2)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .chain(std::iter::once('<'))
                    .rev()
                    .collect()
            } else {
                s.to_string()
            }
        };
        let short_first = shorten(first);
        let short_second = shorten(second);

        let format_title = |text| {
            if self.style.right_to_left {
                format!("{text:<namewidth$} {title} ")
            } else {
                format!("{title} {text:>namewidth$} ")
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
            "F1/1: Help   F2: Unalign  F3: Align    F4: Settings F6: Goto     F7: Search ";
        let print_addr = disp_bottom_addr(addresses, self.style.addr_width);
        let info_width = self.full_width().saturating_sub(print_addr.chars().count());
        let bottom_text = BOTTOM_TEXT.chars().take(info_width).collect::<String>();
        let info_text = if self.style.right_to_left {
            format!("{print_addr}{bottom_text:>info_width$}")
        } else {
            format!("{bottom_text:<info_width$}{print_addr}")
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

    /// decrease the amount of columns by one
    pub fn dec_columns(&mut self) {
        let default = self.cursor.bytes_per_row() as u16;
        self.style.column_count = ColumnSetting::Fixed(
            self.style
                .column_count
                .fixed()
                .unwrap_or(default)
                .saturating_sub(1)
                .max(1),
        );
    }
    /// increase the amount of columns by one
    pub fn inc_columns(&mut self) {
        let default = self.cursor.bytes_per_row() as u16;
        self.style.column_count = ColumnSetting::Fixed(
            self.style
                .column_count
                .fixed()
                .unwrap_or(default)
                .saturating_add(1),
        );
    }
    pub fn auto_columns(&mut self, bytes: [&[u8]; 2]) {
        const MIN_AUTOCOR_WIDTH: usize = 6;
        const MAX_AUTOCOR_WIDTH: usize = 65535;
        const AUTOCOR_THRESHOLD: f32 = 0.2;
        let [first, second] = bytes.map(autocorrelation);
        let max_len = first.len().max(second.len());
        let ratio = if second.len() > 0 {
            first.len() as f32 / second.len() as f32
        } else {
            1.0
        };
        let sum: Vec<f32> = first
            .iter()
            .chain(repeat(&0.0))
            .zip(second.iter().chain(repeat(&0.0)))
            .map(|(x, y)| ratio * x + (1.0 - ratio) * y)
            .take(max_len.min(MAX_AUTOCOR_WIDTH))
            .skip(MIN_AUTOCOR_WIDTH)
            .collect();
        let cmp = |x: &f32, y: &f32| {
            (!x.is_nan())
                .then_some(*x)
                .partial_cmp(&(!y.is_nan()).then_some(*y))
                .unwrap()
        };
        let max_index = sum
            .iter()
            .enumerate()
            .max_by(|(_, x), (_, y)| cmp(x, y))
            .filter(|(_, x)| **x > AUTOCOR_THRESHOLD)
            .map(|(i, _)| i + MIN_AUTOCOR_WIDTH);
        let max_index = if let Some(index) = max_index {
            index
        } else {
            return;
        };
        self.style.column_count = ColumnSetting::Multiple(max_index as u16);
    }
}

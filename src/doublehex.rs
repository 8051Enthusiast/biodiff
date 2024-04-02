use std::iter::repeat;

use crate::{
    backend::{Backend, BackgroundColor, Color, Effect},
    cursor::{CursorActive, CursorState},
    style::{
        background_color, byte, byte_effect, disp_addr, disp_ascii, disp_bottom_addr,
        disp_column_blocks, spacer_background_color, ByteData, ColumnSetting, Layout, Style,
        FRONT_PAD, MIDDLE_PAD,
    },
    util::autocorrelation,
};
use unicode_width::UnicodeWidthStr;

/// A line that can be printed using a backend for two hex views next to each other
#[derive(Debug, Clone)]
pub struct DoubleHexLine {
    pub address: [Option<usize>; 2],
    pub bytes: Vec<(ByteData, ByteData)>,
}

impl DoubleHexLine {
    fn print_unimportant<B: Backend>(&self, printer: &mut B, text: &str) {
        printer.append_text(
            text,
            Color::Unimportant,
            BackgroundColor::Blank,
            Effect::none(),
        );
    }
    /// Prints one side of the line
    fn print_half<B>(&self, printer: &mut B, line: usize, style: Style, first: bool)
    where
        B: Backend,
    {
        self.print_unimportant(printer, FRONT_PAD);
        let address = self.address[(!first) as usize];
        let mut bytes = vec![(ByteData::default(), ByteData::default()); self.bytes.len()];
        for (i, (a, b)) in self.bytes.iter().enumerate() {
            let target = if style.right_to_left {
                &mut bytes[self.bytes.len() - 1 - i]
            } else {
                &mut bytes[i]
            };
            *target = if first { (*a, *b) } else { (*b, *a) }
        }
        if !style.right_to_left {
            self.print_unimportant(printer, &disp_addr(address, style.addr_width));
        }
        let width = self.bytes.len();
        for (i, (a, b)) in bytes.iter().enumerate() {
            let s = style.mode.disp(byte(*a), false);
            let color = style.mode.color(*a, *b, line);
            let effect = byte_effect(*a);
            let bg = background_color(*a);
            printer.append_text(&s, color, bg, effect);
            if style.spacer && i + 1 != width && i % 8 == 7 {
                let spacer_bg = spacer_background_color(*a, style.right_to_left);
                printer.append_text(" ", color, spacer_bg, effect);
            }
        }
        if style.right_to_left {
            self.print_unimportant(printer, &disp_addr(address, style.addr_width));
        }
        for col_disp in [
            (style.ascii_col, disp_ascii as fn(_) -> _),
            (style.bars_col, disp_column_blocks),
        ]
        .iter()
        .filter(|&(c, _)| *c)
        .map(|(_, d)| d)
        {
            self.print_unimportant(printer, MIDDLE_PAD);
            for (a, b) in &bytes {
                let s = col_disp(byte(*a));
                let color = style.mode.color(*a, *b, line);
                let effect = byte_effect(*a);
                let bg = background_color(*a);
                printer.append_text(&s, color, bg, effect);
            }
        }
    }
    /// Prints the DoubleHexLine using the given backend at the line given in `line`
    /// with the views being on the left and right
    fn print_hor<B: Backend>(&self, printer: &mut B, line: usize, style: Style) {
        printer.set_line(line);
        self.print_half(printer, line, style, true);

        printer.append_text(
            MIDDLE_PAD,
            Color::Unimportant,
            BackgroundColor::Blank,
            Effect::none(),
        );
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
    pub cursor_act: CursorActive,
}

impl DoubleHexContext {
    /// Creates a new DoubleHexContext with default style
    pub fn new(size: (usize, usize)) -> Self {
        let cursor = CursorState::new(size);
        DoubleHexContext {
            cursor,
            style: Style::default(),
            cursor_act: CursorActive::Both,
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
        match self.style.layout {
            Layout::Vertical => self.hor_half_width(),
            Layout::Horizontal => 2 * self.hor_half_width() + MIDDLE_PAD.width(),
        }
    }
    fn full_height(&self) -> usize {
        match self.style.layout {
            Layout::Vertical => self.cursor.get_size_y() * 2 + 3,
            Layout::Horizontal => self.cursor.get_size_y() + 2,
        }
    }
    /// Prints a whole screen of hex data
    pub fn print_doublehex_screen<B: Backend>(&self, content: &[DoubleHexLine], backend: &mut B) {
        for (i, line) in content.iter().enumerate() {
            match self.style.layout {
                Layout::Vertical => line.print_vert(
                    backend,
                    [i + 1, self.vert_half_height() + i + 1],
                    self.style,
                ),
                // we offset because of the title bar
                Layout::Horizontal => line.print_hor(backend, i + 1, self.style),
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
        match self.style.layout {
            Layout::Vertical => (pos.0, pos.1 + self.vert_half_height()),
            Layout::Horizontal => (pos.0 + self.hor_half_width() + MIDDLE_PAD.width(), pos.1),
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
        print_bars: impl FnOnce(&mut B),
    ) {
        let rows = self.cursor.get_size_y();
        if scroll_amount == 0 {
            return;
        }
        if !backend.can_scroll()
            || !self.style.mode.can_scroll()
            || scroll_amount.unsigned_abs() > content.len()
            || self.style.no_scroll
        {
            return self.print_doublehex_screen(content, backend);
        }
        backend.scroll(scroll_amount);
        // we print bars before doing anything else to reduce flickering
        if scroll_amount != 0 {
            print_bars(backend);
        }
        for line in (rows - scroll_amount.unsigned_abs())..rows {
            // note that the lines where the previous bars were scrolled to
            // are overwritten in the first iteration of this loop to further
            // reduce flickering
            let line = if scroll_amount > 0 {
                line
            } else {
                rows - line - 1
            };
            match self.style.layout {
                Layout::Vertical => content[line].print_vert(
                    backend,
                    [line + 1, self.vert_half_height() + line + 1],
                    self.style,
                ),
                Layout::Horizontal => content[line].print_hor(backend, line + 1, self.style),
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
        at_cursor: (ByteData, ByteData),
        cursor_addr: [Option<usize>; 2],
    ) {
        // the cursor is displayed with reverse video
        let effect = |is_active, byte: ByteData| {
            if is_active {
                Effect {
                    inverted: is_active,
                    bold: byte.is_search_result,
                }
            } else {
                byte_effect(byte)
            }
        };

        // first cursor
        let (first_x, first_y) = self.first_cursor();
        let first_effect = effect(active.is_first(), at_cursor.0);
        let first_color = self.style.mode.color(at_cursor.0, at_cursor.1, first_y);
        let first_bg = background_color(at_cursor.0);
        let first_text = self.style.mode.disp(byte(at_cursor.0), true);
        // note again that the title bar is skipped
        backend.set_pos(first_x, first_y);
        // we cut of the last byte of the disp_hex so that the space is not reverse video'd
        backend.append_text(&first_text, first_color, first_bg, first_effect);
        // first ascii and bars column
        for (fx, fy, disp_col) in [
            (self.first_cursor_ascii(), disp_ascii as fn(_) -> _),
            (self.first_cursor_bars(), disp_column_blocks),
        ]
        .iter()
        .filter_map(|(a, b)| a.map(|(a0, a1)| (a0, a1, b)))
        {
            backend.set_pos(fx, fy);
            backend.append_text(
                &disp_col(byte(at_cursor.0)),
                first_color,
                first_bg,
                first_effect,
            );
        }

        // second cursor
        let (second_x, second_y) = self.second_cursor();
        let second_effect = effect(active.is_second(), at_cursor.1);
        let second_color = self.style.mode.color(at_cursor.1, at_cursor.0, second_y);
        let second_bg = background_color(at_cursor.1);
        let second_text = self.style.mode.disp(byte(at_cursor.1), true);
        backend.set_pos(second_x, second_y);
        backend.append_text(&second_text, second_color, second_bg, second_effect);
        // second ascii and bars column
        for (sx, sy, disp_col) in [
            (self.second_cursor_ascii(), disp_ascii as fn(_) -> _),
            (self.second_cursor_bars(), disp_column_blocks),
        ]
        .iter()
        .filter_map(|(a, b)| a.map(|(a0, a1)| (a0, a1, b)))
        {
            backend.set_pos(sx, sy);
            backend.append_text(
                &disp_col(byte(at_cursor.1)),
                second_color,
                second_bg,
                second_effect,
            );
        }

        // status bar address
        let addr_print = disp_bottom_addr(cursor_addr, self.style.addr_width);
        let addr_print = &addr_print[..addr_print.len().min(self.full_width())];
        if self.style.right_to_left {
            backend.set_pos(0, self.full_height() - 1);
        } else {
            backend.set_pos(
                self.full_width().saturating_sub(addr_print.len()),
                self.full_height() - 1,
            );
        }
        backend.append_text(
            addr_print,
            Color::HexSame,
            BackgroundColor::Blank,
            Effect::inverted(),
        );
    }

    /// prints the line at the top containing the filenames and status
    pub fn print_title_line<B: Backend>(
        &self,
        printer: &mut B,
        title: &str,
        first: &str,
        second: &str,
    ) {
        let title = &title[..title.len().min(self.hor_half_width() - 2)];
        let namewidth = self.hor_half_width().saturating_sub(title.len() + 2);
        // title is all ascii so just count bytes
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
        printer.append_text(
            &first_title,
            Color::HexSame,
            BackgroundColor::Blank,
            Effect::inverted(),
        );
        match self.style.layout {
            Layout::Vertical => printer.set_line(self.vert_half_height()),
            Layout::Horizontal => printer.append_text(
                MIDDLE_PAD,
                Color::HexSame,
                BackgroundColor::Blank,
                Effect::inverted(),
            ),
        }
        let second_title = format_title(short_second);
        printer.append_text(
            &second_title,
            Color::HexSame,
            BackgroundColor::Blank,
            Effect::inverted(),
        );
    }

    /// Prints the bottom text containing key information
    pub fn print_bottom_line<B: Backend>(&self, printer: &mut B, addresses: [Option<usize>; 2]) {
        const BOTTOM_TEXT: &str =
            "F1/1: Help F2: Unalign F3: Align F4: Settings F6: Goto F7: Search";
        let print_addr = disp_bottom_addr(addresses, self.style.addr_width);
        let print_addr = &print_addr[..print_addr.len().min(self.full_width())];
        let info_width = self.full_width().saturating_sub(print_addr.len());
        let bottom_text = &BOTTOM_TEXT[..BOTTOM_TEXT.len().min(info_width)];
        let info_text = if self.style.right_to_left {
            format!("{print_addr}{bottom_text:>info_width$}")
        } else {
            format!("{bottom_text:<info_width$}{print_addr}")
        };
        let line = self.full_height() - 1;
        printer.set_line(line);
        printer.append_text(
            &info_text,
            Color::HexSame,
            BackgroundColor::Blank,
            Effect::inverted(),
        );
        for line in self.full_height()..printer.size().1 {
            printer.set_line(line);
            printer.append_text(
                &format!("{:width$}", " ", width = self.full_width()),
                Color::HexSame,
                BackgroundColor::Blank,
                Effect::none(),
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
    /// looks at where the autocorrelation peaks are and sets the column count
    pub fn auto_columns(&mut self, bytes: [&[u8]; 2]) {
        const MIN_AUTOCOR_WIDTH: usize = 6;
        const MAX_AUTOCOR_WIDTH: usize = 65535;
        const AUTOCOR_THRESHOLD: f64 = 0.2;
        let [first, second] = bytes.map(autocorrelation);
        let max_len = first.len().max(second.len());
        let ratio = if !second.is_empty() {
            first.len() as f64 / second.len() as f64
        } else {
            1.0
        };
        let sum: Vec<f64> = first
            .iter()
            .chain(repeat(&0.0))
            .zip(second.iter().chain(repeat(&0.0)))
            .map(|(x, y)| ratio * x + (1.0 - ratio) * y)
            .take(max_len.min(MAX_AUTOCOR_WIDTH))
            .skip(MIN_AUTOCOR_WIDTH)
            .collect();
        let cmp = |x: &f64, y: &f64| {
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

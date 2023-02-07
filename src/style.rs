use std::str::FromStr;

use serde::{Deserialize, Serialize};
use unicode_width::UnicodeWidthStr;

use crate::{
    backend::{BackgroundColor, Color, Effect},
    selection::SelectionStatus,
};
pub const FRONT_PAD: &str = " ";
pub const MIDDLE_PAD: &str = " |";
pub const SPACER_PERIOD: usize = 8;

#[derive(Debug, Clone, Copy, Default)]
pub struct ByteData {
    pub byte: Option<u8>,
    pub is_search_result: bool,
    pub is_selected: SelectionStatus,
}

impl ByteData {
    pub fn new(byte: Option<u8>, is_search_result: bool, is_selected: SelectionStatus) -> Self {
        ByteData {
            byte,
            is_search_result,
            is_selected,
        }
    }
}

pub fn byte(data: ByteData) -> Option<u8> {
    data.byte
}

/// Contains `digits` digits of an address, with a space at the end
pub fn disp_addr(maddr: Option<usize>, digits: u8) -> String {
    match maddr {
        Some(addr) => {
            format!("{addr:0width$x} ", width = digits as usize)
        }
        None => {
            format!("{:width$} ", " ", width = digits as usize)
        }
    }
}

/// Formats the addresses that get displayed on the lower right of the screen
pub fn disp_bottom_addr(addresses: [Option<usize>; 2], digits: u8) -> String {
    let diff = if let [Some(a), Some(b)] = addresses {
        let d = (b as isize).wrapping_sub(a as isize);
        if d < 0 {
            format!("(-{:0digits$x})", -d, digits = digits as usize)
        } else {
            format!("(+{:0digits$x})", d, digits = digits as usize)
        }
    } else {
        format!("  {:digits$} ", " ", digits = digits as usize)
    };
    let addr = |a| {
        if let Some(x) = a {
            format!("{:0digits$x}", x, digits = digits as usize)
        } else {
            format!("{:digits$}", " ", digits = digits as usize)
        }
    };
    format!(" {}|{}{diff}", addr(addresses[0]), addr(addresses[1]))
}

/// Contains two hex digits of a byte and a space behind it, or just three spaces for None
fn disp_hex(h: Option<u8>) -> String {
    match h {
        Some(hex) => format!("{hex:02x} "),
        None => String::from("   "),
    }
}

fn disp_binary(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{bin:08b} "),
        None => String::from("         "),
    }
}

fn disp_octal(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{bin:03o} "),
        None => String::from("    "),
    }
}

fn disp_decimal(h: Option<u8>) -> String {
    match h {
        Some(bin) => format!("{bin:>3} "),
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
            format!("│{braille_char}")
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
        Some(c) => format!("{c:02x} "),
        None => String::from("   "),
    }
}

pub fn disp_ascii(h: Option<u8>) -> String {
    match h {
        Some(c @ b' '..=b'~') => c as char,
        Some(_) => '.',
        None => ' ',
    }
    .into()
}

pub fn disp_column_blocks(h: Option<u8>) -> String {
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
    format!("{s:>9} ")
}

pub fn byte_effect(x: ByteData) -> Effect {
    Effect {
        inverted: false,
        bold: x.is_search_result,
    }
}

pub fn background_color(x: ByteData) -> BackgroundColor {
    if x.is_selected.is_active() {
        BackgroundColor::Highlight
    } else {
        BackgroundColor::Blank
    }
}

pub fn spacer_background_color(x: ByteData, rtl: bool) -> BackgroundColor {
    if x.is_selected.continues_on_right(rtl) {
        BackgroundColor::Highlight
    } else {
        BackgroundColor::Blank
    }
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_bytes(a: ByteData, b: ByteData) -> Color {
    match (a.byte, b.byte) {
        (Some(a), Some(b)) if a == b => Color::HexSame,
        (Some(_), Some(_)) => Color::HexDiff,
        (None, _) | (_, None) => Color::HexOneside,
    }
}

/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_secondary_from_bytes(a: ByteData, b: ByteData) -> Color {
    match (a.byte, b.byte) {
        (Some(a), Some(b)) if a == b => Color::HexSameSecondary,
        (Some(_), Some(_)) => Color::HexDiffSecondary,
        (None, _) | (_, None) => Color::HexOnesideSecondary,
    }
}
/// Insertions/Deletions are typically green, mismatches red and same bytes white
fn color_from_mixed_bytes(a: ByteData, b: ByteData) -> Color {
    if matches!(byte(a), Some(b'!'..=b'~' | b' ' | b'\n' | b'\t' | b'\r')) {
        color_from_bytes(a, b)
    } else {
        color_secondary_from_bytes(a, b)
    }
}

#[repr(usize)]
#[derive(Clone, Copy, Debug, Serialize, Deserialize, Default)]
pub enum DisplayMode {
    #[default]
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
    pub fn color(&self, a: ByteData, b: ByteData, row: usize) -> Color {
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
    pub fn disp(&self, a: Option<u8>, short: bool) -> String {
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
    pub fn can_scroll(&self) -> bool {
        !matches!(self, Self::Braille)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Default)]
pub enum ColumnSetting {
    #[default]
    Fit,
    Fixed(u16),
    Multiple(u16),
}

impl ColumnSetting {
    pub fn fixed(self) -> Option<u16> {
        match self {
            Self::Fixed(n) => Some(n),
            _ => None,
        }
    }
}

impl FromStr for ColumnSetting {
    type Err = std::num::ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Self::Fit);
        }
        if let Some(rest) = s.strip_suffix('x') {
            Ok(Self::Multiple(rest.parse()?))
        } else {
            Ok(Self::Fixed(s.parse()?))
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
    pub column_count: ColumnSetting,
    pub no_scroll: bool,
    #[serde(skip)]
    pub addr_width: u8,
}

impl Style {
    fn size_per_byte(&self) -> usize {
        self.mode.size_per_byte() + self.ascii_col as usize + self.bars_col as usize
    }
    pub fn addr_size(&self) -> usize {
        self.addr_width as usize + 1
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
            Some(self.addr_size() + FRONT_PAD.width() + self.n_column_width(n) + MIDDLE_PAD.width())
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
                self.addr_size()
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
        self.addr_size()
            + FRONT_PAD.width()
            + self.ascii_width(n)
            + self.bars_width(n)
            + self.n_column_width(n)
    }
    /// the position of the first character of the nth hex column
    pub fn nth_column_pos(&self, n: usize) -> usize {
        self.mode.size_per_byte() * n
            + if self.spacer { n / SPACER_PERIOD } else { 0 }
            + if self.right_to_left {
                0
            } else {
                self.addr_size()
            }
            + FRONT_PAD.width()
    }
    /// the amount of characters that are on a line regardless of column number
    fn const_overhead(&self) -> usize {
        let single_overhead = self.addr_size()
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
    /// in case the column_count is not set, otherwise it uses the column_count
    pub fn get_doublehex_dims(&self, columns: usize, rows: usize) -> ((usize, usize), usize) {
        let y = if self.vertical {
            rows.saturating_sub(3) / 2
        } else {
            rows.saturating_sub(2)
        };
        let max_col = if columns <= self.const_overhead() {
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
            without_spacer / unit_width
        };
        let x = match self.column_count {
            ColumnSetting::Fit => {
                if max_col < 8 {
                    max_col
                } else if max_col < 24 {
                    max_col / 4 * 4
                } else {
                    max_col / 8 * 8
                }
            }
            ColumnSetting::Fixed(n) => max_col.min(n as usize),
            ColumnSetting::Multiple(n) => {
                if max_col < n as usize {
                    max_col
                } else {
                    max_col / n as usize * n as usize
                }
            }
        };
        let bytes_per_row = match self.column_count {
            ColumnSetting::Fit => x,
            ColumnSetting::Fixed(n) => n as usize,
            ColumnSetting::Multiple(n) => (x / n as usize * n as usize).max(n as usize),
        };
        ((x, y), bytes_per_row)
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
            no_scroll: false,
            column_count: ColumnSetting::Fit,
            addr_width: 0,
        }
    }
}

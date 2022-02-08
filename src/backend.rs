use crossterm::{
    cursor,
    event::{read, Event, KeyCode},
    style,
    style::Attribute,
    style::Color as CrossColor,
    terminal,
};
use crossterm::{execute, queue};
use cursive::{theme, Printer};
use std::{
    convert::{TryFrom, TryInto},
    io::{Cursor, Write},
};
use std::{io::Stdout, sync::mpsc::Sender};
use unicode_width::UnicodeWidthStr;

/// A wrapper for events coming from crossterm
#[derive(Clone, Copy, Debug)]
pub enum Action {
    Up,
    Down,
    Left,
    Right,
    UpAlt,
    DownAlt,
    LeftAlt,
    RightAlt,
    PgUp,
    PgDown,
    NextDifference,
    Quit,
    Unalign,
    Align,
    Help,
    Algorithm,
    Refresh,
    Goto,
    Search,
    SetOffset,
    NextSearch,
    PrevSearch,
    Top,
    Bottom,
    CursorFirst,
    CursorBoth,
    CursorSecond,
}

impl TryFrom<Event> for Action {
    // unknown event for now unit
    type Error = ();

    fn try_from(value: Event) -> Result<Self, Self::Error> {
        Ok(
            match match value {
                Event::Resize(_, _) => return Ok(Action::Refresh),
                Event::Mouse(_) => return Err(()),
                // ignore modifiers for now
                Event::Key(x) => x.code,
            } {
                KeyCode::Up => Action::Up,
                KeyCode::Down => Action::Down,
                KeyCode::Left => Action::Left,
                KeyCode::Right => Action::Right,
                KeyCode::PageDown => Action::PgDown,
                KeyCode::PageUp => Action::PgUp,
                KeyCode::Char('q') => Action::Quit,
                KeyCode::Esc => Action::Quit,
                KeyCode::Char('?') => Action::Help,
                KeyCode::Char('r') => Action::Refresh,
                KeyCode::Char('a') => Action::CursorFirst,
                KeyCode::Char('s') => Action::CursorBoth,
                KeyCode::Char('d') => Action::CursorSecond,
                KeyCode::Char('h') => Action::LeftAlt,
                KeyCode::Char('j') => Action::DownAlt,
                KeyCode::Char('k') => Action::UpAlt,
                KeyCode::Char('l') => Action::RightAlt,
                KeyCode::Char('n') => Action::NextSearch,
                KeyCode::Char('N') => Action::PrevSearch,
                KeyCode::Char('o') => Action::SetOffset,
                KeyCode::Char(' ') => Action::NextDifference,
                KeyCode::F(1) => Action::Help,
                KeyCode::Char('1') => Action::Help,
                KeyCode::F(2) => Action::Unalign,
                KeyCode::Char('2') => Action::Unalign,
                KeyCode::F(3) => Action::Align,
                KeyCode::Char('3') => Action::Align,
                KeyCode::F(4) => Action::Algorithm,
                KeyCode::Char('4') => Action::Algorithm,
                KeyCode::F(5) => Action::Refresh,
                KeyCode::Char('5') => Action::Refresh,
                KeyCode::F(6) => Action::Goto,
                KeyCode::Char('6') => Action::Goto,
                KeyCode::F(7) => Action::Search,
                KeyCode::Char('7') => Action::Search,
                KeyCode::Home => Action::Top,
                KeyCode::End => Action::Bottom,
                _ => return Err(()),
            },
        )
    }
}

/// Reads crossterm events and sends them into a sender that understands them
pub fn send_cross_actions<F, A: From<Action>>(quit_predicate: F, sender: &mut Sender<A>)
where
    F: Fn(Action) -> bool,
{
    loop {
        match read()
            .unwrap_or_else(quit_with_error("Could not get key event"))
            .try_into()
            .map(|action| sender.send(A::from(action)).map(|()| action))
        {
            Ok(Ok(action)) => {
                if quit_predicate(action) {
                    return;
                }
            }
            // quit when other end has disconnected
            Ok(Err(_)) => return,
            // drop unknown event
            Err(()) => (),
        }
    }
}

pub trait Backend {
    /// moves to start of given line and initializes background color
    fn set_line(&mut self, line: usize);
    /// moves to a position
    fn set_pos(&mut self, column: usize, line: usize);
    /// appends text with given text and color to current line
    fn append_text(&mut self, text: &str, color: Color, effect: Effect);
    /// returns wether the terminal has the ability to scroll
    fn can_scroll(&self) -> bool;
    /// scrolls amount (positive moves content of terminal up)
    fn scroll(&mut self, amount: isize);
    /// refreshes the screen after content has been queued
    fn refresh(&mut self);
    /// gets the dimensions of the display
    fn size(&mut self) -> (usize, usize);
    /// clears display
    fn clear(&mut self);
}

/// Wrapper for crossterm and cursive colors
#[derive(Clone, Copy, Debug)]
pub enum Color {
    Unimportant,
    HexSame,
    HexSameSecondary,
    HexDiff,
    HexDiffSecondary,
    HexOneside,
    HexOnesideSecondary,
}

impl Color {
    /// Converts to a crossterm color
    fn to_cross(self) -> CrossColor {
        match self {
            Color::Unimportant => CrossColor::DarkGrey,
            Color::HexSame => CrossColor::White,
            Color::HexDiff => CrossColor::Red,
            Color::HexOneside => CrossColor::Green,
            Color::HexSameSecondary => CrossColor::Yellow,
            Color::HexDiffSecondary => CrossColor::DarkRed,
            Color::HexOnesideSecondary => CrossColor::DarkGreen,
        }
    }
    /// Converts to a cursive color (with black background)
    fn to_cursiv(self) -> theme::ColorStyle {
        let col = match self {
            Color::Unimportant => theme::Color::Light(theme::BaseColor::Black),
            Color::HexSame => theme::Color::Light(theme::BaseColor::White),
            Color::HexDiff => theme::Color::Light(theme::BaseColor::Red),
            Color::HexOneside => theme::Color::Light(theme::BaseColor::Green),
            Color::HexSameSecondary => theme::Color::Light(theme::BaseColor::Yellow),
            Color::HexDiffSecondary => theme::Color::Dark(theme::BaseColor::Red),
            Color::HexOnesideSecondary => theme::Color::Dark(theme::BaseColor::Green),
        };
        theme::ColorStyle::new(col, theme::Color::Dark(theme::BaseColor::Black))
    }
}

/// An effect, for now either reverse video or normal
#[derive(Clone, Copy, Debug)]
pub enum Effect {
    Inverted,
    Bold,
    None,
}

impl Effect {
    fn to_cross(self) -> style::Attribute {
        match self {
            // this assumes that no other effects are in effect
            // so change this when adding others
            // note that Reset also resets colors
            Effect::None => Attribute::Reset,
            Effect::Inverted => Attribute::Reverse,
            Effect::Bold => Attribute::Bold,
        }
    }
    fn to_cursiv(self) -> theme::Effect {
        match self {
            Effect::None => theme::Effect::Simple,
            Effect::Inverted => theme::Effect::Reverse,
            Effect::Bold => theme::Effect::Bold,
        }
    }
}

#[derive(Debug)]
pub struct Cross {
    stdout: Stdout,
    buffer: Cursor<Vec<u8>>,
    prev_color: Option<CrossColor>,
    prev_effect: Option<Attribute>,
}

impl Cross {
    /// Private API for creating a new object and not yet initializing the terminal
    fn new_uninit() -> Self {
        Cross {
            stdout: std::io::stdout(),
            buffer: Cursor::new(Vec::new()),
            prev_color: None,
            prev_effect: None,
        }
    }
    /// init the crossterm backend, places the screen into raw mode and the alternative buffer
    /// and hides the cursor etc.
    pub fn init() -> Self {
        let mut ret = Self::new_uninit();
        execute!(ret.stdout, terminal::EnterAlternateScreen,)
            .unwrap_or_else(quit_with_error("Could not get terminal size"));
        terminal::enable_raw_mode().unwrap_or_else(quit_with_error("Could not enable raw mode"));
        execute!(
            ret.stdout,
            style::ResetColor,
            style::SetAttribute(style::Attribute::Reset),
            style::SetBackgroundColor(CrossColor::Black),
            terminal::Clear(terminal::ClearType::All),
            terminal::DisableLineWrap,
            cursor::MoveTo(0, 0),
            cursor::Hide,
        )
        .unwrap_or_else(quit_with_error("Could not initialize crossterm"));
        ret
    }
    /// uninitializes everything we initialized and goes back to the normal screen
    pub fn uninit(mut self) {
        let _ = execute!(
            self.stdout,
            style::ResetColor,
            terminal::EnableLineWrap,
            cursor::Show,
            terminal::LeaveAlternateScreen,
        );
        let _ = terminal::disable_raw_mode();
    }
}

/// Convenience function for quitting and uninitializing the terminal before it
pub fn quit_with_error<E: std::error::Error, Out>(premsg: &'static str) -> impl Fn(E) -> Out {
    move |err| {
        let tmp = Cross::new_uninit();
        tmp.uninit();
        eprintln!("{}: {}", premsg, err);
        std::process::exit(1)
    }
}

impl Backend for Cross {
    fn set_line(&mut self, line: usize) {
        queue!(
            self.buffer,
            cursor::MoveTo(
                0,
                u16::try_from(line).unwrap_or_else(quit_with_error("line out of range"))
            ),
            // i haven't check whether this is actually needed
            style::SetBackgroundColor(CrossColor::Black),
        )
        .unwrap_or_else(quit_with_error("Could not move cursor"));
    }

    fn set_pos(&mut self, column: usize, line: usize) {
        queue!(
            self.buffer,
            cursor::MoveTo(
                u16::try_from(column).unwrap_or_else(quit_with_error("column out of range")),
                u16::try_from(line).unwrap_or_else(quit_with_error("line out of range"))
            ),
        )
        .unwrap_or_else(quit_with_error("Could not move cursor"));
    }

    fn append_text(&mut self, text: &str, color: Color, effect: Effect) {
        let attribute = effect.to_cross();
        // try to optimize by not printing the color if it hasn't changed
        if Some(attribute) != self.prev_effect {
            queue!(
                self.buffer,
                style::SetAttribute(Attribute::Reset),
                style::SetAttribute(attribute),
                style::SetBackgroundColor(CrossColor::Black)
            )
            .unwrap_or_else(quit_with_error("Could not write out text"));
            self.prev_effect = Some(attribute);
            // because the attribute is Reset, then we also need to set the color again
            self.prev_color = None;
        }
        let cross_color = color.to_cross();
        if Some(cross_color) != self.prev_color {
            queue!(self.buffer, style::SetForegroundColor(cross_color),)
                .unwrap_or_else(quit_with_error("Could not write out text"));
            self.prev_color = Some(cross_color);
        }
        queue!(self.buffer, style::Print(text))
            .unwrap_or_else(quit_with_error("Could not write out text"));
    }

    fn can_scroll(&self) -> bool {
        // this doesn't work on linux's native terminal and i would like to know
        // how to feature detect this (also, i'm pretty sure there are some other
        // scroll sequences that work there?) but for now just pretend it works
        true
    }

    fn scroll(&mut self, amount: isize) {
        match amount {
            isize::MIN..=-1 => {
                queue!(
                    self.buffer,
                    terminal::ScrollDown(
                        u16::try_from(-amount)
                            .unwrap_or_else(quit_with_error("scroll out of range"))
                    )
                )
                .unwrap_or_else(quit_with_error("Could not scroll"));
            }
            1..=isize::MAX => {
                queue!(
                    self.buffer,
                    terminal::ScrollUp(
                        u16::try_from(amount)
                            .unwrap_or_else(quit_with_error("scroll out of range"))
                    )
                )
                .unwrap_or_else(quit_with_error("Could not scroll"));
            }
            _ => (),
        }
    }

    fn refresh(&mut self) {
        let _ = self.buffer.flush();
        let mut buffer = Cursor::new(Vec::new());
        std::mem::swap(&mut buffer, &mut self.buffer);
        self.stdout
            .write_all(&buffer.into_inner())
            .unwrap_or_else(quit_with_error("Could not write to stdout"));
        let _ = self.stdout.flush();
    }

    fn size(&mut self) -> (usize, usize) {
        let (a, b) =
            terminal::size().unwrap_or_else(quit_with_error("Could not get terminal size"));
        (usize::from(a), usize::from(b))
    }

    fn clear(&mut self) {
        self.prev_effect = Some(Attribute::NoReverse);
        queue!(
            self.buffer,
            style::SetAttribute(Attribute::NoReverse),
            terminal::Clear(terminal::ClearType::All),
        )
        .unwrap_or_else(quit_with_error("Could not clear screen"))
    }
}

/// Painter for the cursive backend, keeps track of the position so that
/// append_text works.
pub struct Cursiv<'a, 'b, 'c> {
    current_pos: (usize, usize),
    printer: &'c Printer<'a, 'b>,
}

impl<'a, 'b, 'c> Cursiv<'a, 'b, 'c> {
    pub fn from_printer(printer: &'c Printer<'a, 'b>) -> Self {
        Cursiv {
            current_pos: (0, 0),
            printer,
        }
    }
}

impl<'a, 'b, 'c> Backend for Cursiv<'a, 'b, 'c> {
    fn set_line(&mut self, line: usize) {
        self.current_pos = (0, line)
    }

    fn set_pos(&mut self, column: usize, line: usize) {
        self.current_pos = (column, line)
    }

    fn append_text(&mut self, text: &str, color: Color, effect: Effect) {
        let len = text.width();
        let style = theme::Style::none()
            .combine(color.to_cursiv())
            .combine(effect.to_cursiv());
        self.printer
            .with_style(style, |p| p.print(self.current_pos, text));
        self.current_pos.0 += len;
    }

    // i don't think cursive has a way to natively scroll?
    fn can_scroll(&self) -> bool {
        false
    }

    fn scroll(&mut self, _amount: isize) {}

    // this is not necessary since cursive itself does the refreshing
    fn refresh(&mut self) {}

    fn size(&mut self) -> (usize, usize) {
        (self.printer.size.x, self.printer.size.y)
    }

    fn clear(&mut self) {
        self.printer.clear()
    }
}

pub struct Dummy;

impl Backend for Dummy {
    fn set_line(&mut self, _: usize) {}

    fn set_pos(&mut self, _: usize, _: usize) {}

    fn append_text(&mut self, _: &str, _: Color, _: Effect) {}

    fn can_scroll(&self) -> bool {
        false
    }

    fn scroll(&mut self, _: isize) {}

    fn refresh(&mut self) {}

    fn size(&mut self) -> (usize, usize) {
        (0, 0)
    }

    fn clear(&mut self) {}
}

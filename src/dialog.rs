mod algorithm_presets;
mod algorithm_settings;
mod goto;
mod search;
mod set_offset;
mod settings;
mod style_settings;

use crate::{
    align::{FlatAlignProgressMessage, FlatAlignmentContext},
    backend::Dummy,
    config::{Config, ConfigV1},
    control::{CursiveCallback, DelegateEvent, WrappedEvent},
    file::FileContent,
    search::{Query, QueryType, SearchContext},
    style::{ColumnSetting, DisplayMode, Style},
    util::{self, Finalable},
    view::{Aligned, Unaligned},
};

use biodiff_align::{AlignAlgorithm, AlignMode, Banded, DEFAULT_BLOCKSIZE};
use cursive::{
    event::Key,
    theme::{PaletteColor, StyleType},
    traits::*,
    utils::Counter,
    view::ViewWrapper,
    views::*,
    wrap_impl, CbSink, Cursive, View,
};
use std::{
    fmt::Display,
    num::ParseIntError,
    ops::Range,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicUsize},
        Arc,
    },
    time::Duration,
};

pub use algorithm_presets::presets;
pub use algorithm_settings::algorithm;
pub use goto::goto;
pub use search::search;
pub use set_offset::set_offset;
pub use settings::settings;
pub use style_settings::style;

/// A box that changes color when the content is invalid
fn validated_box<F: Fn(&str) -> bool + 'static>(
    name: &'static str,
    initial_value: String,
    width: usize,
    validator: F,
) -> ResizedView<NamedView<EditView>> {
    EditView::new()
        .content(initial_value)
        .on_edit_mut(move |siv, s, _| {
            match validator(s) {
                true => siv.call_on_name(name, |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Secondary))
                }),
                false => siv.call_on_name(name, |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Highlight))
                }),
            };
        })
        .with_name(name)
        .fixed_width(width)
}

/// parses the content of a EditView with a given name, appending errors into err on failure
/// and writing into val on success
fn parse_box<S: FromStr>(siv: &mut Cursive, name: &'static str, val: &mut S, err: &mut String)
where
    S::Err: Display,
{
    match siv
        .call_on_name(name, |v: &mut EditView| v.get_content().parse())
        .expect("Could not find textbox")
    {
        Ok(x) => *val = x,
        Err(e) => err.push_str(&format!("{name} is invalid: {e}\n")),
    }
}

/// Executes function either on aligned or unaligned hexview in current cursive
/// context
fn on_hexview<F, G, T>(siv: &mut Cursive, aligned: F, unaligned: G) -> T
where
    F: FnOnce(&mut Aligned) -> T,
    G: FnOnce(&mut Unaligned) -> T,
{
    siv.call_on_name("aligned", aligned)
        .or_else(|| siv.call_on_name("unaligned", unaligned))
        .expect("Could not find aligned or unaligned view in cursive stack")
}

/// We only want to quit cursive and return to our crossterm native implementation
/// when no other windows are open. This function wraps that behaviour.
pub fn close_top_maybe_quit(siv: &mut Cursive) {
    if siv.screen().len() <= 1 {
        siv.quit();
        return;
    }
    siv.pop_layer();
    if siv.screen().len() <= 1 {
        siv.quit()
    }
}

/// A help window that displays a fixed text.
pub fn help_window(help_text: &'static str) -> impl Fn(&mut Cursive) {
    move |siv| {
        siv.add_layer(
            Dialog::around(ScrollView::new(TextView::new(help_text)))
                .title("Help")
                .button("Close", close_top_maybe_quit),
        )
    }
}

/// An error window that displays a fixed text.
pub fn error_window(error_text: String) -> impl Fn(&mut Cursive) {
    move |siv| {
        siv.add_layer(
            Dialog::around(TextView::new(&error_text))
                .title("Error")
                .button("Close", close_top_maybe_quit),
        )
    }
}

pub fn memory_warning(siv: &mut Cursive, event: WrappedEvent) {
    let event_cp = event.clone();
    let content = LinearLayout::vertical()
        .child(TextView::new(
            "Alignments of these sizes may cause the program to run out of memory. \
            Continue anyway?",
        ))
        .child(
            LinearLayout::horizontal()
                .child(Checkbox::new().with_name("no_memory_warn"))
                .child(TextView::new(" Don't show this warning again")),
        );
    let cont = move |siv: &mut Cursive| {
        event_cp.set(DelegateEvent::SwitchToAlign);
        if siv
            .call_on_name("no_memory_warn", |v: &mut Checkbox| v.is_checked())
            .expect("Could not find checkbox")
        {
            let config = siv.user_data::<ConfigV1>().expect("Could not find config");
            config.set_no_memory_warn();
        }
        close_top_maybe_quit(siv)
    };
    let mut dialog = Dialog::around(content)
        .title("Memory Warning")
        .button("Continue", cont)
        .button("Cancel", move |siv| {
            event.set(DelegateEvent::Continue);
            close_top_maybe_quit(siv)
        });
    dialog.set_focus(DialogFocus::Button(0));
    siv.add_layer(dialog);
}

/// Wraps the callback in a box that also returns DelegateEvent::Continue
pub fn continue_dialog(dialog: impl Fn(&mut Cursive) + 'static + Send) -> CursiveCallback {
    Box::new(move |siv, ev| {
        dialog(siv);
        ev.set(DelegateEvent::Continue)
    })
}

pub const MAIN_HELP: &str = include_str!("help/main.txt");
pub const ALGORITHM_HELP: &str = include_str!("help/algorithm.txt");
pub const PRESET_HELP: &str = include_str!("help/preset.txt");
pub const STYLE_HELP: &str = include_str!("help/style.txt");
pub const GOTO_HELP: &str = include_str!("help/goto.txt");
pub const SEARCH_HELP: &str = include_str!("help/search.txt");
pub const SET_OFFSET_HELP: &str = include_str!("help/set_offset.txt");

use crate::{
    align::{AlignAlgorithm, AlignMode, Banded, DEFAULT_BLOCKSIZE, DEFAULT_KMER, DEFAULT_WINDOW},
    control::Settings,
    drawer::{DisplayMode, Style},
    view::{Aligned, Unaligned},
};
use cursive::{event::Key, theme::PaletteColor, traits::*, views::*, Cursive, View};
use std::{fmt::Display, str::FromStr};
const TEXT_WIDTH: usize = 6;

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
                    v.set_style(PaletteColor::Secondary.into())
                }),
                false => siv.call_on_name(name, |v: &mut EditView| {
                    v.set_style(PaletteColor::Highlight.into())
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
        Err(e) => err.push_str(&format!("{} is invalid: {}\n", name, e)),
    }
}

/// Reads the algorithm settings from the algorithm dialog box and applies it
/// onto the AlignAlgorithm stored in the user data.
fn apply_algorithm(siv: &mut Cursive) {
    let mut algorithm = AlignAlgorithm::default();
    let mut errors = String::new();

    // read common variables
    parse_box(siv, "gap open", &mut algorithm.gap_open, &mut errors);
    if algorithm.gap_open > 0 {
        errors.push_str("gap open is invalid: must not be positive\n");
    }
    parse_box(siv, "gap extend", &mut algorithm.gap_extend, &mut errors);
    if algorithm.gap_extend > 0 {
        errors.push_str("gap extend is invalid: must not be positive\n");
    }
    parse_box(
        siv,
        "mismatch score",
        &mut algorithm.mismatch_score,
        &mut errors,
    );
    parse_box(siv, "match score", &mut algorithm.match_score, &mut errors);

    // read band settings
    if siv
        .call_on_name("banded", |v: &mut Checkbox| v.is_checked())
        .unwrap()
    {
        let (mut k, mut w) = (DEFAULT_KMER, DEFAULT_WINDOW);
        parse_box(siv, "kmer len", &mut k, &mut errors);
        parse_box(siv, "window size", &mut w, &mut errors);
        algorithm.band = Banded::Banded { kmer: k, window: w };
    } else {
        algorithm.band = Banded::Normal;
    }
    let mut radio_is_selected = |s| {
        siv.call_on_name(s, |v: &mut RadioButton<String>| v.is_selected())
            .unwrap()
    };

    // read mode settings
    if radio_is_selected("local radio") {
        algorithm.mode = AlignMode::Local
    } else if radio_is_selected("global radio") {
        algorithm.mode = AlignMode::Global
    } else if radio_is_selected("blockwise radio") {
        let mut blocksize = DEFAULT_BLOCKSIZE;
        parse_box(siv, "block size", &mut blocksize, &mut errors);
        algorithm.mode = AlignMode::Blockwise(blocksize)
    } else {
        errors.push_str("Could not find any enabled mode radio button\n")
    }

    if !errors.is_empty() {
        siv.add_layer(
            Dialog::text(format!("Error(s) occured:\n{}", errors))
                .title("Error reading algorithm configuration")
                .button("Continue", close_top_maybe_quit),
        );
    } else {
        // AlignAlgorithm is stored in the user data
        siv.user_data::<Settings>().unwrap().algo = algorithm;
        close_top_maybe_quit(siv)
    }
}

fn on_hexview<F, G, T>(siv: &mut Cursive, aligned: F, unaligned: G) -> T
where
    F: FnOnce(&mut Aligned) -> T + 'static,
    G: FnOnce(&mut Unaligned) -> T + 'static,
{
    siv.call_on_name("aligned", aligned)
        .or_else(|| siv.call_on_name("unaligned", unaligned))
        .expect("Could not find aligned or unaligned view in cursive stack")
}

fn apply_style(siv: &mut Cursive) {
    let ascii_col = siv
        .find_name::<Checkbox>("ascii_col")
        .expect("Could not find ascii checkbox in settings")
        .is_checked();
    let vertical = siv
        .find_name::<Checkbox>("vertical")
        .expect("Could not find vertical checkbox in settings")
        .is_checked();
    let mode = number_to_stylemode(
        &siv.find_name::<SelectView<usize>>("display mode")
            .expect("Could not find display mode select view")
            .selected_id()
            .expect("Display mode select view appears to be empty"),
    );
    let new_style = Style {
        mode,
        ascii_col,
        vertical,
    };
    siv.user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .style = new_style;
    on_hexview(
        siv,
        move |v| v.dh.style = new_style,
        move |v| v.dh.style = new_style,
    );
    close_top_maybe_quit(siv)
}

/// Creates a dialog box for algorithm settings.
pub fn algorithm(siv: &mut Cursive) -> impl View {
    let algorithm: &mut AlignAlgorithm = &mut siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .algo;

    // various validator functions for the textboxes
    let is_i32 = |s: &str| s.parse::<i32>().is_ok();
    let is_nonpos_i32 = |s: &str| s.parse::<i32>().map_or(false, |x| x <= 0);
    let is_usize = |s: &str| s.parse::<usize>().is_ok();

    // common parameters:
    // * gap open penalty
    // * gap extend penalty
    // * mismatch score
    // * match score
    // * whether the banded algorithm is used
    let right_always_list = ListView::new()
        .child(
            "Gap Open:",
            validated_box(
                "gap open",
                algorithm.gap_open.to_string(),
                TEXT_WIDTH,
                is_nonpos_i32,
            ),
        )
        .child(
            "Gap Extend:",
            validated_box(
                "gap extend",
                algorithm.gap_extend.to_string(),
                TEXT_WIDTH,
                is_nonpos_i32,
            ),
        )
        .child(
            "Mismatch:",
            validated_box(
                "mismatch score",
                algorithm.mismatch_score.to_string(),
                TEXT_WIDTH,
                is_i32,
            ),
        )
        .child(
            "Match:",
            validated_box(
                "match score",
                algorithm.match_score.to_string(),
                TEXT_WIDTH,
                is_i32,
            ),
        )
        .child(
            "Banded:",
            Checkbox::new()
                .with_checked(!matches!(algorithm.band, crate::align::Banded::Normal))
                .on_change(|siv, state| {
                    siv.call_on_name("band args", |v: &mut EnableableView<ListView>| {
                        v.set_enabled(state)
                    });
                })
                .with_name("banded"),
        );

    // get current k-mer and window size if enabled, otherwise use defaults
    let (k, w) = match algorithm.band {
        Banded::Normal => (DEFAULT_KMER, DEFAULT_WINDOW),
        Banded::Banded { kmer, window } => (kmer, window),
    };

    // the band args are disabled when the `banded` checkbox is unset, so
    // it is wrapped in an EnableableView
    let mut band_args = EnableableView::new(
        ListView::new()
            .child(
                "k-mer Len: ",
                validated_box("kmer len", k.to_string(), TEXT_WIDTH, is_usize),
            )
            .child(
                "Window:",
                validated_box("window size", w.to_string(), TEXT_WIDTH, is_usize),
            ),
    );
    if matches!(algorithm.band, Banded::Normal) {
        band_args.disable();
    }

    // right side consists of common values at the top and band args at the bottom
    let right_side = LinearLayout::vertical()
        .child(right_always_list)
        .child(band_args.with_name("band args"));

    // a radio button group determines what AlignMode we choose
    // below that group, there is a textbox that allows changing the blocksize
    // when the blocksize radio button is enabled
    let mut mode_select = RadioGroup::new().on_change(|siv, item| {
        siv.call_on_name("blocksize enable", |v: &mut EnableableView<ListView>| {
            v.set_enabled(*item == "Blockwise")
        });
    });
    let default_blocksize = match algorithm.mode {
        AlignMode::Local | AlignMode::Global => DEFAULT_BLOCKSIZE,
        AlignMode::Blockwise(x) => x,
    };
    let blocksize_enable = EnableableView::new(ListView::new().child(
        "Block size:",
        validated_box(
            "block size",
            default_blocksize.to_string(),
            TEXT_WIDTH,
            is_usize,
        ),
    ))
    .with_enabled(matches!(algorithm.mode, AlignMode::Blockwise(_)))
    .with_name("blocksize enable");
    let left_side = LinearLayout::vertical()
        .child(Panel::new(
            LinearLayout::vertical()
                .child(
                    mode_select
                        .button_str("Local")
                        .with(|b| {
                            if matches!(algorithm.mode, AlignMode::Local) {
                                b.select();
                            }
                        })
                        .with_name("local radio"),
                )
                .child(
                    mode_select
                        .button_str("Global")
                        .with(|b| {
                            if matches!(algorithm.mode, AlignMode::Global) {
                                b.select();
                            }
                        })
                        .with_name("global radio"),
                )
                .child(
                    mode_select
                        .button_str("Blockwise")
                        .with(|b| {
                            if matches!(algorithm.mode, AlignMode::Blockwise(_)) {
                                b.select();
                            }
                        })
                        .with_name("blockwise radio"),
                )
                .child(blocksize_enable),
        ))
        .child(Button::new("OK", apply_algorithm))
        .child(Button::new("Cancel", close_top_maybe_quit))
        .child(Button::new("Help", help_window(ALGORITHM_HELP)));
    // catch F1 for help
    OnEventView::new(
        Dialog::around(
            LinearLayout::horizontal()
                .child(left_side)
                .child(Panel::new(right_side)),
        )
        .title("Algorithm Options"),
    )
    .on_event(Key::F1, help_window(ALGORITHM_HELP))
}

fn number_to_stylemode(x: &usize) -> DisplayMode {
    match x {
        0 => DisplayMode::Hex,
        1 => DisplayMode::Binary,
        2 => DisplayMode::Decimal,
        3 => DisplayMode::Octal,
        4 => DisplayMode::HexAsciiMix,
        5 => DisplayMode::Braille,
        6 => DisplayMode::Roman,
        otherwise => panic!(
            "Unknown item number {} for style displaymode setting",
            otherwise
        ),
    }
}

pub fn style(siv: &mut Cursive) -> impl View {
    let on_quit = |s: &mut Cursive| {
        let old_style = s
            .user_data::<Settings>()
            .expect("Could not get settings from cursive")
            .style;
        on_hexview(
            s,
            move |v| v.dh.style = old_style,
            move |v| v.dh.style = old_style,
        );
        close_top_maybe_quit(s);
    };
    let style_settings: Style = siv
        .user_data::<Settings>()
        .expect("Could not get style settings from cursive")
        .style;
    let left_side = LinearLayout::vertical()
        .child(
            ListView::new()
                .child(
                    "Ascii Column:",
                    Checkbox::new()
                        .with_checked(style_settings.ascii_col)
                        .on_change(|s, check| {
                            on_hexview(
                                s,
                                move |v| v.dh.style.ascii_col = check,
                                move |v| v.dh.style.ascii_col = check,
                            )
                        })
                        .with_name("ascii_col"),
                )
                .child(
                    "Vertical Split:",
                    Checkbox::new()
                        .with_checked(style_settings.vertical)
                        .on_change(|s, check| {
                            on_hexview(
                                s,
                                move |v| v.dh.style.vertical = check,
                                move |v| v.dh.style.vertical = check,
                            )
                        })
                        .with_name("vertical"),
                ),
        )
        .child(Button::new("OK", apply_style))
        .child(Button::new("Cancel", on_quit))
        .child(Button::new("Help", help_window(STYLE_HELP)));
    let right_side = SelectView::new()
        .with_all([
            ("Hex", 0usize),
            ("Binary", 1),
            ("Decimal", 2),
            ("Octal", 3),
            ("Hex/Ascii Mixed", 4),
            ("Braille", 5),
            ("Roman", 6),
        ])
        .selected(style_settings.mode as usize)
        .on_select(|s, t| {
            let mode = number_to_stylemode(t);
            on_hexview(
                s,
                move |v| v.dh.style.mode = mode,
                move |v| v.dh.style.mode = mode,
            )
        })
        .with_name("display mode");
    OnEventView::new(
        Dialog::around(
            LinearLayout::horizontal()
                .child(Panel::new(left_side))
                .child(Panel::new(right_side)),
        )
        .title("Style Settings"),
    )
    .on_event(Key::F1, help_window(STYLE_HELP))
    .on_event(Key::Esc, apply_style)
}

pub fn settings(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::around(
            LinearLayout::vertical()
                .child(TextView::new("Choose the settings you wish to change"))
                .child(
                    SelectView::new()
                        .with_all([("Algorithm", 0), ("Display Style", 1)])
                        .on_submit(|s, t| match t {
                            0 => {
                                let v = algorithm(s);
                                s.add_layer(v)
                            }
                            1 => {
                                let v = style(s);
                                s.add_layer(v)
                            }
                            otherwise => panic!("Unknown setting selection index: {}", otherwise),
                        }),
                )
                .child(Button::new("Close", close_top_maybe_quit)),
        )
        .title("Settings"),
    )
}

pub fn goto(siv: &mut Cursive) {
    let parse_hex = |s: &str| usize::from_str_radix(s.strip_prefix("0x").unwrap_or(s), 16);
    let call_goto = move |s: &mut Cursive, right: bool| {
        let result = s
            .call_on_name("goto address", |v: &mut EditView| {
                parse_hex(&v.get_content())
            })
            .unwrap()
            .map_err(|e| e.to_string())
            .and_then(|pos| {
                s.call_on_name("aligned", |v: &mut Aligned| {
                    v.goto(&mut crate::backend::Dummy {}, right, pos)
                })
                .or_else(|| {
                    s.call_on_name("unaligned", |v: &mut Unaligned| {
                        v.goto(&mut crate::backend::Dummy {}, right, pos)
                    })
                })
                .unwrap()
            });

        match result {
            Err(e) => s.add_layer(
                Dialog::text(format!("Error in search: {}", e))
                    .button("Continue", close_top_maybe_quit),
            ),
            Ok(()) => close_top_maybe_quit(s),
        }
    };
    siv.add_layer(
        OnEventView::new(
            Dialog::around(
                LinearLayout::horizontal()
                    .child(TextView::new("Address: "))
                    .child(validated_box("goto address", String::new(), 16, move |s| {
                        parse_hex(s).is_ok()
                    })),
            )
            // fun fact: call_goto itself is 4 nested closures, so this makes it 5
            .button("Goto Primary", move |siv| call_goto(siv, false))
            .button("Goto Secondary", move |siv| call_goto(siv, true))
            .button("Cancel", close_top_maybe_quit)
            .title("Goto"),
        )
        .on_event(Key::F1, help_window(GOTO_HELP)),
    );
}

/// We only want to quit cursive and return to our crossterm native implementation
/// when no other windows are open. This function wraps that behaviour.
pub fn close_top_maybe_quit(siv: &mut Cursive) {
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

pub const MAIN_HELP: &str = include_str!("help/main.txt");
pub const ALGORITHM_HELP: &str = include_str!("help/algorithm.txt");
pub const STYLE_HELP: &str = include_str!("help/style.txt");
pub const GOTO_HELP: &str = include_str!("help/goto.txt");

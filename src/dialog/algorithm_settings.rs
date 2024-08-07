use crate::{config::Settings, preset::PresetCursor};
use biodiff_align::{
    rustbio::{RustBio, DEFAULT_KMER, DEFAULT_WINDOW},
    wfa2::{Wfa2, WFA2_AVAILABLE},
    AlgorithmKind, AlignBackend, CheckStatus,
};

use self::algorithm_presets::refresh_presets;

use super::*;
const TEXT_WIDTH: usize = 10;

fn apply_rustbio(siv: &mut Cursive, algo: &mut AlignAlgorithm, errors: &mut String) {
    let mut rustbio = RustBio::default();
    // read band settings
    if siv
        .call_on_name("banded", |v: &mut Checkbox| v.is_checked())
        .unwrap()
    {
        let (mut k, mut w) = (DEFAULT_KMER, DEFAULT_WINDOW);
        parse_box(siv, "kmer len", &mut k, errors);
        parse_box(siv, "window size", &mut w, errors);
        rustbio.band = Banded::Banded { kmer: k, window: w };
    } else {
        rustbio.band = Banded::Normal;
    }
    algo.backend = AlignBackend::RustBio(rustbio);
}

fn apply_wfa2(_: &mut Cursive, algo: &mut AlignAlgorithm, _: &mut String) {
    algo.backend = AlignBackend::Wfa2(Wfa2);
}

/// Reads the algorithm settings from the algorithm dialog box and applies it
/// onto the AlignAlgorithm stored in the user data.
fn apply_algorithm(
    siv: &mut Cursive,
    cursor: PresetCursor,
    backend_radio: RadioGroup<&'static str>,
) {
    let mut algorithm = AlignAlgorithm::default();
    let mut errors = String::new();
    let mut radio_is_selected = |s| {
        siv.call_on_name(s, |v: &mut RadioButton<String>| v.is_selected())
            .unwrap()
    };

    // read mode settings
    if cursor.kind == AlgorithmKind::Global {
        if radio_is_selected("global radio") {
            algorithm.mode = AlignMode::Global
        } else if radio_is_selected("blockwise radio") {
            let mut blocksize = DEFAULT_BLOCKSIZE;
            parse_box(siv, "block size", &mut blocksize, &mut errors);
            algorithm.mode = AlignMode::Blockwise(blocksize)
        } else {
            errors.push_str("Could not find any enabled mode radio button\n")
        }
    } else {
        algorithm.mode = AlignMode::Semiglobal;
    }

    // read common variables
    parse_box(siv, "name", &mut algorithm.name, &mut errors);
    parse_box(siv, "gap open", &mut algorithm.gap_open, &mut errors);
    parse_box(siv, "gap extend", &mut algorithm.gap_extend, &mut errors);
    parse_box(
        siv,
        "mismatch score",
        &mut algorithm.mismatch_score,
        &mut errors,
    );
    parse_box(siv, "match score", &mut algorithm.match_score, &mut errors);

    match *backend_radio.selection() {
        "rustbio" => apply_rustbio(siv, &mut algorithm, &mut errors),
        "wfa2" => apply_wfa2(siv, &mut algorithm, &mut errors),
        _ => errors.push_str("Unknown backend selected\n"),
    }
    if let CheckStatus::Error(e) = algorithm.check_parameters([0; 2]) {
        errors.push_str(&e);
    }
    if !errors.is_empty() {
        siv.add_layer(
            Dialog::text(format!("Error(s) occured:\n{errors}"))
                .title("Error reading algorithm configuration")
                .button("Continue", close_top_maybe_quit),
        );
        return;
    }
    let name = algorithm.name.clone();
    // AlignAlgorithm is stored in the user data
    let is_set = siv
        .user_data::<Settings>()
        .unwrap()
        .presets
        .set(cursor, algorithm);
    if !is_set {
        siv.add_layer(
            Dialog::text(format!(
                "Algorithm Settings with name \"{}\" already exists",
                name
            ))
            .title("Error setting algorithm configuration")
            .button("Continue", close_top_maybe_quit),
        );
        return;
    }
    refresh_presets(siv);
    close_top_maybe_quit(siv)
}

fn rustbio_settings(rustbio: RustBio) -> LinearLayout {
    let mut layout = LinearLayout::vertical();
    let is_usize = |s: &str| s.parse::<usize>().is_ok();
    layout.add_child(
        Checkbox::new()
            .with_checked(!matches!(rustbio.band, crate::align::Banded::Normal,))
            .on_change(|siv, state| {
                siv.call_on_name("band args", |v: &mut EnableableView<ListView>| {
                    v.set_enabled(state)
                });
            })
            .with_name("banded"),
    );
    // get current k-mer and window size if enabled, otherwise use defaults
    let (k, w) = match rustbio.band {
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
    if matches!(rustbio.band, Banded::Normal) {
        band_args.disable();
    }
    layout.add_child(band_args.with_name("band args"));
    layout
}

/// Creates a dialog box for algorithm settings.
pub fn algorithm(siv: &mut Cursive, cursor: PresetCursor) -> impl View {
    let algorithm: &AlignAlgorithm = &siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .presets
        .get(cursor);

    // various validator functions for the textboxes
    let is_i32 = |s: &str| s.parse::<i32>().is_ok();
    let is_nonpos_i32 = |s: &str| s.parse::<i32>().map_or(false, |x| x <= 0);
    let is_usize = |s: &str| s.parse::<usize>().is_ok();

    let mut backend_radio = RadioGroup::new().on_change(|siv, item: &&str| {
        siv.call_on_name("rustbio settings", |v: &mut HideableView<LinearLayout>| {
            v.set_visible(*item == "rustbio")
        });
    });

    let backends = LinearLayout::vertical()
        .child(
            backend_radio
                .button("rustbio", "rustbio")
                .with(|b| {
                    if matches!(algorithm.backend, AlignBackend::RustBio(_)) {
                        b.select();
                    }
                })
                .with_name("rustbio radio"),
        )
        .child(
            backend_radio
                .button("wfa2", "wfa2")
                .with(|b| {
                    if matches!(algorithm.backend, AlignBackend::Wfa2(_)) {
                        b.select();
                    }
                    b.set_enabled(WFA2_AVAILABLE)
                })
                .with_name("wfa2 radio"),
        );
    // common parameters:
    // * gap open penalty
    // * gap extend penalty
    // * mismatch score
    // * match score
    // * what backend is used
    let right_always_list = ListView::new()
        .child(
            "Name:",
            validated_box("name", algorithm.name.clone(), TEXT_WIDTH, |s| {
                !s.is_empty()
            }),
        )
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
        .child("Backend:", backends);
    let rustbio = match &algorithm.backend {
        AlignBackend::RustBio(r) => *r,
        _ => RustBio::default(),
    };
    let rustbio = rustbio_settings(rustbio);

    // right side consists of common values at the top and band args at the bottom
    let right_side = LinearLayout::vertical().child(right_always_list).child(
        HideableView::new(rustbio)
            .visible(matches!(&algorithm.backend, AlignBackend::RustBio(_)))
            .with_name("rustbio settings"),
    );

    // a radio button group determines what AlignMode we choose
    // below that group, there is a textbox that allows changing the blocksize
    // when the blocksize radio button is enabled
    let mut mode_select = RadioGroup::new().on_change(|siv, item| {
        siv.call_on_name("blocksize enable", |v: &mut EnableableView<ListView>| {
            v.set_enabled(*item == "Blockwise")
        });
    });
    let default_blocksize = match algorithm.mode {
        AlignMode::Blockwise(x) => x,
        _ => DEFAULT_BLOCKSIZE,
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

    let mut left_side = LinearLayout::vertical();
    if cursor.kind == AlgorithmKind::Global {
        left_side.add_child(Panel::new(
            LinearLayout::vertical()
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
        ));
    } else {
        // this is inserted to make it more consistent with the global algorithm
        // panel and because the buttons below look weird otherwise
        left_side.add_child(Panel::new(TextView::new("Semiglobal").min_width(12)));
    }
    if cursor.preset.is_some() {
        let backend_radio = backend_radio.clone();
        left_side.add_child(Button::new("Apply", move |siv| {
            apply_algorithm(siv, cursor, backend_radio.clone())
        }))
    }
    left_side = left_side
        .child(Button::new("New Preset", move |siv| {
            apply_algorithm(
                siv,
                PresetCursor {
                    preset: None,
                    kind: cursor.kind,
                },
                backend_radio.clone(),
            )
        }))
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

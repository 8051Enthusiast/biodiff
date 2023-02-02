use super::*;
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
            Dialog::text(format!("Error(s) occured:\n{errors}"))
                .title("Error reading algorithm configuration")
                .button("Continue", close_top_maybe_quit),
        );
    } else {
        // AlignAlgorithm is stored in the user data
        siv.user_data::<Settings>().unwrap().algo = algorithm;
        close_top_maybe_quit(siv)
    }
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

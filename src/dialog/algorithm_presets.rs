use crate::{
    config::Settings,
    preset::{AlgorithmKind, PresetCursor, PresetList},
};

use super::*;

fn title(kind: AlgorithmKind) -> &'static str {
    match kind {
        AlgorithmKind::Global => "Global Presets",
        AlgorithmKind::Semiglobal => "Semiglobal Presets",
    }
}

// Allows the user to select a global/semiglobal preset from the preset lists
// and create new presets
pub fn presets(siv: &mut Cursive) -> NamedView<impl View> {
    let preset_lists: &PresetList = &mut siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .presets;

    // we have two lists of presets next to eachother
    // and make each of them with this function

    let make_side = |presets: &[AlignAlgorithm], current: u32, kind: AlgorithmKind| {
        let mut select_view = SelectView::new();
        select_view.add_all(
            presets
                .iter()
                .enumerate()
                .map(|(i, p)| (p.name.clone(), i as u32)),
        );
        select_view.set_selection(current as usize);
        select_view.set_on_submit(move |siv, idx| {
            apply_presets(siv);
            let dialog = algorithm(
                siv,
                PresetCursor {
                    preset: Some(*idx),
                    kind,
                },
            );
            siv.add_layer(dialog);
        });
        LinearLayout::vertical()
            .child(TextView::new(title(kind)))
            .child(select_view.with_name(title(kind)))
            .min_width(22)
    };

    let global = make_side(
        &preset_lists.global,
        preset_lists.current_global,
        AlgorithmKind::Global,
    );
    let semiglobal = make_side(
        &preset_lists.semiglobal,
        preset_lists.current_semiglobal,
        AlgorithmKind::Semiglobal,
    );

    let close = |siv: &mut Cursive| {
        apply_presets(siv);
        close_top_maybe_quit(siv);
    };
    OnEventView::new(
        Dialog::around(LinearLayout::horizontal().child(global).child(semiglobal))
            .title("Alignment Presets")
            .button("Close", close),
    )
    .on_event(Key::Esc, close)
    .with_name("presets")
}

pub fn refresh_presets(siv: &mut Cursive) {
    let Ok(view) = presets(siv).into_inner() else {
        panic!("Could not refresh presets view")
    };
    siv.call_on_name("presets", |v| {
        *v = view;
    });
}

fn apply_presets(siv: &mut Cursive) {
    let global = siv
        .find_name::<SelectView<u32>>(title(AlgorithmKind::Global))
        .expect("Could not find global presets select view")
        .selected_id()
        .expect("Global presets select view appears to be empty");
    let semiglobal = siv
        .find_name::<SelectView<u32>>(title(AlgorithmKind::Semiglobal))
        .expect("Could not find semiglobal presets select view")
        .selected_id()
        .expect("Semiglobal presets select view appears to be empty");

    let preset_lists: &mut PresetList = &mut siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .presets;
    preset_lists.current_global = global as u32;
    preset_lists.current_semiglobal = semiglobal as u32;
}

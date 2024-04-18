use cursive::{direction::Direction, event::Event};

use crate::{
    align::AlgorithmKind,
    config::Settings,
    preset::{PresetCursor, PresetList},
};

use super::*;

fn title(kind: AlgorithmKind) -> &'static str {
    match kind {
        AlgorithmKind::Global => "Global Presets",
        AlgorithmKind::Semiglobal => "Semiglobal Presets",
    }
}

fn delete_preset(siv: &mut Cursive, kind: AlgorithmKind, i: usize) {
    let preset_lists: &mut PresetList = &mut siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .presets;
    let cursor = PresetCursor {
        kind,
        preset: Some(i as u32),
    };
    if !preset_lists.delete(cursor) {
        error_window(String::from(
            "Cannot delete: At least one preset needs to be present.",
        ))(siv)
    } else {
        refresh_presets(siv);
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
    let global_radio = RadioGroup::new();

    let semiglobal_radio = RadioGroup::new();

    let make_side = |presets: &[AlignAlgorithm],
                     current: u32,
                     kind: AlgorithmKind,
                     mut group: RadioGroup<u32>| {
        let mut layout = LinearLayout::vertical().child(TextView::new(title(kind)));
        let global = global_radio.clone();
        let semiglobal = semiglobal_radio.clone();
        layout.add_child(Button::new("Edit Current", move |siv| {
            apply_presets(siv, &global, &semiglobal);
            let dialog = algorithm(
                siv,
                PresetCursor {
                    preset: Some(current),
                    kind,
                },
            );
            siv.add_layer(dialog);
        }));
        let global = global_radio.clone();
        let semiglobal = semiglobal_radio.clone();
        layout.add_child(Button::new("New Preset", move |siv| {
            apply_presets(siv, &global, &semiglobal);
            let dialog = algorithm(siv, PresetCursor { preset: None, kind });
            siv.add_layer(dialog);
        }));
        let mut inner_layout = LinearLayout::vertical();
        for (i, p) in presets.iter().enumerate() {
            let mut button = group.button(i as u32, p.name.clone());
            if i as u32 == current {
                button.select();
            }
            let button = OnEventView::new(button)
                .on_event(Event::Key(Key::Del), move |siv| delete_preset(siv, kind, i));
            inner_layout.add_child(button);
        }
        layout.add_child(Panel::new(inner_layout.scrollable().scroll_x(true)));
        layout.min_width(25)
    };

    let global = make_side(
        &preset_lists.global,
        preset_lists.current_global,
        AlgorithmKind::Global,
        global_radio.clone(),
    );
    let semiglobal = make_side(
        &preset_lists.semiglobal,
        preset_lists.current_semiglobal,
        AlgorithmKind::Semiglobal,
        semiglobal_radio.clone(),
    );

    let close = move |siv: &mut Cursive| {
        apply_presets(siv, &global_radio, &semiglobal_radio);
        close_top_maybe_quit(siv);
    };
    OnEventView::new(
        Dialog::around(LinearLayout::horizontal().child(global).child(semiglobal))
            .title("Alignment Presets")
            .button("Close", close.clone())
            .button("Help", help_window(PRESET_HELP)),
    )
    .on_event(Key::Esc, close)
    .on_event(Key::F1, help_window(PRESET_HELP))
    .with_name("presets")
}

pub fn refresh_presets(siv: &mut Cursive) {
    let Ok(view) = presets(siv).into_inner() else {
        panic!("Could not refresh presets view")
    };
    siv.call_on_name("presets", |v| {
        *v = view;
        v.take_focus(Direction::left())
    });
}

fn apply_presets(siv: &mut Cursive, global: &RadioGroup<u32>, semiglobal: &RadioGroup<u32>) {
    let preset_lists: &mut PresetList = &mut siv
        .user_data::<Settings>()
        .expect("Could not get align algorithm info from cursive")
        .presets;
    preset_lists.current_global = *global.selection();
    preset_lists.current_semiglobal = *semiglobal.selection();
}

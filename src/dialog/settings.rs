use super::*;
fn save_settings(siv: &mut Cursive) {
    let settings = siv
        .user_data::<Settings>()
        .expect("Could not get settings from cursive");
    if let Err(e) = settings.save_config() {
        siv.add_layer(
            Dialog::text(format!("Could not save config: {e}"))
                .button("Close", close_top_maybe_quit)
                .title("Error saving config"),
        )
    } else {
        siv.add_layer(
            Dialog::text("Successfully saved config!")
                .button("Close", close_top_maybe_quit)
                .title("Saved config"),
        )
    }
}

/// A settings dialog to choose between algorithm and style settings
pub fn settings(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::around(
            LinearLayout::vertical()
                .child(TextView::new(
                    "Choose the settings you wish to change.\n\
                    Settings can be saved permanently by clicking 'Save'.\n",
                ))
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
                            otherwise => panic!("Unknown setting selection index: {otherwise}"),
                        }),
                ),
        )
        .button("Close", close_top_maybe_quit)
        .button("Save", save_settings)
        .title("Settings"),
    )
}

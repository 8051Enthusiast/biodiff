use super::*;
fn apply_style(siv: &mut Cursive) {
    let ascii_col = siv
        .find_name::<Checkbox>("ascii_col")
        .expect("could not find ascii checkbox in settings")
        .is_checked();
    let bars_col = siv
        .find_name::<Checkbox>("bars_col")
        .expect("could not find bars checkbox in settings")
        .is_checked();
    let vertical = siv
        .find_name::<Checkbox>("vertical")
        .expect("Could not find vertical checkbox in settings")
        .is_checked();
    let spacer = siv
        .find_name::<Checkbox>("spacer")
        .expect("Could not find spacer checkbox in settings")
        .is_checked();
    let right_to_left = siv
        .find_name::<Checkbox>("right_to_left")
        .expect("Could not find right_to_left checkbox in settings")
        .is_checked();
    let column_count = siv
        .find_name::<EditView>("column_count")
        .expect("Could not find column count edit view")
        .get_content();
    let column_count = match column_count.as_str() {
        "" => ColumnSetting::Fit,
        otherwise => match otherwise.parse::<ColumnSetting>() {
            Ok(c) => c,
            Err(e) => {
                siv.add_layer(
                    Dialog::text(format!("Could not parse column count: {}", e))
                        .title("Error")
                        .button("Continue", close_top_maybe_quit),
                );
                return;
            }
        },
    };
    let mode = number_to_stylemode(
        &siv.find_name::<SelectView<usize>>("display mode")
            .expect("Could not find display mode select view")
            .selected_id()
            .expect("Display mode select view appears to be empty"),
    );
    let new_style = Style {
        mode,
        ascii_col,
        bars_col,
        vertical,
        spacer,
        right_to_left,
        column_count,
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

fn number_to_stylemode(x: &usize) -> DisplayMode {
    match x {
        0 => DisplayMode::Hex,
        1 => DisplayMode::Binary,
        2 => DisplayMode::Decimal,
        3 => DisplayMode::Octal,
        4 => DisplayMode::HexAsciiMix,
        5 => DisplayMode::Braille,
        6 => DisplayMode::Roman,
        otherwise => panic!("Unknown item number {otherwise} for style displaymode setting"),
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
    let column_string = match style_settings.column_count {
        ColumnSetting::Fit => String::new(),
        ColumnSetting::Fixed(x) => x.to_string(),
        ColumnSetting::Multiple(x) => format!("{}x", x),
    };
    let column_box = EditView::new()
        .content(column_string)
        .on_edit_mut(move |siv, s, _| {
            let col = s.parse::<ColumnSetting>();
            if col.is_ok() {
                siv.call_on_name("column_count", |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Secondary))
                });
            } else {
                siv.call_on_name("column_count", |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Highlight))
                });
            };
            if let Ok(m) = col {
                on_hexview(
                    siv,
                    move |v| v.dh.style.column_count = m,
                    move |v| v.dh.style.column_count = m,
                )
            }
        })
        .with_name("column_count")
        .fixed_width(TEXT_WIDTH);
    // checkboxes for:
    // * ascii column
    // * vertical split
    // * hex spacers
    // * right to left mode
    let left_side = ListView::new()
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
        )
        .child(
            "Hex Spacer:",
            Checkbox::new()
                .with_checked(style_settings.spacer)
                .on_change(|s, check| {
                    on_hexview(
                        s,
                        move |v| v.dh.style.spacer = check,
                        move |v| v.dh.style.spacer = check,
                    )
                })
                .with_name("spacer"),
        )
        .child(
            "Right to Left:",
            Checkbox::new()
                .with_checked(style_settings.right_to_left)
                .on_change(|s, check| {
                    on_hexview(
                        s,
                        move |v| v.dh.style.right_to_left = check,
                        move |v| v.dh.style.right_to_left = check,
                    )
                })
                .with_name("right_to_left"),
        )
        .child(
            "Ascii Column:",
            Checkbox::new()
                .with_checked(style_settings.ascii_col)
                .on_change(|s, check| {
                    on_hexview(
                        s,
                        move |v| v.dh.style.ascii_col = check,
                        move |v| v.dh.style.ascii_col = check,
                    );
                })
                .with_name("ascii_col"),
        )
        .child(
            "Bar Column:",
            Checkbox::new()
                .with_checked(style_settings.bars_col)
                .on_change(|s, check| {
                    on_hexview(
                        s,
                        move |v| v.dh.style.bars_col = check,
                        move |v| v.dh.style.bars_col = check,
                    );
                })
                .with_name("bars_col"),
        )
        .child("Column Count:", column_box);
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
        .button("OK", apply_style)
        .button("Cancel", on_quit)
        .button("Help", help_window(STYLE_HELP))
        .title("Style Settings"),
    )
    .on_event(Key::F1, help_window(STYLE_HELP))
    .on_event(Key::Esc, apply_style)
}

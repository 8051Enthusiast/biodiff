use super::*;

fn parse_hex(s: &str) -> Result<(usize, Option<usize>), ParseIntError> {
    let mut split = s.splitn(2, '|');
    let to_int = |s: &str| usize::from_str_radix(s.strip_prefix("0x").unwrap_or(s), 16);
    let first = to_int(split.next().unwrap())?;
    let second = split.next().map(to_int).transpose()?;
    Ok((first, second))
}

/// A dialog to go to a given position in the hexview
pub fn goto(siv: &mut Cursive) {
    let call_goto = move |siv: &mut Cursive, s: &str| {
        let result = parse_hex(s)
            .map_err(|e| e.to_string())
            .and_then(|(first, second)| {
                on_hexview(
                    siv,
                    // we pass the dummy printer because we do not need
                    // to draw when we are in the cursive backend
                    move |v| v.goto(&mut crate::backend::Dummy, first, second),
                    move |v| v.goto(&mut crate::backend::Dummy, first, second),
                )
            });

        match result {
            Err(e) => siv.add_layer(
                Dialog::text(format!("Error in goto: {e}"))
                    .button("Continue", close_top_maybe_quit),
            ),
            Ok(()) => close_top_maybe_quit(siv),
        }
    };
    let find_s_and_call_goto = move |siv: &mut Cursive| {
        let s = siv
            .call_on_name("goto address", |v: &mut EditView| v.get_content())
            .unwrap();
        call_goto(siv, &s)
    };
    let name = "goto address";
    let textbox = EditView::new()
        .content(String::new())
        .on_edit_mut(move |siv, s, _| {
            match parse_hex(s).is_ok() {
                true => siv.call_on_name(name, |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Secondary))
                }),
                false => siv.call_on_name(name, |v: &mut EditView| {
                    v.set_style(StyleType::from(PaletteColor::Highlight))
                }),
            };
        })
        .on_submit(call_goto)
        .with_name(name)
        .fixed_width(33);
    siv.add_layer(
        OnEventView::new(
            Dialog::around(
                LinearLayout::horizontal()
                    .child(TextView::new("Address: "))
                    .child(textbox),
            )
            .button("Goto", find_s_and_call_goto)
            .button("Cancel", close_top_maybe_quit)
            .title("Goto"),
        )
        .on_event(Key::F1, help_window(GOTO_HELP)),
    );
}

use super::*;
/// A dialog to go to a given position in the hexview
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
                on_hexview(
                    s,
                    // we pass the dummy printer because we do not need
                    // to draw when we are in the cursive backend
                    move |v| v.goto(&mut crate::backend::Dummy, right, pos),
                    move |v| v.goto(&mut crate::backend::Dummy, right, pos),
                )
            });

        match result {
            Err(e) => s.add_layer(
                Dialog::text(format!("Error in search: {e}"))
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
            .button("Goto Primary", move |siv| call_goto(siv, false))
            .button("Goto Secondary", move |siv| call_goto(siv, true))
            .button("Cancel", close_top_maybe_quit)
            .title("Goto"),
        )
        .on_event(Key::F1, help_window(GOTO_HELP)),
    );
}

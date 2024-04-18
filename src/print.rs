use std::sync::mpsc;

use crate::{
    align::AlignElement,
    backend::{Backend, BackgroundColor, Color, Cross, Effect, Plain},
    config::get_settings,
    doublehex::DoubleHexLine,
    file::FileContent,
    style::{ByteData, ColumnSetting},
    view::AlignedMessage,
};

pub fn print(x: FileContent, y: FileContent, cols: u16, plain: bool) {
    let backend = if plain {
        &mut Plain::new() as &mut dyn Backend
    } else {
        &mut Cross::init_textmode() as &mut dyn Backend
    };
    let mut settings = get_settings();
    settings.style.set_addr_size(&x, &y);
    settings.style.column_count = ColumnSetting::Fixed(cols);
    let align_info = settings.presets.current_info();
    let mut buf = Vec::new();
    let (sender, receiver) = mpsc::channel();
    align_info.start_align_with_selection([x, y], [None, None], [0, 0], sender);
    let mut idx = 0;
    let byte_data = |byte| ByteData {
        byte,
        is_search_result: false,
        is_selected: crate::selection::SelectionStatus::None,
    };
    let new_line = |buf: &[AlignElement], idx: usize| {
        let xaddr = buf[idx].xaddr;
        let yaddr = buf[idx].yaddr;
        let end = buf.len().min(idx + cols as usize);
        let mut bytes: Vec<_> = buf[idx..end]
            .iter()
            .map(|x| (byte_data(x.xbyte), byte_data(x.ybyte)))
            .collect();
        bytes.resize_with(cols as usize, || (byte_data(None), byte_data(None)));
        DoubleHexLine {
            address: [Some(xaddr), Some(yaddr)],
            bytes,
        }
    };
    for msg in receiver {
        let mut appendix = match msg {
            AlignedMessage::Initial(v, _) | AlignedMessage::Append(v) => v,
            AlignedMessage::Prepend(_) | AlignedMessage::UserEvent(_) => continue,
        };
        buf.append(&mut appendix);
        while idx + cols as usize <= buf.len() {
            let line = new_line(&buf, idx);
            line.print_hor(backend, 0, settings.style);
            backend.append_text(
                "\n",
                Color::HexSame,
                BackgroundColor::Blank,
                Effect::default(),
            );
            backend.refresh();
            idx += cols as usize;
        }
    }
    if idx < buf.len() {
        let line = new_line(&buf, idx);
        line.print_hor(backend, 0, settings.style);
        backend.append_text(
            "\n",
            Color::HexSame,
            BackgroundColor::Blank,
            Effect::default(),
        );
        backend.refresh();
    }
}

use std::{
    io::IsTerminal,
    sync::{mpsc, Arc},
};

use crate::{
    backend::{Backend, BackgroundColor, Color, Cross, Effect, Plain},
    config::Settings,
    doublehex::DoubleHexLine,
    file::{FileBytes, FileContent},
    style::{ByteData, ColumnSetting},
    view::AlignedMessage,
};
use biodiff_align::AlignElement;

fn use_color(color_override: bool) -> bool {
    if color_override {
        return true;
    }
    if let Ok(nc) = std::env::var("NO_COLOR") {
        if !nc.is_empty() {
            return false;
        }
    }
    let stdout = std::io::stdout();
    if !stdout.is_terminal() {
        return false;
    }
    true
}

fn hex_line(buf: &[AlignElement], line_num: usize, cols: u16) -> DoubleHexLine {
    let idx = line_num * cols as usize;
    let byte_data = |byte| ByteData {
        byte,
        is_search_result: false,
        is_selected: crate::selection::SelectionStatus::None,
    };
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
}

fn print_impl<B: Backend>(
    settings: Settings,
    x: Arc<FileBytes>,
    y: Arc<FileBytes>,
    cols: u16,
    backend: &mut B,
) {
    let align_info = settings.presets.current_info();
    let mut buf = Vec::new();
    let (sender, receiver) = mpsc::channel();
    align_info.start_align_with_selection([x, y], [None, None], [0, 0], move |r| {
        sender.send(r.into()).is_ok()
    });
    let mut line_num = 0;
    for msg in receiver {
        let mut appendix = match msg {
            AlignedMessage::Initial(v, _) | AlignedMessage::Append(v) => v,
            AlignedMessage::Prepend(_) | AlignedMessage::UserEvent(_) => continue,
        };
        buf.append(&mut appendix);
        while (line_num + 1) * cols as usize <= buf.len() {
            let line = hex_line(&buf, line_num, cols);
            line.print_hor(backend, line_num, settings.style);
            backend.append_text(
                "\n",
                Color::HexSame,
                BackgroundColor::Blank,
                Effect::default(),
            );
            backend.refresh();
            line_num += 1;
        }
    }
    if (line_num * cols as usize) < buf.len() {
        let line = hex_line(&buf, line_num, cols);
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

pub fn print(mut settings: Settings, x: FileContent, y: FileContent, color: bool) {
    let cols = if let ColumnSetting::Fixed(cols) = settings.style.column_count {
        cols
    } else {
        16
    };
    let plain = !use_color(color);
    settings.style.set_addr_size(&x, &y);
    settings.style.column_count = ColumnSetting::Fixed(cols);

    if plain {
        print_impl(settings, x, y, cols, &mut Plain::new());
    } else {
        print_impl(settings, x, y, cols, &mut Cross::init_textmode());
    };
}

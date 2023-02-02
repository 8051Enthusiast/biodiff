use super::*;
/// "set offset" dialog, which can set the offset of an unaligned view
/// in various ways
pub fn set_offset(siv: &mut Cursive) {
    if on_hexview(siv, |_| true, |_| false) {
        // TODO: figure out why this dialog box is needed and i cannot just call
        // close_top_maybe_quit directly
        siv.add_layer(
            Dialog::text("Set offset can only be done on an unaligned view!")
                .title("Specifity Error")
                .button("Continue", close_top_maybe_quit),
        );
        return;
    };
    let execute_align = |s: &mut Cursive, which: &i32| match *which {
        0 => {
            on_hexview(s, |_| (), |v| v.align_start(&mut Dummy));
            close_top_maybe_quit(s);
        }
        1 => {
            on_hexview(s, |_| (), |v| v.align_end(&mut Dummy));
            close_top_maybe_quit(s);
        }
        2 => {
            s.pop_layer();
            FlatAlignmentProgress::make_new(s);
        }
        _ => (),
    };
    let dialog = OnEventView::new(
        Dialog::around(
            SelectView::new()
                .with_all([
                    ("Align offset at start", 0),
                    ("Align offset at end", 1),
                    ("Align offset at biggest overlap", 2),
                ])
                .on_submit(execute_align),
        )
        .title("Set offset")
        .button("Cancel", close_top_maybe_quit)
        .button("Help", help_window(SET_OFFSET_HELP)),
    )
    .on_event(Key::F1, help_window(SET_OFFSET_HELP));
    siv.add_layer(dialog);
}

const FLAT_ALIGNMENT_PROGRESS: &str = "flat alignment progress";
/// Shows the progress of the flat alignment from the "set offset" dialog
pub struct FlatAlignmentProgress {
    view: BoxedView,
    /// Boolean to cancel the aligning process
    is_running: Arc<AtomicBool>,
    /// Progress counter (0 to 256)
    counter: Arc<AtomicUsize>,
}

impl ViewWrapper for FlatAlignmentProgress {
    wrap_impl!(self.view: BoxedView);
}

impl FlatAlignmentProgress {
    /// opens a new  flat alignment progress dialog and starts the alignment process
    /// which calls back with the results later
    pub fn make_new(siv: &mut Cursive) {
        let content = match siv.call_on_name("unaligned", |s: &mut Unaligned| s.data.clone()) {
            Some(c) => [c.xvec, c.yvec],
            None => {
                siv.add_layer(
                    Dialog::text("Hexview is not unaligned")
                        .title("Error")
                        .button("Cancel", close_top_maybe_quit),
                );
                return;
            }
        };
        let sink = siv.cb_sink().clone();
        let is_running = Arc::new(AtomicBool::new(true));
        let is_running_2_electric_boogaloo = is_running.clone();
        let update_progress = aligned_callback(sink, is_running.clone());
        let counter = Counter(Arc::new(AtomicUsize::new(0)));
        let dialog = Dialog::around(
            LinearLayout::vertical()
                .child(TextView::new("Aligning..."))
                .child(
                    ProgressBar::new()
                        .min(0)
                        .max(256)
                        .with_value(counter.clone())
                        .min_width(16),
                ),
        )
        .button("Cancel", |s| {
            s.call_on_name(FLAT_ALIGNMENT_PROGRESS, |fa: &mut FlatAlignmentProgress| {
                fa.is_running
                    .store(false, std::sync::atomic::Ordering::Relaxed);
            });
            close_top_maybe_quit(s)
        });
        siv.add_layer(
            FlatAlignmentProgress {
                view: BoxedView::new(Box::new(dialog)),
                is_running: is_running_2_electric_boogaloo,
                counter: counter.0,
            }
            .with_name(FLAT_ALIGNMENT_PROGRESS),
        );
        std::thread::spawn(move || {
            FlatAlignmentContext::new(is_running, content, update_progress).align_flat()
        });
    }
    fn update_count(&mut self, new_count: u16) {
        self.counter
            .store(new_count as usize, std::sync::atomic::Ordering::Relaxed);
    }
}

/// the alignment process sends back messages of two kinds:
///  * incomplete messages, which include the current progress (from 0 to 256)
///  * an complete message, which is the last message and contains the result offset
///  this callback either updates the progress bar or applies the result to the
///  unaligned hexview
fn aligned_callback(
    sink: CbSink,
    is_running: Arc<AtomicBool>,
) -> Box<dyn FnMut(FlatAlignProgressMessage) + 'static + Send> {
    Box::new(move |msg| {
        let sink = sink.clone();
        let is_running = is_running.clone();
        let is_running2 = is_running.clone();
        let _ = sink
            .send(Box::new(move |siv: &mut Cursive| match msg {
                FlatAlignProgressMessage::Incomplete(i) => {
                    let send = siv
                        .call_on_name(FLAT_ALIGNMENT_PROGRESS, |f: &mut FlatAlignmentProgress| {
                            f.update_count(i)
                        });
                    if send.is_none() {
                        is_running.store(false, std::sync::atomic::Ordering::Relaxed)
                    }
                }
                FlatAlignProgressMessage::Complete(c) => {
                    let _ = siv.call_on_name("unaligned", |s: &mut Unaligned| {
                        s.align_custom(&mut Dummy, c);
                    });
                    close_top_maybe_quit(siv);
                }
            }))
            .map_err(|_| is_running2.store(false, std::sync::atomic::Ordering::Relaxed));
    })
}

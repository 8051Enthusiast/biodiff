use super::*;
const SEARCH_DIALOG: &str = "search dialog";
const SEARCH_BOX: &str = "search box";
const SEARCH_MODE: &str = "search mode";

/// A dialog for searching bytes in the hexview
pub fn search(siv: &mut Cursive) {
    let query = on_hexview(
        siv,
        |v| v.current_search_query().cloned(),
        |v| v.current_search_query().cloned(),
    );
    let query_kind = match query.as_ref().map_or(QueryType::Text, |x| x.query_type()) {
        QueryType::Text => 0,
        QueryType::Regex => 1,
        QueryType::Hexagex => 2,
    };
    let query_text = query.as_ref().map_or("", |x| x.text());
    // this pops up on regex compilation errors
    let do_search = |s: &mut Cursive| {
        if let Err(e) = on_search(s) {
            s.add_layer(
                Dialog::text(e)
                    .title("Error in search!")
                    .button("Continue", close_top_maybe_quit),
            )
        }
    };
    let dialog = OnEventView::new(
        Dialog::around(
            LinearLayout::horizontal()
                .child(PaddedView::lrtb(
                    1,
                    1,
                    2,
                    2,
                    EditView::new()
                        .content(query_text)
                        .on_submit(move |s, _| do_search(s))
                        .with_name(SEARCH_BOX)
                        .min_width(24),
                ))
                .child(Panel::new(
                    SelectView::new()
                        .with_all([("Text", "text"), ("Regex", "regex"), ("Hexagex", "hexagex")])
                        .selected(query_kind)
                        .with_name(SEARCH_MODE),
                )),
        )
        .title("Search")
        .button("Search", do_search)
        .button("Cancel", close_top_maybe_quit)
        .button("Help", help_window(SEARCH_HELP))
        .with_name(SEARCH_DIALOG),
    )
    .on_event(Key::F1, help_window(SEARCH_HELP));
    siv.add_layer(dialog)
}

const SEARCH_BUFFER_SIZE: usize = 1000000;

/// Action to execute when submitting a search
fn on_search(siv: &mut Cursive) -> Result<(), String> {
    let content = siv
        .call_on_name(SEARCH_BOX, |view: &mut EditView| {
            view.get_content().as_ref().clone()
        })
        .unwrap();
    if content.is_empty() {
        on_hexview(siv, Aligned::clear_search, Unaligned::clear_search);
        close_top_maybe_quit(siv);
        return Ok(());
    }
    let search_mode = siv
        .call_on_name(SEARCH_MODE, |view: &mut SelectView<&str>| {
            view.selection()
                .ok_or_else(|| String::from("No search mode selected!"))
        })
        .unwrap()?;
    // restore the dialog fields from the saved query
    let query_type = match *search_mode.as_ref() {
        "text" => QueryType::Text,
        "regex" => QueryType::Regex,
        "hexagex" => QueryType::Hexagex,
        otherwise => return Err(format!("Invaild search mode: {otherwise}")),
    };
    let query = Query::new(query_type, &content).map_err(|e| e.to_string())?;
    let q1 = query.clone();
    let ((context1, file1), second) = on_hexview(
        siv,
        move |v| v.setup_search(q1),
        move |v| v.setup_search(query),
    );
    // close the search dialog and show a status window showing progress
    // of the current search
    siv.pop_layer();
    search_result_status(siv, 1 + second.is_some() as usize);

    let start_search = |context: SearchContext, content: FileContent| {
        let sink = siv.cb_sink().clone();
        let send = util::rate_limit_channel(
            SEARCH_BUFFER_SIZE,
            Duration::from_millis(200),
            search_result_receiver(sink, context.clone()),
        );
        context.start_search(send, content)
    };
    start_search(context1, file1);
    if let Some((context2, file2)) = second {
        start_search(context2, file2)
    }
    Ok(())
}

const SEARCH_STATS: &str = "search stats";
/// view that shows the progress of the current search
struct SearchResultStats {
    view: BoxedView,
    /// the total count of search results
    count: usize,
    /// the count is to track when to close the window,
    /// as there are two search processes for both halves
    usage_count: usize,
    /// "Results: {self.count}..."
    text: TextContent,
}

impl ViewWrapper for SearchResultStats {
    wrap_impl!(self.view: BoxedView);
}

impl SearchResultStats {
    fn update_count(&mut self, diff: usize) {
        self.count += diff;
        self.text.set_content(format!("Results: {}...", self.count));
    }
}

/// create a new view showing the search progress
fn search_result_status(siv: &mut Cursive, usage_count: usize) {
    let count = 0;
    let content = TextContent::new("Results: 0...");
    let view = Dialog::around(TextView::new_with_content(content.clone()))
        .button("Cancel", close_top_maybe_quit);
    let search_result_stats = SearchResultStats {
        view: BoxedView::new(Box::new(view)),
        count,
        usage_count,
        text: content,
    };
    siv.add_layer(search_result_stats.with_name(SEARCH_STATS))
}

/// creates an adapter to put the search results into
fn search_result_receiver(
    cb: cursive::CbSink,
    context: SearchContext,
) -> impl FnMut(Vec<Option<Range<usize>>>) -> bool + Send + 'static {
    move |v| {
        let context = context.clone();
        cb.send(Box::new(move |siv| add_search_results(siv, v, context)))
            .is_ok()
    }
}

/// use a list of search results to update the progress window and
/// maybe close it and jump the hexview to the next result when
/// finished.
fn add_search_results(
    siv: &mut Cursive,
    results: Vec<Option<Range<usize>>>,
    context: SearchContext,
) {
    let count = results.iter().flatten().count();
    let is_final = results.is_final();
    let SearchContext {
        query,
        first,
        is_running,
    } = context;
    let q1 = query.clone();
    let r1 = results.clone();
    on_hexview(
        siv,
        move |v| v.add_search_results(q1, results, first),
        move |v| v.add_search_results(query, r1, first),
    );
    match siv.call_on_name(SEARCH_STATS, |view: &mut SearchResultStats| {
        view.update_count(count);
        if is_final {
            view.usage_count -= 1;
        }
        view.usage_count == 0
    }) {
        Some(true) => {
            on_hexview(
                siv,
                |v| v.jump_next_search_result(&mut Dummy),
                |v| v.jump_next_search_result(&mut Dummy),
            );
            close_top_maybe_quit(siv)
        }
        None => {
            is_running.store(false, std::sync::atomic::Ordering::Relaxed);
        }
        Some(false) => (),
    };
}

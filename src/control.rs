use cursive::{
    backend::Backend as CursiveBackend,
    backends::crossterm,
    event::Key,
    traits::Nameable,
    view::ViewWrapper,
    views::{LayerPosition, NamedView},
    View,
};
use cursive::{traits::Resizable, views::ResizedView, Cursive};
use cursive_buffered_backend::BufferedBackend;
use std::{cell::Cell, rc::Rc, thread::scope};

use crate::{
    align::{AlignInfo, CheckStatus},
    backend::{send_cross_actions, Action, Cross, Dummy},
    config::{Config, Settings},
    dialog::{self, continue_dialog, error_window},
    doublehex::DoubleHexContext,
    file::{FileContent, FileState},
    view::{self, Aligned, AlignedMessage},
};
use std::{
    ops::Range,
    sync::mpsc::{channel, Receiver, Sender},
};

pub type WrappedEvent = Rc<Cell<DelegateEvent>>;
pub type CursiveCallback = Box<dyn Fn(&mut Cursive, WrappedEvent) + 'static + Send>;

/// This is the main loop, here we switch between our custom backend and the cursive backend
/// when opening dialog boxes. This is done because initially, the cursive backend was too flickery.
/// However, this was fixed by using cursive_buffered_backend, so now this is only a minor optimization.
pub fn run(x: FileState, y: FileState) {
    let mut settings = Config::from_config()
        .map(Config::into_current_version)
        .unwrap_or_default();
    let digits = x.content.address_digits().max(y.content.address_digits());
    settings.style.addr_width = digits;
    settings.load_memory_warn_status();
    let mut hv = HexView::new(x, y);
    let mut event = DelegateEvent::Continue;
    loop {
        *match hv {
            HexView::Aligned(ref mut v, _, _) => &mut v.dh.style,
            HexView::Unaligned(ref mut v) => &mut v.dh.style,
        } = settings.style;
        match event {
            DelegateEvent::Continue
            | DelegateEvent::SwitchToAlign
            | DelegateEvent::SwitchToUnalign
            | DelegateEvent::Reload => {
                let mut cross = Cross::init();
                (hv, event) = hv.process_cross(&mut cross, &settings, event);
                cross.uninit();
            }
            DelegateEvent::OpenDialog(dia) => {
                (hv, settings, event) = hv.show_dialog(dia, settings);
            }
            DelegateEvent::Quit => break,
        }
        // the column setting can be changed during the non-dialog,
        // so we need to keep it updated here
        settings.style = match &hv {
            HexView::Aligned(v, _, _) => v.dh.style,
            HexView::Unaligned(v) => v.dh.style,
        };
    }
}

/// An enum containing either an aligned or unaligned hexview, without
/// a backend for painting.
/// The aligned view also contains a channel for messages, as the alignment
/// algorithms need to dynamically append/prepend new blocks to the view
/// and the crossterm backend also sends user events over that.
pub enum HexView {
    Aligned(
        view::Aligned,
        Sender<AlignedMessage>,
        Receiver<AlignedMessage>,
    ),
    Unaligned(view::Unaligned),
}

impl HexView {
    /// Creates a new unaligned view from two files with given indexes and cursor
    /// size 16x16.
    pub fn new(left: FileState, right: FileState) -> Self {
        HexView::Unaligned(view::Unaligned::new(
            left,
            right,
            DoubleHexContext::new((16, 16)),
        ))
    }
    fn files(&self) -> [FileContent; 2] {
        match self {
            HexView::Aligned(a, _, _) => a.files(),
            HexView::Unaligned(u) => u.files(),
        }
    }
    /// Turns a hexview into an aligned view using the given algorithm parameters
    fn into_aligned(self, algo: &AlignInfo, select: [Option<Range<usize>>; 2]) -> HexView {
        let (send, recv) = channel();
        match match self {
            // first destruct our old hexview into its parts
            HexView::Aligned(a, send, recv) => {
                a.destruct().map_err(|a| HexView::Aligned(a, send, recv))
            }
            HexView::Unaligned(u) => u.destruct().map_err(HexView::Unaligned),
        } {
            // if the cursor was not placed on any index, we currently do nothing
            // maybe one could think up some better values to align at here or something
            Err(hv) => hv,
            Ok((left, right, dh)) => HexView::Aligned(
                view::Aligned::new(left, right, dh, algo, select, send.clone()),
                send,
                recv,
            ),
        }
    }
    /// Turns a hexview into an unaligned view at the current cursor
    fn into_unaligned(self) -> HexView {
        match self {
            HexView::Aligned(a, send, recv) => match a.destruct() {
                Ok((left, right, cursor)) => {
                    HexView::Unaligned(view::Unaligned::new(left, right, cursor))
                }
                Err(a) => HexView::Aligned(a, send, recv),
            },
            // we don't need to change anything for unaligned views
            HexView::Unaligned(_) => self,
        }
    }
    /// Call the relevant event processing functions for the crossterm backend
    fn event_proc(&mut self, cross: &mut Cross) -> DelegateEvent {
        match self {
            HexView::Aligned(ref mut a, ref mut send, ref mut recv) => {
                aligned_cross(a, cross, send, recv)
            }
            HexView::Unaligned(ref mut u) => unaligned_cross(u, cross),
        }
    }
    fn selection(&self) -> [Option<Range<usize>>; 2] {
        match self {
            HexView::Aligned(a, _, _) => a.selection_file_ranges(),
            HexView::Unaligned(u) => u.selection_file_ranges(),
        }
    }
    fn reload(&mut self, cross: &mut Cross) -> DelegateEvent {
        match self {
            HexView::Aligned(_, _, _) => {
                return DelegateEvent::OpenDialog(continue_dialog(error_window(String::from(
                    "Reloading files is only supported in the unaligned view",
                ))))
            }
            HexView::Unaligned(u) => {
                if let Err(e) = u.reload(cross) {
                    return DelegateEvent::OpenDialog(continue_dialog(error_window(format!(
                        "Error reloading files: {e}"
                    ))));
                }
            }
        }
        DelegateEvent::Continue
    }
    /// control loop for crossterm backend, switches the view between aligned and unaligned when
    /// requested and runs event loops
    fn process_cross(
        self,
        cross: &mut Cross,
        settings: &Settings,
        mut event: DelegateEvent,
    ) -> (Self, DelegateEvent) {
        let mut view = self;
        loop {
            view = match event {
                // delegate to top-level control loop
                DelegateEvent::Quit => {
                    event = if !match &mut view {
                        HexView::Aligned(v, _, _) => v.process_escape(cross),
                        HexView::Unaligned(v) => v.process_escape(cross),
                    } {
                        event
                    } else {
                        DelegateEvent::Continue
                    };
                    view
                }
                DelegateEvent::Reload => {
                    event = view.reload(cross);
                    view
                }
                DelegateEvent::SwitchToAlign => {
                    event = DelegateEvent::Continue;
                    let select = view.selection();
                    let align_info = &settings.presets.current_info();
                    view.into_aligned(align_info, select)
                }
                DelegateEvent::SwitchToUnalign => {
                    event = DelegateEvent::Continue;
                    view.into_unaligned()
                }
                DelegateEvent::Continue | DelegateEvent::OpenDialog(_) => view,
            };
            if !matches!(event, DelegateEvent::Continue) {
                break;
            }
            event = view.event_proc(cross);
            if matches!(event, DelegateEvent::SwitchToAlign) {
                let align_info = settings.presets.current_info();
                let selection = view.selection();
                match align_info.check_start_align(view.files(), selection) {
                    CheckStatus::Ok => {}
                    CheckStatus::MemoryWarning => {
                        if !settings.no_memory_warn {
                            event = DelegateEvent::OpenDialog(Box::new(dialog::memory_warning))
                        }
                    }
                    CheckStatus::Error(msg) => {
                        event =
                            DelegateEvent::OpenDialog(continue_dialog(dialog::error_window(msg)));
                    }
                }
            }
        }
        (view, event)
    }
    /// Setup a cursive instance and shows a dialog constructed through the callback given in `dialog`.
    ///
    /// Note that the settings are placed into the user_data of the cursive instace and can be modified
    /// by the callback.
    fn show_dialog(
        self,
        dialog: CursiveCallback,
        settings: Settings,
    ) -> (Self, Settings, DelegateEvent) {
        let mut siv = cursive::default();
        // this theme is the default theme except that the background color is black
        siv.set_theme(cursiv_theme());
        siv.add_global_callback(Key::Esc, dialog::close_top_maybe_quit);
        siv.set_user_data(settings);
        // placeholder value that will always be overwritten in the scope
        let event = Rc::new(Cell::new(DelegateEvent::Continue));
        match self {
            HexView::Aligned(a, send, mut recv) => {
                siv.add_fullscreen_layer(a.with_name("aligned").full_screen());
                let mut sink = siv.cb_sink().clone();
                // we create a new thread that converts the `AlignedMessage`s coming from
                // the alignment threads to callbacks on the cursive instance, so this case
                // is a bit more complicated than the unaligned one.
                scope(|s| {
                    let join_handle = s.spawn(|| cursiv_align_relay(&mut recv, &mut sink));

                    dialog(&mut siv, event.clone());
                    siv.try_run_with(|| {
                        // use the buffered backend as it involves way less flickering
                        crossterm::Backend::init()
                            .map(|x| Box::new(BufferedBackend::new(x)) as Box<dyn CursiveBackend>)
                    })
                    .expect("Could not run");
                    // misuse the Action::Quit as a signal for the thread to exit
                    send.send(AlignedMessage::UserEvent(Action::Quit))
                        .expect("Could not tell align relay thread to quit");
                    join_handle
                        .join()
                        .expect("Could not join align relay thread");
                });
                // extract the view from the cursive instance
                match peel_onion(&mut siv) {
                    Some(x) => (
                        HexView::Aligned(x, send, recv),
                        siv.take_user_data().unwrap(),
                        event.take(),
                    ),
                    None => panic!("Internal error, could not downcast view"),
                }
            }
            HexView::Unaligned(u) => {
                siv.add_fullscreen_layer(u.with_name("unaligned").full_screen());
                dialog(&mut siv, event.clone());
                siv.try_run_with(|| {
                    crossterm::Backend::init()
                        .map(|x| Box::new(BufferedBackend::new(x)) as Box<dyn CursiveBackend>)
                })
                .expect("Could not run");
                // extract the view from the cursive instance
                match peel_onion(&mut siv) {
                    Some(v) => (
                        HexView::Unaligned(v),
                        siv.take_user_data().unwrap(),
                        event.take(),
                    ),
                    None => panic!("Internal error, could not downcast view"),
                }
            }
        }
    }
}

// this one causes tears to come from my eyes
fn peel_onion<V: View>(siv: &mut Cursive) -> Option<V> {
    siv.screen_mut()
        .remove_layer(LayerPosition::FromBack(0))
        .downcast::<ResizedView<NamedView<V>>>()
        .ok()
        .and_then(|view| view.into_inner().ok())
        .and_then(|view| view.into_inner().ok())
}

/// Default Cursive theme except that the background color is black
fn cursiv_theme() -> cursive::theme::Theme {
    use cursive::theme::{BaseColor::*, Color::*, PaletteColor::*};
    let mut cursiv_theme = cursive::theme::load_default();
    cursiv_theme.palette[Background] = Dark(Black);
    cursiv_theme
}

/// Forwards `AlignedMessage`s from the alignment thread into callbacks for the cursive instance
fn cursiv_align_relay(recv: &mut Receiver<AlignedMessage>, sink: &mut cursive::CbSink) {
    for ev in recv.iter() {
        match ev {
            AlignedMessage::UserEvent(Action::Quit) => break,
            otherwise => {
                sink.send(Box::new(|siv: &mut Cursive| {
                    siv.call_on_name("aligned", |view: &mut Aligned| {
                        view.process_action(&mut Dummy, otherwise);
                    })
                    .expect("Could not send new data to view");
                }))
                .expect("Could not send event to view");
            }
        }
    }
}

/// This enum is used for delegating actions to higher level event loops.
#[derive(Default)]
pub enum DelegateEvent {
    #[default]
    Continue,
    Quit,
    SwitchToAlign,
    SwitchToUnalign,
    OpenDialog(CursiveCallback),
    Reload,
}

/// Converts an event to a delegation
fn delegate_action(action: Action) -> Option<DelegateEvent> {
    match action {
        Action::Quit => Some(DelegateEvent::Quit),
        Action::Align => Some(DelegateEvent::SwitchToAlign),
        Action::Unalign => Some(DelegateEvent::SwitchToUnalign),
        Action::Algorithm => Some(DelegateEvent::OpenDialog(continue_dialog(dialog::settings))),
        Action::Goto => Some(DelegateEvent::OpenDialog(continue_dialog(dialog::goto))),
        Action::Search => Some(DelegateEvent::OpenDialog(continue_dialog(dialog::search))),
        Action::SetOffset => Some(DelegateEvent::OpenDialog(continue_dialog(
            dialog::set_offset,
        ))),
        Action::Help => Some(DelegateEvent::OpenDialog(continue_dialog(
            dialog::help_window(dialog::MAIN_HELP),
        ))),
        Action::Refresh => Some(DelegateEvent::Reload),
        _otherwise => None,
    }
}

/// This function is the one that processes actions sent by the event reader loop
/// setup in `unaligned_cross`. Note that the event reader loop has to stay in the same
/// thread, so this process is chosen to not be in the main thread instead.
fn unaligned_cross_recv(
    unaligned: &mut view::Unaligned,
    cross: &mut Cross,
    recv: Receiver<Action>,
) -> DelegateEvent {
    unaligned.refresh(cross);
    for action in recv.iter() {
        if let Some(q) = delegate_action(action) {
            return q;
        }
        let delegate = unaligned.process_action(cross, action);
        if !matches!(delegate, DelegateEvent::Continue) {
            return delegate;
        }
    }
    DelegateEvent::Quit
}

/// This setups the event processing thread for the crossterm backend and reads crossterm's events
fn unaligned_cross(unaligned: &mut view::Unaligned, cross: &mut Cross) -> DelegateEvent {
    unaligned.refresh(cross);
    let (mut send, recv) = channel();
    let mut quit = DelegateEvent::Quit;
    scope(|s| {
        // both this thread and the send_cross_actions function determine when to quit by
        // checking the output of delegate_action, so make sure this is the same
        let receiver_thread = s.spawn(|| unaligned_cross_recv(unaligned, cross, recv));
        send_cross_actions(|action| delegate_action(action).is_some(), &mut send);
        quit = receiver_thread.join().unwrap();
    });
    quit
}

/// This function is the one that processes actions sent by the event reader loop
/// setup in `aligned_cross`, and also the ones sent by the alignment process.
/// Note that the event reader loop has to stay in the same thread, so this
/// process is chosen to not be in the main thread instead.
fn aligned_cross_recv(
    aligned: &mut view::Aligned,
    cross: &mut Cross,
    recv: &mut Receiver<AlignedMessage>,
) -> DelegateEvent {
    for msg in recv.iter() {
        let msg = match msg {
            AlignedMessage::UserEvent(action) => {
                if let Some(q) = delegate_action(action) {
                    return q;
                }
                msg
            }
            _ => msg,
        };
        aligned.process_action(cross, msg);
    }
    DelegateEvent::Quit
}

/// Using the existing message channel (send, recv), setup a thread that
/// processes the messages and also read the crossterm events in the main thread.
/// The channel should be the same one used when setting up the Aligned view.
fn aligned_cross(
    aligned: &mut view::Aligned,
    cross: &mut Cross,
    send: &mut Sender<AlignedMessage>,
    recv: &mut Receiver<AlignedMessage>,
) -> DelegateEvent {
    aligned.refresh(cross);
    let mut quit = DelegateEvent::Quit;
    scope(|s| {
        // both the thread and the send_cross_actions function determine when to quit by
        // checking the output of delegate_action, so make sure this is the same.
        let receiver_thread = s.spawn(|| aligned_cross_recv(aligned, cross, recv));
        send_cross_actions(|action| delegate_action(action).is_some(), send);
        quit = receiver_thread.join().unwrap();
    });
    quit
}

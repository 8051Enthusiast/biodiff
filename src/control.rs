use crossbeam_utils::thread::scope;
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
use dirs::config_dir;
use serde::{Deserialize, Serialize};

use crate::{
    align::{AlignAlgorithm, AlignMode},
    backend::{send_cross_actions, Action, Cross, Dummy},
    cursor::CursorState,
    dialog,
    doublehex::DoubleHexContext,
    file::FileState,
    style::Style,
    view::{self, Aligned, AlignedMessage},
};
use std::{
    error::Error,
    fs::read_to_string,
    ops::Range,
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
};

type CursiveCallback = Box<dyn Fn(&mut Cursive) + 'static + Send>;

/// This is the main loop, here we switch between our custom backend and the cursive backend
/// when opening dialog boxes. This is done because initially, the cursive backend was too flickery.
/// However, this was fixed by using cursive_buffered_backend, so now this is only a minor optimization.
pub fn run(x: FileState, y: FileState) {
    let mut settings = Settings::from_config().unwrap_or_default();
    let digits = x.address_digits().max(y.address_digits());
    settings.style.addr_width = digits;
    let mut hv = HexView::new(x, y);
    loop {
        *match hv {
            HexView::Aligned(ref mut v, _, _) => &mut v.dh.style,
            HexView::Unaligned(ref mut v) => &mut v.dh.style,
        } = settings.style;
        let mut cross = Cross::init();
        let (hv_new, quit) = hv.process_cross(&mut cross, &settings);
        hv = hv_new;
        cross.uninit();
        // the column setting can be changed during the non-dialog,
        // so we need to keep it updated here
        settings.style = match &hv {
            HexView::Aligned(v, _, _) => v.dh.style,
            HexView::Unaligned(v) => v.dh.style,
        };
        let (hv_new, settings_new) = match quit {
            DelegateEvent::Quit => break,
            DelegateEvent::OpenDialog(dia) => hv.show_dialog(dia, settings),
            _ => (hv, settings),
        };
        hv = hv_new;
        settings = settings_new;
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Settings {
    pub algo: AlignAlgorithm,
    pub style: Style,
}

impl Settings {
    fn config_path() -> Result<PathBuf, std::io::Error> {
        match std::env::var_os("BIODIFF_CONFIG_DIR") {
            Some(p) => Ok(PathBuf::from(p)),
            None => match config_dir() {
                Some(mut p) => {
                    p.push("biodiff");
                    Ok(p)
                }
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Could not find configuration directory",
                )),
            },
        }
    }
    fn settings_file() -> Result<PathBuf, std::io::Error> {
        let mut path = Self::config_path()?;
        path.push("config.json");
        Ok(path)
    }
    pub fn from_config() -> Option<Self> {
        let config = read_to_string(Self::settings_file().ok()?).ok()?;
        serde_json::from_str(&config).ok()
    }

    pub fn save_config(&self) -> Result<(), Box<dyn Error + 'static>> {
        let config = serde_json::to_string(self)?;
        let r = std::fs::create_dir_all(Self::config_path()?);
        if let Err(ref e) = r {
            match e.kind() {
                std::io::ErrorKind::AlreadyExists => (),
                _ => r?,
            }
        }
        std::fs::write(Self::settings_file()?, config)?;
        Ok(())
    }
}

/// An enum containing either an aligned or unaligned hexview, without
/// a backend for painting.
/// The aligned view also contains a channel for messages, as the alignment
/// algorithms need to dynamically append/prepend new blocks to the view
/// and the crossbeam backend also sends user events over that.
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
    /// Turns a hexview into an aligned view using the given algorithm parameters
    fn into_aligned(self, algo: &AlignAlgorithm, select: [Option<Range<usize>>; 2]) -> HexView {
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
            Ok((left, right, mut dh)) => {
                if matches!(algo.mode, AlignMode::Local | AlignMode::Global) {
                    dh.cursor = CursorState::new((dh.cursor.get_size_x(), dh.cursor.get_size_y()))
                };
                HexView::Aligned(
                    view::Aligned::new(left, right, dh, algo, select, send.clone()),
                    send,
                    recv,
                )
            }
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
    /// control loop for crossbeam backend, switches the view between aligned and unaligned when
    /// requested and runs event loops
    fn process_cross(self, cross: &mut Cross, settings: &Settings) -> (Self, DelegateEvent) {
        let mut view = self;
        let mut quit;
        let quit_reason = loop {
            let q = view.event_proc(cross);
            view = match q {
                // delegate to top-level control loop
                DelegateEvent::Quit | DelegateEvent::OpenDialog(_) => {
                    quit = match &mut view {
                        HexView::Aligned(v, _, _) => !v.process_escape(cross),
                        HexView::Unaligned(v) => !v.process_escape(cross),
                    }
                    .then_some(q);
                    view
                }
                DelegateEvent::SwitchToAlign => {
                    quit = None;
                    let select = view.selection();
                    view.into_aligned(&settings.algo, select)
                }
                DelegateEvent::SwitchToUnalign => {
                    quit = None;
                    view.into_unaligned()
                }
            };
            if let Some(q) = quit {
                break q;
            }
        };
        (view, quit_reason)
    }
    /// Setup a cursive instance and shows a dialog constructed through the callback given in `dialog`.
    ///
    /// Note that the settings are placed into the user_data of the cursive instace and can be modified
    /// by the callback.
    fn show_dialog(self, dialog: CursiveCallback, settings: Settings) -> (Self, Settings) {
        let mut siv = cursive::default();
        // this theme is the default theme except that the background color is black
        siv.set_theme(cursiv_theme());
        siv.add_global_callback(Key::Esc, dialog::close_top_maybe_quit);
        siv.set_user_data(settings);
        match self {
            HexView::Aligned(a, send, mut recv) => {
                siv.add_fullscreen_layer(a.with_name("aligned").full_screen());
                let mut sink = siv.cb_sink().clone();
                // we create a new thread that converts the `AlignedMessage`s coming from
                // the alignment threads to callbacks on the cursive instance, so this case
                // is a bit more complicated than the unaligned one.
                scope(|s| {
                    let join_handle = s.spawn(|_| cursiv_align_relay(&mut recv, &mut sink));
                    dialog(&mut siv);
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
                })
                .expect("Could not join align relay thread");
                // extract the view from the cursive instance
                match peel_onion(&mut siv) {
                    Some(x) => (
                        HexView::Aligned(x, send, recv),
                        siv.take_user_data().unwrap(),
                    ),
                    None => panic!("Internal error, could not downcast view"),
                }
            }
            HexView::Unaligned(u) => {
                siv.add_fullscreen_layer(u.with_name("unaligned").full_screen());
                dialog(&mut siv);
                siv.try_run_with(|| {
                    crossterm::Backend::init()
                        .map(|x| Box::new(BufferedBackend::new(x)) as Box<dyn CursiveBackend>)
                })
                .expect("Could not run");
                // extract the view from the cursive instance
                match peel_onion(&mut siv) {
                    Some(v) => (HexView::Unaligned(v), siv.take_user_data().unwrap()),
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
enum DelegateEvent {
    Quit,
    SwitchToAlign,
    SwitchToUnalign,
    OpenDialog(CursiveCallback),
}

/// Converts an event to a delegation
fn delegate_action(action: Action) -> Option<DelegateEvent> {
    match action {
        Action::Quit => Some(DelegateEvent::Quit),
        Action::Align => Some(DelegateEvent::SwitchToAlign),
        Action::Unalign => Some(DelegateEvent::SwitchToUnalign),
        Action::Algorithm => Some(DelegateEvent::OpenDialog(Box::new(dialog::settings))),
        Action::Goto => Some(DelegateEvent::OpenDialog(Box::new(dialog::goto))),
        Action::Search => Some(DelegateEvent::OpenDialog(Box::new(dialog::search))),
        Action::SetOffset => Some(DelegateEvent::OpenDialog(Box::new(dialog::set_offset))),
        Action::Help => Some(DelegateEvent::OpenDialog(Box::new(dialog::help_window(
            dialog::MAIN_HELP,
        )))),
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
        unaligned.process_action(cross, action);
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
        let receiver_thread = s.spawn(|_| unaligned_cross_recv(unaligned, cross, recv));
        send_cross_actions(|action| delegate_action(action).is_some(), &mut send);
        quit = receiver_thread.join().unwrap();
    })
    .unwrap();
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
        let receiver_thread = s.spawn(|_| aligned_cross_recv(aligned, cross, recv));
        send_cross_actions(|action| delegate_action(action).is_some(), send);
        quit = receiver_thread.join().unwrap();
    })
    .unwrap();
    quit
}

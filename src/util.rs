use std::{
    sync::mpsc::{sync_channel, Receiver},
    time::{Duration, Instant},
};

struct RateLimitedChannel<T: Finalable + Send, F: FnMut(Vec<T>) -> bool + Send + 'static> {
    input: Receiver<Option<T>>,
    receiver: F,
    size: usize,
    refresh_period: Duration,
    last_refresh: Option<Instant>,
    element_buffer: Vec<T>,
}

impl<T: Finalable + Send + 'static, F: FnMut(Vec<T>) -> bool + Send + 'static>
    RateLimitedChannel<T, F>
{
    fn spawn(mut self) {
        std::thread::spawn(move || {
            let period = self.refresh_period;
            for i in self.input {
                if self.element_buffer.len() >= self.size {
                    if !(self.receiver)(self.element_buffer) {
                        break;
                    }
                    self.last_refresh = Some(Instant::now());
                    self.element_buffer = vec![];
                }
                if let Some(x) = i {
                    self.element_buffer.push(x);
                }
                let is_final = self.element_buffer.is_final();
                let do_refresh = self
                    .last_refresh
                    .map(|x| Instant::now() - x >= period || is_final)
                    .unwrap_or(true);
                if !do_refresh || self.element_buffer.is_empty() {
                    continue;
                }
                if !(self.receiver)(self.element_buffer) {
                    break;
                }
                self.last_refresh = Some(Instant::now());
                self.element_buffer = vec![];
                if is_final {
                    break;
                }
            }
        });
    }
}

pub trait Finalable {
    fn is_final(&self) -> bool;
}

impl<T> Finalable for Option<T> {
    fn is_final(&self) -> bool {
        self.is_none()
    }
}

impl<T> Finalable for Vec<T>
where
    T: Finalable,
{
    fn is_final(&self) -> bool {
        self.last().map(|x| x.is_final()).unwrap_or(false)
    }
}

pub fn rate_limit_channel<T: Finalable + Send + 'static>(
    size: usize,
    period: Duration,
    receiver: impl FnMut(Vec<T>) -> bool + Send + 'static,
) -> impl FnMut(T) -> bool + Send + 'static {
    let (input_channel_send, input_channel_recv) = sync_channel::<Option<T>>(2);
    let timer_send = input_channel_send.clone();
    let timer = move || loop {
        std::thread::sleep(period);
        if timer_send.send(None).is_err() {
            break;
        }
    };
    let relay = RateLimitedChannel {
        input: input_channel_recv,
        receiver,
        size,
        refresh_period: period,
        last_refresh: None,
        element_buffer: vec![],
    };
    relay.spawn();
    std::thread::spawn(timer);
    move |t| input_channel_send.send(Some(t)).is_ok()
}

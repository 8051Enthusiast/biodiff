use std::{
    sync::mpsc::{sync_channel, Receiver},
    time::{Duration, Instant},
};

use realfft::{num_complex::Complex64, RealFftPlanner};

/// a channel that bunches up data into vectors if there is too much data at once
/// (to avoid too many callbacks to cursive)
/// used by the rate_limit_channel function
struct RateLimitedChannel<T: Finalable + Send, F: FnMut(Vec<T>) -> bool + Send + 'static> {
    /// the receiver of the input
    input: Receiver<Option<T>>,
    /// the function where batched up data is sent to
    receiver: F,
    /// maximum size of a batch
    size: usize,
    /// how long to wait until the batch is sent if it did not fill up
    refresh_period: Duration,
    /// last time a batch was sent
    last_refresh: Option<Instant>,
    /// buffer for current batch
    element_buffer: Vec<T>,
}

impl<T: Finalable + Send + 'static, F: FnMut(Vec<T>) -> bool + Send + 'static>
    RateLimitedChannel<T, F>
{
    fn spawn(mut self) {
        std::thread::spawn(move || {
            let period = self.refresh_period;
            for i in self.input {
                // if we reached our buffer size, we clear it
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

/// trait for types which can have a final value that is sent last
/// for the purpose of hanging up the RateLimitedChannel since there
/// is a timer thread that would not get deleted otherwise
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

/// construct a rate limited channel of maximum buffer size `size`, refresh period `period`
/// and a receiver where data is pushed to, that returns a function that accepts data
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

/// returns the entropy of a blob of data
pub fn entropy(data: &[u8]) -> f32 {
    let mut counts = vec![0usize; 256];
    for byte in data {
        counts[usize::from(*byte)] += 1;
    }
    counts
        .into_iter()
        .map(|x| {
            if x == 0 {
                return 0.0f32;
            };
            let freq = x as f32 / data.len() as f32;
            -freq * freq.log2() / 8.0
        })
        .sum::<f32>()
}

pub fn autocorrelation(data: &[u8]) -> Vec<f64> {
    let padded_len = data.len() * 2;
    let avg = data.iter().copied().map(u64::from).sum::<u64>() as f64 / data.len() as f64;
    let mut a = vec![0.0f64; padded_len];
    let mut a_out = vec![Complex64::new(0.0, 0.0); padded_len / 2 + 1];
    let mut fft_planner = RealFftPlanner::new();
    let fft_forward = fft_planner.plan_fft_forward(padded_len);
    let fft_inverse = fft_planner.plan_fft_inverse(padded_len);
    for (i, x) in data.iter().enumerate() {
        a[i] = *x as f64 - avg;
    }
    let square_sum = a.iter().map(|x| x * x).sum::<f64>();
    if square_sum == 0.0 {
        return vec![0.0f64; padded_len];
    }
    fft_forward.process(&mut a, &mut a_out).unwrap();
    for x in a_out.iter_mut() {
        *x = *x * x.conj() / padded_len as f64 / square_sum;
    }
    fft_inverse.process(&mut a_out, &mut a).unwrap();
    a.truncate(data.len());
    a
}

#[cfg(test)]
mod tests {
    use super::entropy;
    #[test]
    fn ent() {
        let all = (0..=255u8).collect::<Vec<u8>>();
        assert!((entropy(&all) - 1.0).abs() < 0.001);
        let none = vec![0u8; 256];
        assert!(entropy(&none).abs() < 0.001);
    }
}

use std::{
    sync::{
        atomic::{AtomicBool, AtomicU16, Ordering},
        mpsc::SyncSender,
        Arc,
    },
    thread::available_parallelism,
};

use realfft::{num_complex::Complex64, RealFftPlanner, RealToComplex};

pub use biodiff_align::rustbio::Banded;

use crate::file::FileContent;

pub enum FlatAlignProgressMessage {
    Incomplete(u16),
    Complete(isize),
}
pub struct FlatAlignmentContext {
    is_running: Arc<AtomicBool>,
    vecs: [FileContent; 2],
    update_progress: Box<dyn FnMut(FlatAlignProgressMessage) + Send + 'static>,
}

impl FlatAlignmentContext {
    pub fn new(
        is_running: Arc<AtomicBool>,
        vecs: [FileContent; 2],
        update_progress: Box<dyn FnMut(FlatAlignProgressMessage) + Send + 'static>,
    ) -> Self {
        Self {
            is_running,
            vecs,
            update_progress,
        }
    }

    // this finds the alignment between two arrays *without* removing elements such that
    // fewest bytes are different (for the compvec)
    pub fn align_flat(mut self) {
        // this algorithm works by, for each byte:
        // * making an indicator vector for both files indicating the addresses that have the given byte
        // * cross-correlating them, which results in the number of matches of that byte value for each relative offset
        // and then adding them all together to get the total number of matching bytes
        let mut progress = 0u16;
        let current_byte = Arc::new(AtomicU16::new(0));
        let mut fft_planner = RealFftPlanner::new();
        let total_len = self.vecs.iter().map(|x| x.len()).max().unwrap() * 2;
        // the cross correlation is done using the omnipresent fft algorithm
        let fft_forward = fft_planner.plan_fft_forward(total_len);
        let fft_inverse = fft_planner.plan_fft_inverse(total_len);
        let mut sum = fft_forward.make_output_vec();
        // this is easily parallelizable for up to 256 threads, for which we span a thread pool
        let thread_num = available_parallelism().map(usize::from).unwrap_or(1);
        let (send, recv) = std::sync::mpsc::sync_channel::<Vec<Complex64>>(4.max(thread_num));
        for _ in 0..thread_num {
            let vecs = [self.vecs[0].clone(), self.vecs[1].clone()];
            let inbyte = current_byte.clone();
            let outvecs = send.clone();
            let fft = fft_forward.clone();
            std::thread::spawn(move || correlation_thread(vecs, inbyte, outvecs, fft));
        }
        for vec in recv.into_iter().take(256) {
            if !self.is_running.load(Ordering::Relaxed) {
                return;
            }
            // add the vectors together in the frequency domain
            for (a, b) in sum.iter_mut().zip(vec.into_iter()) {
                *a += b;
            }
            progress += 1;
            (self.update_progress)(FlatAlignProgressMessage::Incomplete(progress));
        }
        // get the actual result in the time domain
        let mut result = fft_inverse.make_output_vec();
        fft_inverse
            .process(&mut sum, &mut result)
            .expect("Wrong lengths");
        drop(sum);
        // positive offset of the array with the highest value of overlap
        let offset = result
            .iter()
            .enumerate()
            .max_by(|a, b| {
                a.1.partial_cmp(b.1).unwrap_or_else(|| {
                    if a.1.is_nan() {
                        std::cmp::Ordering::Less
                    } else {
                        std::cmp::Ordering::Greater
                    }
                })
            })
            .unwrap_or((0, &0.0))
            .0;
        drop(result);
        // reverse direction of result array
        let offset = total_len - offset - 1;
        // get the relative offset between the two vectors with optimal overlap
        let relative_offset = if offset >= total_len / 2 {
            offset as isize - total_len as isize
        } else {
            offset as isize
        };
        (self.update_progress)(FlatAlignProgressMessage::Complete(relative_offset))
    }
}

fn correlation_thread(
    vecs: [FileContent; 2],
    inbyte: Arc<AtomicU16>,
    outvecs: SyncSender<Vec<Complex64>>,
    fft: Arc<dyn RealToComplex<f64>>,
) {
    let len = fft.len();
    loop {
        // check if the next value in queue is still below 256
        let byte: u8 = match inbyte.fetch_add(1, Ordering::Relaxed).try_into() {
            Ok(f) => f,
            Err(_) => return,
        };

        // cross-correlation using ffts
        let mut first_out = fft.make_output_vec();
        let mut first = fft.make_input_vec();
        // one of the vectors is reversed because we want correlation, not convolution
        for (i, x) in vecs[0].iter().enumerate() {
            if *x == byte {
                first[len - i - 1] = 1.0;
            }
        }
        fft.process(&mut first, &mut first_out)
            .expect("Wrong fft vector lengths");
        // these vectors can be large, so drop them as soon as possible
        drop(first);
        let mut second = fft.make_input_vec();
        for (i, x) in vecs[1].iter().enumerate() {
            if *x == byte {
                second[i] = 1.0
            }
        }
        let mut second_out = fft.make_output_vec();
        fft.process(&mut second, &mut second_out)
            .expect("Wrong fft vector lengths");
        drop(second);
        for (a, b) in first_out.iter_mut().zip(second_out.iter()) {
            *a *= b;
        }
        drop(second_out);
        // note: we do not correlate fully, since we can add all the samples together
        // in the frequency domain, saving nearly 1/3 of the processing time
        if outvecs.send(first_out).is_err() {
            return;
        }
    }
}

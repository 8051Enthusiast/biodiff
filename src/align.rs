use std::sync::{
    atomic::{AtomicBool, AtomicU16, Ordering},
    mpsc::{Sender, SyncSender},
    Arc,
};

use crate::{file::FileContent, view::AlignedMessage};
use bio::alignment::{
    pairwise::{self, MatchFunc, Scoring},
    AlignmentOperation as Op,
};
use realfft::{num_complex::Complex32, RealFftPlanner, RealToComplex};
use serde::{Deserialize, Serialize};

pub const DEFAULT_BLOCKSIZE: usize = 8192;
pub const DEFAULT_KMER: usize = 8;
pub const DEFAULT_WINDOW: usize = 6;

/// An align mode, can be either Local for local alignment, global for global alignment,
/// or Blockwise with a given block size. The blockwise mode starts from a given position
/// and aligns only using `blocksize` bytes from each sequence in one direction, which
/// makes it works fast and local, but it doesn't see bigger gaps and everything after big gaps
/// tends to be unaligned.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AlignMode {
    Local,
    Global,
    Blockwise(usize),
}

/// Determines whether to use the banded variant of the algorithm with given k-mer length
/// and window size
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Banded {
    Normal,
    Banded { kmer: usize, window: usize },
}
impl Banded {
    fn align<F: MatchFunc>(&self, scoring: Scoring<F>, x: &[u8], y: &[u8]) -> Vec<Op> {
        if x == y {
            return vec![Op::Match; x.len()];
        }
        // note that we recreate the Aligner each call, but i don't think that part is expensive
        match self {
            Banded::Normal => pairwise::Aligner::with_scoring(scoring).custom(x, y),
            Banded::Banded { kmer, window } => {
                pairwise::banded::Aligner::with_scoring(scoring, *kmer, *window).custom(x, y)
            }
        }
        .operations
    }
}

/// Contains parameters to run the alignment algorithm with
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct AlignAlgorithm {
    pub gap_open: i32,
    pub gap_extend: i32,
    pub mismatch_score: i32,
    pub match_score: i32,
    pub mode: AlignMode,
    pub band: Banded,
}

impl Default for AlignAlgorithm {
    fn default() -> Self {
        AlignAlgorithm {
            gap_open: -5,
            gap_extend: -1,
            mismatch_score: -1,
            match_score: 1,
            mode: AlignMode::Blockwise(DEFAULT_BLOCKSIZE),
            band: Banded::Normal,
        }
    }
}

impl AlignAlgorithm {
    /// This function starts the threads for the alignment, which send the data over the sender.
    /// It should then immediately return.
    pub fn start_align(
        &self,
        x: FileContent,
        y: FileContent,
        addr: (usize, usize),
        sender: Sender<AlignedMessage>,
    ) {
        let match_score = self.match_score;
        let mismatch_score = self.mismatch_score;
        let score = move |a: u8, b: u8| {
            if a == b {
                match_score
            } else {
                mismatch_score
            }
        };
        let scorer = pairwise::Scoring::new(self.gap_open, self.gap_extend, score);
        let band = self.band.clone();
        match self.mode {
            AlignMode::Local => {
                // we only need one thread
                std::thread::spawn(move || align_whole(x, y, scorer, band, sender));
            }
            AlignMode::Global => {
                // make xclip and yclip essentially infinitely expensive so that we have global alignment
                let global_scorer = scorer.xclip(pairwise::MIN_SCORE).yclip(pairwise::MIN_SCORE);
                std::thread::spawn(move || align_whole(x, y, global_scorer, band, sender));
            }
            AlignMode::Blockwise(blocksize) => {
                // for Blockwise, we need one thread for each direction from the cursor
                // Clone the data for the second thread here
                let x_cp = x.clone();
                let y_cp = y.clone();
                let scorer_cp = scorer.clone();
                let band_cp = band.clone();
                let sender_cp = sender.clone();
                std::thread::spawn(move || align_end(x, y, addr, scorer, band, blocksize, sender));
                std::thread::spawn(move || {
                    align_front(x_cp, y_cp, addr, scorer_cp, band_cp, blocksize, sender_cp)
                });
            }
        }
    }
}

/// Representation of the alignment that saves the original addresses of the bytes.
/// This has some space overhead, but alignment is slow enough for that not to matter in most cases.
#[derive(Clone, Copy, Debug)]
pub struct AlignElement {
    pub xaddr: usize,
    pub xbyte: Option<u8>,
    pub yaddr: usize,
    pub ybyte: Option<u8>,
}

impl AlignElement {
    /// Creates a vector out of `AlignElement`s from the operations outputted by rust-bio.
    /// Also outputs the addresses at the end of the array.
    fn from_array(
        r: &[Op],
        x: &[u8],
        y: &[u8],
        mut xaddr: usize,
        mut yaddr: usize,
    ) -> (Vec<AlignElement>, usize, usize) {
        let mut v = Vec::new();
        for op in r {
            match op {
                Op::Match | Op::Subst => {
                    v.push(AlignElement {
                        xaddr,
                        xbyte: Some(x[xaddr]),
                        yaddr,
                        ybyte: Some(y[yaddr]),
                    });
                    xaddr += 1;
                    yaddr += 1;
                }
                Op::Ins => {
                    v.push(AlignElement {
                        xaddr,
                        xbyte: Some(x[xaddr]),
                        yaddr,
                        ybyte: None,
                    });
                    xaddr += 1;
                }
                Op::Del => {
                    v.push(AlignElement {
                        xaddr,
                        xbyte: None,
                        yaddr,
                        ybyte: Some(y[yaddr]),
                    });
                    yaddr += 1;
                }
                Op::Xclip(size) => {
                    v.extend((xaddr..xaddr + size).map(|s| AlignElement {
                        xaddr: s,
                        xbyte: Some(x[s]),
                        yaddr,
                        ybyte: None,
                    }));
                    xaddr += size
                }
                Op::Yclip(size) => {
                    v.extend((yaddr..yaddr + size).map(|s| AlignElement {
                        xaddr,
                        xbyte: None,
                        yaddr: s,
                        ybyte: Some(y[s]),
                    }));
                    yaddr += size
                }
            }
        }
        (v, xaddr, yaddr)
    }
}

/// Aligns x to y as a whole
fn align_whole<F: MatchFunc>(
    x: FileContent,
    y: FileContent,
    scoring: Scoring<F>,
    band: Banded,
    sender: Sender<AlignedMessage>,
) {
    let _ = sender.send(AlignedMessage::Append(
        AlignElement::from_array(&band.align(scoring, &x, &y), &x, &y, 0, 0).0,
    ));
}

/// Blockwise alignment in the ascending address direction
pub fn align_end<F: MatchFunc + Clone>(
    x: FileContent,
    y: FileContent,
    addr: (usize, usize),
    scoring: Scoring<F>,
    band: Banded,
    block_size: usize,
    sender: Sender<AlignedMessage>,
) {
    let (mut xaddr, mut yaddr) = addr;
    // we want to have the beginning of our two arrays aligned at the same place
    // since we start from a previous alignment or a cursor
    let end_scorer = scoring
        .xclip_prefix(pairwise::MIN_SCORE)
        .yclip_prefix(pairwise::MIN_SCORE);
    while xaddr < x.len() && yaddr < y.len() {
        // align at most block_size bytes from each sequence
        let end_aligned = band.align(
            end_scorer.clone(),
            &x[xaddr..(xaddr + block_size).min(x.len())],
            &y[yaddr..(yaddr + block_size).min(y.len())],
        );
        // we only actually append at most half of the block size since we make sure gaps crossing
        // block boundaries are better detected
        let ops = &end_aligned[0..end_aligned.len().min(block_size / 2)];
        // we will not progress like this, so might as well quit
        if ops.is_empty() {
            break;
        }
        let (end, new_xaddr, new_yaddr) = AlignElement::from_array(ops, &x, &y, xaddr, yaddr);
        if sender.send(AlignedMessage::Append(end)).is_err() {
            return;
        }
        xaddr = new_xaddr;
        yaddr = new_yaddr;
    }
    let clip = if x.len() == xaddr {
        Op::Yclip(y.len() - yaddr)
    } else if y.len() == yaddr {
        Op::Xclip(x.len() - xaddr)
    } else {
        return;
    };
    let leftover = AlignElement::from_array(&[clip], &x, &y, xaddr, yaddr).0;
    let _ = sender.send(AlignedMessage::Append(leftover));
}

/// Same as align_end, but in the other direction
pub fn align_front<F: MatchFunc + Clone>(
    x: FileContent,
    y: FileContent,
    addr: (usize, usize),
    scoring: Scoring<F>,
    band: Banded,
    block_size: usize,
    sender: Sender<AlignedMessage>,
) {
    let (mut xaddr, mut yaddr) = addr;
    let scorer = scoring
        .xclip_suffix(pairwise::MIN_SCORE)
        .yclip_suffix(pairwise::MIN_SCORE);
    while xaddr > 0 && yaddr > 0 {
        let lower_xaddr = xaddr.saturating_sub(block_size);
        let lower_yaddr = yaddr.saturating_sub(block_size);
        let aligned = band.align(
            scorer.clone(),
            &x[lower_xaddr..xaddr],
            &y[lower_yaddr..yaddr],
        );
        // unlike in align_end, we create the Alignelement from the whole array and then cut it
        // in half. This is because the addresses returned from from_array are at the end, which
        // we already know, so we instead take the start addresses from the array itself
        let (end, _, _) = AlignElement::from_array(&aligned, &x, &y, lower_xaddr, lower_yaddr);
        let real_end = Vec::from(&end[end.len().saturating_sub(block_size / 2)..end.len()]);
        // if this is empty, we will not progress, so send the leftover out and quit after that
        if real_end.is_empty() {
            break;
        }
        let first = real_end.first().unwrap();
        xaddr = first.xaddr;
        yaddr = first.yaddr;
        if sender.send(AlignedMessage::Prepend(real_end)).is_err() {
            return;
        }
    }
    let clip = if xaddr == 0 {
        Op::Yclip(yaddr)
    } else if yaddr == 0 {
        Op::Xclip(xaddr)
    } else {
        return;
    };
    let leftover = AlignElement::from_array(&[clip], &x, &y, 0, 0).0;
    let _ = sender.send(AlignedMessage::Prepend(leftover));
}

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
        let thread_num = num_cpus::get().clamp(1, 256);
        let (send, recv) = std::sync::mpsc::sync_channel::<Vec<Complex32>>(4.max(thread_num));
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
    outvecs: SyncSender<Vec<Complex32>>,
    fft: Arc<dyn RealToComplex<f32>>,
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

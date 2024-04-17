pub mod rustbio;
pub mod wfa2;
use std::{
    ops::Range,
    sync::{
        atomic::{AtomicBool, AtomicU16, Ordering},
        mpsc::{Sender, SyncSender},
        Arc,
    },
    thread::available_parallelism,
};

use crate::{datastruct::SignedArray, file::FileContent, view::AlignedMessage};
use bio::alignment::AlignmentOperation as Op;
use realfft::{num_complex::Complex64, RealFftPlanner, RealToComplex};
use serde::{Deserialize, Serialize};

pub use self::rustbio::Banded;
use self::rustbio::RustBio;
use self::wfa2::Wfa2;

pub const DEFAULT_BLOCKSIZE: usize = 8192;

/// An align mode, can be either Local for local alignment, global for global alignment,
/// or Blockwise with a given block size. The blockwise mode starts from a given position
/// and aligns only using `blocksize` bytes from each sequence in one direction, which
/// makes it works fast and local, but it doesn't see bigger gaps and everything after big gaps
/// tends to be unaligned.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum AlignMode {
    Global,
    Semiglobal,
    Blockwise(usize),
}

#[derive(Clone, Copy, Debug)]
pub enum InternalMode {
    Global,
    Semiglobal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AlgorithmKind {
    Global,
    Semiglobal,
}

impl From<AlignMode> for InternalMode {
    fn from(value: AlignMode) -> Self {
        match value {
            AlignMode::Global | AlignMode::Blockwise(_) => InternalMode::Global,
            AlignMode::Semiglobal => InternalMode::Semiglobal,
        }
    }
}

pub enum CheckStatus {
    Ok,
    MemoryWarning,
    #[allow(dead_code)]
    Error(String),
}

trait Align {
    fn align(&self, algo: &AlignAlgorithm, mode: InternalMode, x: &[u8], y: &[u8]) -> Vec<Op>;
    fn check_params(
        &self,
        algo: &AlignAlgorithm,
        mode: InternalMode,
        x_size: usize,
        y_size: usize,
    ) -> CheckStatus;
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "backend")]
pub enum AlignBackend {
    #[serde(rename = "rustbio")]
    RustBio(RustBio),
    #[serde(rename = "wfa2")]
    Wfa2(Wfa2),
}

impl AlignBackend {
    fn aligner(&self) -> &dyn Align {
        match self {
            AlignBackend::RustBio(r) => r,
            AlignBackend::Wfa2(w) => w,
        }
    }
}

/// Contains parameters to run the alignment algorithm with
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct AlignAlgorithm {
    pub name: String,
    pub gap_open: i32,
    pub gap_extend: i32,
    pub mismatch_score: i32,
    pub match_score: i32,
    pub mode: AlignMode,
    pub backend: AlignBackend,
}

impl Default for AlignAlgorithm {
    fn default() -> Self {
        AlignAlgorithm {
            name: "Default".to_string(),
            gap_open: -5,
            gap_extend: -1,
            mismatch_score: -1,
            match_score: 1,
            mode: AlignMode::Blockwise(DEFAULT_BLOCKSIZE),
            backend: AlignBackend::RustBio(RustBio::default()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AlignInfo {
    pub global: AlignAlgorithm,
    pub semiglobal: AlignAlgorithm,
}

impl Default for AlignInfo {
    fn default() -> Self {
        Self {
            global: Default::default(),
            semiglobal: AlignAlgorithm::default_semiglobal(),
        }
    }
}

impl AlignInfo {
    pub fn check_start_align(
        &self,
        files: [FileContent; 2],
        selection: [Option<Range<usize>>; 2],
    ) -> CheckStatus {
        match selection.clone() {
            [Some(x), None] | [None, Some(x)] if !x.is_empty() => {
                let right = selection[1].is_some();
                let [file0, file1] = files.clone();
                let y_size = if right { file0.len() } else { file1.len() };
                let x_size = x.len();
                let res = self.semiglobal.backend.aligner().check_params(
                    &self.global,
                    InternalMode::Semiglobal,
                    x_size,
                    y_size,
                );
                if !matches!(res, CheckStatus::Ok) {
                    return res;
                }
            }
            _ => {}
        };
        let [x_size, y_size] = if let AlignMode::Blockwise(size) = self.global.mode {
            [size, size]
        } else {
            files.map(|x| x.len())
        };
        self.global.backend.aligner().check_params(
            &self.global,
            InternalMode::Global,
            x_size,
            y_size,
        )
    }

    pub fn start_align_with_selection(
        &self,
        files: [FileContent; 2],
        selection: [Option<Range<usize>>; 2],
        addr: [usize; 2],
        sender: Sender<AlignedMessage>,
    ) {
        let (selected, right, end) = match selection.clone() {
            // we skip this option if the selection is empty,
            // since it does not really make sense to do glocal alignment in that case
            [Some(x), None] | [None, Some(x)] if !x.is_empty() => {
                let right = selection[1].is_some();
                (
                    x.clone(),
                    selection[1].is_some(),
                    addr[right as usize] != x.start,
                )
            }
            _ => {
                let [file0, file1] = files;
                // if both or none are selected, just do the normal process
                return self
                    .global
                    .start_align(file0, file1, (addr[0], addr[1]), sender);
            }
        };
        let algo = self.clone();
        std::thread::spawn(move || {
            algo.align_with_selection(files, (selected, right), end, sender)
        });
    }

    /// aligns two files first using the semiglobal algorithm, then aligns the rest of the files
    /// using the global algorithm
    /// The bool in selection indicates whether the selection is on the right side
    fn align_with_selection(
        &self,
        files: [FileContent; 2],
        selection: (Range<usize>, bool),
        end: bool,
        sender: Sender<AlignedMessage>,
    ) {
        let (select, right) = selection;
        let full_pattern = &files[right as usize].clone();
        let pattern = &files[right as usize].clone()[select.clone()];
        let text = &files[(!right) as usize].clone()[..];
        let alignment = self
            .semiglobal
            .align(pattern, text, InternalMode::Semiglobal);

        let (alignment, textaddr) = ops_pattern_subrange(&alignment);
        let (mut array, pattern_end, text_end) =
            AlignElement::from_array(alignment, full_pattern, text, select.start, textaddr);
        let (start_addr, end_addr) = if right {
            array.iter_mut().for_each(|x| *x = x.mirror());
            ((textaddr, select.start), (text_end, pattern_end))
        } else {
            ((select.start, textaddr), (pattern_end, text_end))
        };
        let (prepend, append) = if end {
            let ap = array.pop().into_iter().collect();
            (array, ap)
        } else {
            (Vec::new(), array)
        };
        if sender.send(AlignedMessage::Append(append)).is_err() {
            return;
        }
        if sender.send(AlignedMessage::Prepend(prepend)).is_err() {
            return;
        }
        let blocksize = if let AlignMode::Blockwise(s) = self.global.mode {
            s
        } else {
            usize::MAX
        };
        let files2 = files.clone();
        let sender2 = sender.clone();
        let algo = self.global.clone();
        std::thread::spawn(move || {
            algo.align_end(
                files2[0].clone(),
                files2[1].clone(),
                end_addr,
                blocksize,
                sender2,
            );
        });
        self.global.align_front(
            files[0].clone(),
            files[1].clone(),
            start_addr,
            blocksize,
            sender,
        );
    }
}

impl AlignAlgorithm {
    pub fn default_semiglobal() -> Self {
        AlignAlgorithm {
            mode: AlignMode::Semiglobal,
            ..Default::default()
        }
    }

    /// Aligns x to y as a whole
    fn align_whole(
        &self,
        x: FileContent,
        y: FileContent,
        (xaddr, yaddr): (usize, usize),
        sender: Sender<AlignedMessage>,
    ) {
        let alignment = self.align(&x, &y, InternalMode::Global);
        let _ = sender.send(AlignedMessage::Initial(
            AlignElement::from_array(&alignment, &x, &y, 0, 0).0,
            [xaddr, yaddr],
        ));
    }
    /// This function starts the threads for the alignment, which send the data over the sender.
    /// It should then immediately return.
    pub fn start_align(
        &self,
        x: FileContent,
        y: FileContent,
        addr: (usize, usize),
        sender: Sender<AlignedMessage>,
    ) {
        let algo = self.clone();
        match self.mode {
            AlignMode::Global => {
                std::thread::spawn(move || algo.align_whole(x, y, addr, sender));
            }
            AlignMode::Blockwise(blocksize) => {
                std::thread::spawn(move || algo.align_initial_block(x, y, addr, blocksize, sender));
            }
            AlignMode::Semiglobal => unreachable!("Semiglobal alignment is not supported here"),
        }
    }

    fn align(&self, x: &[u8], y: &[u8], mode: InternalMode) -> Vec<Op> {
        if x[..] == y[..] {
            return vec![Op::Match; x.len()];
        }
        self.backend.aligner().align(self, mode, x, y)
    }

    pub fn align_initial_block(
        &self,
        x: FileContent,
        y: FileContent,
        (xaddr, yaddr): (usize, usize),
        block_size: usize,
        sender: Sender<AlignedMessage>,
    ) {
        // extend half block size in each direction until we bump into
        // file starts or file ends
        let before = (block_size / 2).min(xaddr).min(yaddr);
        // if we align at the beginning, we have leftover block_size that
        // we can add to the end
        let before_deficit = block_size / 2 - before;
        let after = ((block_size + 1) / 2 + before_deficit)
            .min(x.len() - xaddr)
            .min(y.len() - yaddr);
        let x_block = &x[xaddr - before..xaddr + after];
        let y_block = &y[yaddr - before..yaddr + after];
        let aligned = self.align(x_block, y_block, self.mode.into());
        let (aligned_ops, xaddr_end, yaddr_end) =
            AlignElement::from_array(&aligned, &x, &y, xaddr - before, yaddr - before);
        let (Ok(xcursor) | Err(xcursor)) = aligned_ops.signed_binary_search(&xaddr, |lhs, rhs| {
            let rhs = rhs.unwrap();
            lhs.cmp(&rhs.xaddr)
        });
        let xcursor = xcursor as usize;
        let (Ok(ycursor) | Err(ycursor)) = aligned_ops.signed_binary_search(&xaddr, |lhs, rhs| {
            let rhs = rhs.unwrap();
            lhs.cmp(&rhs.yaddr)
        });
        let ycursor = ycursor as usize;
        let middle = (xcursor + ycursor) / 2;
        // keep half of the block size in each direction, but if the cursors are
        // not included, extend it to keep them so that the view still knows where
        // it is
        let start = (middle - before / 2).min(xcursor).min(ycursor);
        let end = (middle + after / 2).max(xcursor).max(ycursor);
        let ops = aligned_ops[start..end].to_vec();
        if sender
            .send(AlignedMessage::Initial(ops, [xaddr, yaddr]))
            .is_err()
        {
            return;
        }
        let algo = self.clone();
        let x_cp = x.clone();
        let y_cp = y.clone();
        let sender_cp = sender.clone();
        let start_addr = (aligned_ops[start].xaddr, aligned_ops[start].yaddr);
        std::thread::spawn(move || algo.align_front(x_cp, y_cp, start_addr, block_size, sender_cp));
        let end_addr = aligned_ops
            .get(end)
            .map(|x| (x.xaddr, x.yaddr))
            .unwrap_or((xaddr_end, yaddr_end));
        self.align_end(x, y, end_addr, block_size, sender);
    }

    /// Blockwise alignment in the ascending address direction
    pub fn align_end(
        &self,
        x: FileContent,
        y: FileContent,
        addr: (usize, usize),
        block_size: usize,
        sender: Sender<AlignedMessage>,
    ) {
        let (mut xaddr, mut yaddr) = addr;
        // we want to have the beginning of our two arrays aligned at the same place
        // since we start from a previous alignment or a cursor
        while xaddr < x.len() && yaddr < y.len() {
            // align at most block_size bytes from each sequence
            let end_aligned = self.align(
                &x[xaddr..(xaddr + block_size).min(x.len())],
                &y[yaddr..(yaddr + block_size).min(y.len())],
                self.mode.into(),
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
    pub fn align_front(
        &self,
        x: FileContent,
        y: FileContent,
        addr: (usize, usize),
        block_size: usize,
        sender: Sender<AlignedMessage>,
    ) {
        let (mut xaddr, mut yaddr) = addr;
        while xaddr > 0 && yaddr > 0 {
            let lower_xaddr = xaddr.saturating_sub(block_size);
            let lower_yaddr = yaddr.saturating_sub(block_size);
            let aligned = self.align(
                &x[lower_xaddr..xaddr],
                &y[lower_yaddr..yaddr],
                self.mode.into(),
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
    /// mirrors the values
    pub fn mirror(&self) -> AlignElement {
        AlignElement {
            xaddr: self.yaddr,
            xbyte: self.ybyte,
            yaddr: self.xaddr,
            ybyte: self.xbyte,
        }
    }
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

/// Removes leading/trailing deletions and clips from the alignment
fn ops_pattern_subrange(mut ops: &[Op]) -> (&[Op], usize) {
    let mut ret_addr = 0;
    if let [Op::Yclip(addr), rest @ ..] = ops {
        ops = rest;
        ret_addr += addr;
    }
    while let [Op::Del, rest @ ..] = ops {
        ops = rest;
        ret_addr += 1;
    }
    while let [rest @ .., Op::Del | Op::Yclip(_)] = ops {
        ops = rest;
    }
    (ops, ret_addr)
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

//! This module contains bindings for the alignment algorithms used in biodiff.
//! The main interface are the methods of the [`AlignAlgorithm`] struct.
//!
//! If handling of selections and semiglobal alignment is needed, the [`AlignInfo`] struct
//! should be used.
//!
//! Before running the alignment, the parameters should be checked with the [`AlignAlgorithm::check_parameters`]
//! or [`AlignInfo::check_parameters`] methods.
//!
//! Most methods are meant for a gradual alignment of files, but the [`AlignAlgorithm::align_whole`]
//! method is the simplest interface and can be used to align two files as a whole synchronously.
//!
//! The gradual alignment first sends an [`AlignedMessage::Initial`] message with the initial block of bytes,
//! then sends [`AlignedMessage::Append`] and [`AlignedMessage::Prepend`] messages with the rest of the bytes.
pub mod rustbio;
pub mod wfa2;
use std::ops::Range;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

pub use self::rustbio::Banded;
use self::rustbio::RustBio;
use self::wfa2::Wfa2;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Op {
    Match,
    Subst,
    Ins,
    Del,
    Xclip(usize),
    Yclip(usize),
}

fn right_biased_binary_search<F, T, R>(arr: &[R], key: &T, cmp: F) -> Result<usize, usize>
where
    F: Fn(&T, &R) -> std::cmp::Ordering,
{
    let mut search_range = 0..arr.len();
    let mut eq_idx = None;
    while search_range.end - search_range.start > 0 {
        let middle_index = (search_range.start + search_range.end) >> 1;
        match cmp(key, &arr[middle_index]) {
            std::cmp::Ordering::Less => search_range.end = middle_index,
            std::cmp::Ordering::Equal => {
                eq_idx = Some(middle_index);
                search_range.start = middle_index + 1;
            }
            std::cmp::Ordering::Greater => search_range.start = middle_index + 1,
        }
    }
    eq_idx.ok_or(search_range.start)
}

#[cfg(feature = "wfa2")]
pub const DEFAULT_BLOCKSIZE: usize = 32768;
#[cfg(not(feature = "wfa2"))]
pub const DEFAULT_BLOCKSIZE: usize = 8192;

fn as_byte_arrays<FileContent: AsRef<[u8]>>(files: &[Arc<FileContent>; 2]) -> [&[u8]; 2] {
    [(*files[0]).as_ref(), (*files[1]).as_ref()]
}

/// An align mode, can be either Local for local alignment, global for global alignment,
/// or Blockwise with a given block size. The blockwise mode starts from a given position
/// and aligns only using `blocksize` bytes from each sequence in one direction, which
/// makes it works fast and local, but it doesn't see bigger gaps and everything after big gaps
/// tends to be unaligned.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum AlignMode {
    /// Align both sequences as a whole, making the ends match up.
    Global,
    /// Aligns the first sequence as a subsequence of the second sequence,
    /// without penalizing for extra characters in the second sequence.
    Semiglobal,
    /// Aligns blocks of a given size in both sequences, starting from the given address
    /// and extending in both directions until the end of the sequence is reached.
    Blockwise(usize),
}

#[derive(Clone, Copy, Debug)]
enum InternalMode {
    Global,
    Semiglobal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AlgorithmKind {
    /// Align both sequences as a whole, making the ends match up.
    Global,
    /// Aligns the first sequence as a subsequence of the second sequence,
    /// without penalizing for extra characters in the second sequence.
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
    /// Everything is fine
    Ok,
    /// The alignment could use a lot of memory
    MemoryWarning,
    /// The alignment cannot be done because of some error
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

/// The backend used for alignment
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "backend")]
#[non_exhaustive]
pub enum AlignBackend {
    /// The RustBio aligner, which is a bit slower but has more features
    #[serde(rename = "rustbio")]
    RustBio(RustBio),
    /// The WFA2 aligner, which is faster and uses less memory
    /// for global alignment and sequences that are more similar
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
    /// A name for displaying the algorithm in settings etc.
    pub name: String,
    /// The penalty for opening a gap (negative)
    pub gap_open: i32,
    /// The penalty for extending a gap by one character (negative)
    pub gap_extend: i32,
    /// The penalty for a mismatch (negative)
    pub mismatch_score: i32,
    /// The score for a match (non-negative)
    pub match_score: i32,
    /// Whether to align globally, semiglobally or blockwise
    pub mode: AlignMode,
    /// The backend used for alignment
    pub backend: AlignBackend,
}

impl Default for AlignAlgorithm {
    fn default() -> Self {
        #[cfg(feature = "wfa2")]
        let backend = AlignBackend::Wfa2(Wfa2);
        #[cfg(not(feature = "wfa2"))]
        let backend = AlignBackend::RustBio(RustBio::default());
        AlignAlgorithm {
            name: "default".to_string(),
            gap_open: -8,
            gap_extend: -1,
            mismatch_score: -1,
            match_score: 0,
            mode: AlignMode::Blockwise(DEFAULT_BLOCKSIZE),
            backend,
        }
    }
}

/// A bundle of two alignment algorithms, one for global alignment and one for semiglobal alignment.
#[derive(Clone, Debug)]
pub struct AlignInfo {
    pub global: AlignAlgorithm,
    pub semiglobal: AlignAlgorithm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Side {
    First,
    Second,
}

impl Side {
    pub fn as_index(self) -> usize {
        match self {
            Side::First => 0,
            Side::Second => 1,
        }
    }
    pub fn other(self) -> Side {
        match self {
            Side::First => Side::Second,
            Side::Second => Side::First,
        }
    }
}

impl AlignInfo {
    /// Checks whether aligning the files could use a lot of memory
    /// whether the backend is available, and whether the parameters are valid.
    /// The filesizes are the sizes of the two files to be aligned, or zero if they are not known.
    pub fn check_parameters(
        &self,
        filesizes: [usize; 2],
        selection: [Option<Range<usize>>; 2],
    ) -> CheckStatus {
        match selection.clone() {
            // Exactly one side is selected, so we check the parameters for the semiglobal alignment
            [Some(x), None] | [None, Some(x)] if !x.is_empty() => {
                let right = selection[1].is_some();
                let [x, y] = filesizes;
                let [y_size, x_size] = if right { [x, y] } else { [y, x] };
                let res = self.semiglobal.check_parameters([x_size, y_size]);
                if !matches!(res, CheckStatus::Ok) {
                    return res;
                }
            }
            _ => {}
        };
        let [x_size, y_size] = if let AlignMode::Blockwise(size) = self.global.mode {
            [size, size]
        } else {
            filesizes
        };
        self.global.check_parameters([x_size, y_size])
    }

    /// Starts the alignment of the files, using either global or semiglobal alignment
    /// depending on the selection:
    /// * If exactly one side is selected, the selected side is aligned semiglobally
    ///   and then the global algorithm is used to extend in both directions.
    /// * If both or no sides are selected, the files are aligned globally
    /// `addr` represents the current position of the cursor, where alignment starts.
    /// `sender` should return false if the alignment should be stopped.
    pub fn start_align_with_selection<FileContent: AsRef<[u8]> + Send + Sync + 'static>(
        &self,
        files: [Arc<FileContent>; 2],
        selection: [Option<Range<usize>>; 2],
        addr: [usize; 2],
        sender: impl (FnMut(AlignedMessage) -> bool) + Clone + Send + 'static,
    ) {
        let (selected, right, end) = match selection.clone() {
            // we skip this option if the selection is empty,
            // since it does not really make sense to do glocal alignment in that case
            [Some(x), None] | [None, Some(x)] if !x.is_empty() => {
                let right = selection[1].is_some();
                let side = if right { Side::Second } else { Side::First };
                (x.clone(), side, addr[right as usize] != x.start)
            }
            _ => {
                // if both or none are selected, just do the normal process
                return self.global.start_align(files, addr, sender);
            }
        };
        let algo = self.clone();
        std::thread::spawn(move || {
            algo.align_with_selection(files, (selected, right), end, sender)
        });
    }

    /// Aligns two files first using the semiglobal algorithm, then aligns the rest of the files
    /// using the global algorithm
    /// The bool in selection indicates whether the selection is on the right side.
    /// `sender` should return false if the alignment should be stopped.
    fn align_with_selection<FileContent: AsRef<[u8]> + Send + Sync>(
        &self,
        files: [Arc<FileContent>; 2],
        selection: (Range<usize>, Side),
        end: bool,
        mut sender: impl (FnMut(AlignedMessage) -> bool) + Clone + Send,
    ) {
        let (select, side) = selection;
        let full_pattern = (*files[side.as_index()]).as_ref();
        let pattern = &(*files[side.as_index()]).as_ref()[select.clone()];
        let text = &(*files[side.other().as_index()]).as_ref()[..];
        let alignment = self
            .semiglobal
            .align([pattern, text], InternalMode::Semiglobal);

        let (alignment, textaddr) = ops_pattern_subrange(&alignment);
        let (mut array, pattern_end, text_end) =
            AlignElement::from_array(alignment, full_pattern, text, select.start, textaddr);
        let (start_addr, end_addr) = match side {
            Side::First => ([select.start, textaddr], [pattern_end, text_end]),
            Side::Second => {
                array.iter_mut().for_each(|x| *x = x.mirror());
                ([textaddr, select.start], [text_end, pattern_end])
            }
        };
        let (prepend, append) = if end {
            let ap = array.pop().into_iter().collect();
            (array, ap)
        } else {
            (Vec::new(), array)
        };
        if !sender(AlignedMessage::Append(append)) {
            return;
        }
        if !sender(AlignedMessage::Prepend(prepend)) {
            return;
        }
        let files2 = files.clone();
        let sender2 = sender.clone();
        let algo = self.global.clone();
        std::thread::scope(|s| {
            s.spawn(move || {
                algo.align_end(as_byte_arrays(&files2), end_addr, sender2);
            });
            self.global
                .align_front(as_byte_arrays(&files), start_addr, sender);
        });
    }
}

impl AlignAlgorithm {
    /// Checks whether aligning the files could use a lot of memory
    /// whether the backend is available, and whether the parameters are valid.
    /// The filesizes are the sizes of the two files to be aligned, or zero if they are not known.
    pub fn check_parameters(&self, filesizes: [usize; 2]) -> CheckStatus {
        let mut errors = String::new();
        if self.name.is_empty() {
            errors.push_str("name is invalid: must not be empty\n");
        }
        if self.gap_open > 0 {
            errors.push_str("gap open is invalid: must not be positive\n");
        }
        if self.gap_extend > 0 {
            errors.push_str("gap extend is invalid: must not be positive\n");
        }
        if !errors.is_empty() {
            if errors.ends_with('\n') {
                errors.pop();
            }
            return CheckStatus::Error(errors);
        }
        let [x_size, y_size] = if let AlignMode::Blockwise(size) = self.mode {
            [size, size]
        } else {
            filesizes
        };
        self.backend
            .aligner()
            .check_params(self, self.mode.into(), x_size, y_size)
    }

    /// The normal default() implementation gives the default global algorithm.
    /// This one gives the default semiglobal algorithm.
    pub fn default_semiglobal() -> Self {
        AlignAlgorithm {
            mode: AlignMode::Semiglobal,
            ..Default::default()
        }
    }

    /// Aligns x to y as a whole, starting at address 0 in both files.
    pub fn align_whole<FileContent: AsRef<[u8]>>(
        &self,
        files: [Arc<FileContent>; 2],
    ) -> Vec<AlignElement> {
        let [x, y] = as_byte_arrays(&files);
        let alignment = self.align([x, y], InternalMode::Global);
        AlignElement::from_array(&alignment, x.as_ref(), y.as_ref(), 0, 0).0
    }
    /// This function starts the threads for the alignment, which send the data over the sender.
    /// It should then immediately return.
    /// Cannot be used for semiglobal alignment.
    /// `sender` should return false if the alignment should be stopped.
    pub fn start_align<FileContent: AsRef<[u8]> + Send + Sync + 'static>(
        &self,
        files: [Arc<FileContent>; 2],
        addr: [usize; 2],
        mut sender: impl (FnMut(AlignedMessage) -> bool) + Clone + Send + 'static,
    ) {
        let algo = self.clone();
        match self.mode {
            AlignMode::Global => {
                std::thread::spawn(move || {
                    let res = algo.align_whole(files);
                    sender(AlignedMessage::Initial(res, addr));
                });
            }
            AlignMode::Blockwise(_) => {
                std::thread::spawn(move || algo.align_initial_block(files, addr, sender));
            }
            AlignMode::Semiglobal => panic!("Semiglobal alignment is not supported here"),
        }
    }

    fn align(&self, [x, y]: [&[u8]; 2], mode: InternalMode) -> Vec<Op> {
        if x[..] == y[..] {
            return vec![Op::Match; x.len()];
        }
        self.backend.aligner().align(self, mode, x, y)
    }

    fn block_size(&self, files: [&[u8]; 2]) -> usize {
        if let AlignMode::Blockwise(size) = self.mode {
            size
        } else {
            let [x, y] = files;
            x.len().max(y.len())
        }
    }

    /// Aligns the initial block centered around the given addresses, then aligns the rest of the
    /// files in both directions.
    ///
    /// `sender` should return false if the alignment should be stopped.
    pub fn align_initial_block<FileContent: AsRef<[u8]> + Send + Sync>(
        &self,
        files: [Arc<FileContent>; 2],
        addresses: [usize; 2],
        mut sender: impl (FnMut(AlignedMessage) -> bool) + Clone + Send,
    ) {
        let [xaddr, yaddr] = addresses;
        let [x, y] = as_byte_arrays(&files);
        let block_size = self.block_size(as_byte_arrays(&files));
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
        let aligned = self.align([x_block, y_block], self.mode.into());
        let (aligned_ops, xaddr_end, yaddr_end) =
            AlignElement::from_array(&aligned, x, y, xaddr - before, yaddr - before);
        let (Ok(xcursor) | Err(xcursor)) =
            right_biased_binary_search(&aligned_ops, &xaddr, |lhs, rhs| lhs.cmp(&rhs.xaddr));
        let xcursor = xcursor as usize;
        let (Ok(ycursor) | Err(ycursor)) =
            right_biased_binary_search(&aligned_ops, &xaddr, |lhs, rhs| lhs.cmp(&rhs.yaddr));
        let ycursor = ycursor as usize;
        let middle = (xcursor + ycursor) / 2;
        // keep half of the block size in each direction, but if the cursors are
        // not included, extend it to keep them so that the view still knows where
        // it is
        let start = (middle - before / 2).min(xcursor).min(ycursor);
        let end = (middle + after / 2).max(xcursor).max(ycursor);
        let ops = aligned_ops[start..end].to_vec();
        if !sender(AlignedMessage::Initial(ops, [xaddr, yaddr])) {
            return;
        }
        let algo = self.clone();
        let files_cp = files.clone();
        let sender_cp = sender.clone();
        let start_addr = [aligned_ops[start].xaddr, aligned_ops[start].yaddr];
        std::thread::scope(|s| {
            s.spawn(move || {
                let files_cp = as_byte_arrays(&files_cp);
                algo.align_front(files_cp, start_addr, sender_cp)
            });
            let end_addr = aligned_ops
                .get(end)
                .map(|x| [x.xaddr, x.yaddr])
                .unwrap_or([xaddr_end, yaddr_end]);
            self.align_end(as_byte_arrays(&files), end_addr, sender);
        })
    }

    /// Blockwise alignment in the ascending address direction.
    pub fn align_end(
        &self,
        files: [&[u8]; 2],
        start_addresses: [usize; 2],
        mut sender: impl FnMut(AlignedMessage) -> bool,
    ) {
        let block_size = self.block_size(files);
        let [mut xaddr, mut yaddr] = start_addresses;
        let [x, y] = files;
        // we want to have the beginning of our two arrays aligned at the same place
        // since we start from a previous alignment or a cursor
        while xaddr < x.len() && yaddr < y.len() {
            // align at most block_size bytes from each sequence
            let end_aligned = self.align(
                [
                    &x[xaddr..(xaddr + block_size).min(x.len())],
                    &y[yaddr..(yaddr + block_size).min(y.len())],
                ],
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
            if !sender(AlignedMessage::Append(end)) {
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
        let _ = sender(AlignedMessage::Append(leftover));
    }
    /// Blockwise alignment in the descending address direction.
    pub fn align_front(
        &self,
        files: [&[u8]; 2],
        end_addresses: [usize; 2],
        mut sender: impl FnMut(AlignedMessage) -> bool,
    ) {
        let block_size = self.block_size(files);
        let [mut xaddr, mut yaddr] = end_addresses;
        let [x, y] = files;
        while xaddr > 0 && yaddr > 0 {
            let lower_xaddr = xaddr.saturating_sub(block_size);
            let lower_yaddr = yaddr.saturating_sub(block_size);
            let aligned = self.align(
                [&x[lower_xaddr..xaddr], &y[lower_yaddr..yaddr]],
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
            if !sender(AlignedMessage::Prepend(real_end)) {
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
        let _ = sender(AlignedMessage::Prepend(leftover));
    }
}

/// Representation of the alignment that saves the original addresses of the bytes.
/// This has some space overhead, but alignment is slow enough for that not to matter in most cases.
#[derive(Clone, Copy, Debug)]
pub struct AlignElement {
    /// Address of the byte in the first file
    pub xaddr: usize,
    /// The byte in the first file, or None if there is a gap
    pub xbyte: Option<u8>,
    /// Address of the byte in the second file
    pub yaddr: usize,
    /// The byte in the second file, or None if there is a gap
    pub ybyte: Option<u8>,
}

#[derive(Clone, Debug)]
pub enum AlignedMessage {
    /// New bytes to append to the end of the current view
    Append(Vec<AlignElement>),
    /// New bytes to prepend to the end of the current view
    Prepend(Vec<AlignElement>),
    /// The initial block of bytes.
    /// The two addresses are the original cursor addresses passed to the aligner.
    Initial(Vec<AlignElement>, [usize; 2]),
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

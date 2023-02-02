mod aligned;
mod unaligned;

use std::{iter::Peekable, ops::Range};

pub use aligned::{Aligned, AlignedMessage};
pub use unaligned::Unaligned;

/// checks if addr is in the current element of `iter` and if not,
/// moves it ahead
fn is_next_search_result<O: Ord + Copy>(
    iter: &mut Peekable<impl Iterator<Item = (O, O)>>,
    addr: O,
) -> bool {
    loop {
        match iter.peek() {
            Some((_, end)) if end <= &addr => {
                iter.next();
            }
            otherwise => break otherwise.copied(),
        }
    }
    .map_or(false, |(start, end)| (start..end).contains(&addr))
}

fn next_difference(
    address: isize,
    range: Range<isize>,
    forward: bool,
    is_different: impl Fn(isize) -> bool,
) -> isize {
    let sign = if forward { 1 } else { -1 };
    let mut i = address;
    while range.contains(&i) && is_different(i) {
        i += sign;
    }
    while range.contains(&i) && !is_different(i) {
        i += sign;
    }
    i.clamp(range.start, range.end - 1)
}

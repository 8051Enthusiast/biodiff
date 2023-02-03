mod aligned;
mod unaligned;

use std::ops::Range;

pub use aligned::{Aligned, AlignedMessage};
pub use unaligned::Unaligned;

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

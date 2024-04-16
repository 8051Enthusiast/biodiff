use std::ops::Range;

use crate::{file::FileContent, util::entropy};

/// trait for arrays with signed index
pub trait SignedArray {
    type Item: Clone;
    fn bounds(&self) -> Range<isize>;
    fn get_signed(&self, index: isize) -> Self::Item;
    fn get_range(&self, range: Range<isize>) -> Vec<Self::Item> {
        range.map(|x| self.get_signed(x)).collect()
    }
    fn signed_binary_search<F, T>(&self, key: &T, cmp: F) -> Result<isize, isize>
    where
        F: Fn(&T, &Self::Item) -> std::cmp::Ordering,
    {
        let mut search_range = self.bounds();
        let mut eq_idx = None;
        while search_range.end - search_range.start > 0 {
            let middle_index = (search_range.start + search_range.end) >> 1;
            match cmp(key, &self.get_signed(middle_index)) {
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
}

impl<T: Clone> SignedArray for [T] {
    type Item = Option<T>;

    fn bounds(&self) -> Range<isize> {
        0..self.len() as isize
    }

    fn get_signed(&self, index: isize) -> Option<T> {
        self.get(index as usize).cloned()
    }
}

/// A vector that can easily be extended in both directions
pub struct DoubleVec<T: Clone> {
    front: Vec<T>,
    end: Vec<T>,
}

impl<T: Clone> DoubleVec<T> {
    pub fn new() -> Self {
        DoubleVec {
            front: Vec::new(),
            end: Vec::new(),
        }
    }

    pub fn extend_front(&mut self, other: &[T]) {
        self.front.extend(other.iter().rev().cloned());
    }

    pub fn extend_end(&mut self, other: &[T]) {
        self.end.extend_from_slice(other);
    }

    pub fn first(&self) -> Option<&T> {
        self.front.last().or_else(|| self.end.first())
    }

    pub fn last(&self) -> Option<&T> {
        self.end.last().or_else(|| self.front.first())
    }
}

impl<T: Clone> SignedArray for DoubleVec<T> {
    type Item = Option<T>;

    fn bounds(&self) -> Range<isize> {
        -(self.front.len() as isize)..self.end.len() as isize
    }

    fn get_signed(&self, index: isize) -> Option<T> {
        if index < 0 {
            self.front.get((-1 - index) as usize).cloned()
        } else {
            self.end.get(index as usize).cloned()
        }
    }
}

/// A vector consisting of two arrays, the second one being at an
/// offset of the second one, with at least one byte of overlap
#[derive(Clone)]
pub struct CompVec {
    pub xvec: FileContent,
    pub yvec: FileContent,
    pub shift: isize,
}

impl CompVec {
    /// Creates a new vector pair with zero offset between the vectors.
    pub fn new(xvec: FileContent, yvec: FileContent) -> Self {
        CompVec {
            xvec,
            yvec,
            shift: 0,
        }
    }
    // adds relative_shift to self.shift, ensuring that the arrays
    // still overlap and returns the actual relative shift applied
    fn modify_shift(&mut self, relative_shift: isize) -> isize {
        // they obviously cannot overlap if they're both empty, so just do nothing
        if self.yvec.is_empty() && self.xvec.is_empty() {
            return 0;
        }
        let shift_range = -(self.yvec.len() as isize - 1)..self.xvec.len() as isize;
        let old_shift = self.shift;
        let proposed_shift = old_shift + relative_shift;
        self.shift = proposed_shift.clamp(shift_range.start, shift_range.end - 1);

        self.shift - old_shift
    }
    /// Adds a shift to the left vector, returns the change of index to the same element
    pub fn add_first_shift(&mut self, relative_shift: isize) -> isize {
        self.modify_shift(-relative_shift);
        -relative_shift
    }
    /// Adds a shift to the right vector, returns the change of index to the same element
    pub fn add_second_shift(&mut self, relative_shift: isize) -> isize {
        -relative_shift + self.modify_shift(relative_shift)
    }
    /// Gets the address of the left vector at a given index
    pub fn get_first_addr(&self, index: isize) -> Option<usize> {
        if index < 0 || self.xvec.len() <= index as usize {
            None
        } else {
            Some(index as usize)
        }
    }
    /// Gets the address of the right vector at a given index
    pub fn get_second_addr(&self, index: isize) -> Option<usize> {
        let addr = index - self.shift;
        if addr < 0 || self.yvec.len() <= addr as usize {
            None
        } else {
            Some(addr as usize)
        }
    }
    /// Returns the two arrays
    pub fn get_data(&self) -> [FileContent; 2] {
        [self.xvec.clone(), self.yvec.clone()]
    }
    /// Returns the possible indexes where the first vector has data
    pub fn first_bound(&self) -> Range<isize> {
        0..self.xvec.len() as isize
    }
    /// Returns the possible indexes where the second vector has data
    pub fn second_bound(&self) -> Range<isize> {
        self.shift..self.shift + self.yvec.len() as isize
    }
    /// Calculates the index of a subsequence (with the current offset)
    /// where the product of entropy and length is the highest
    pub fn highest_common_entropy(&self) -> isize {
        if self.xvec.is_empty() || self.yvec.is_empty() {
            return 0;
        }
        let first_start_index = 0.max(self.shift) as usize;
        let second_start_index = 0.max(-self.shift) as usize;
        let max = CommonSequenceIterator::new(
            &self.xvec[first_start_index..],
            &self.yvec[second_start_index..],
        )
        .map(|(data, range)| {
            let entr = entropy(data);
            (entr * range.len() as f32, range.start)
        })
        .max_by(|a, b| {
            // we prefer the one that is not nan for the maximum
            a.0.partial_cmp(&b.0).unwrap_or_else(|| {
                if a.0.is_nan() {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
        });
        let (_, addr) = match max {
            Some(x) => x,
            None => return 0,
        };
        (first_start_index + addr) as isize
    }
}

/// Iterates over common sequences of the two given vectors
pub struct CommonSequenceIterator<'a> {
    idx: usize,
    a: &'a [u8],
    b: &'a [u8],
}

impl<'a> CommonSequenceIterator<'a> {
    /// Returns the CommonSequenceIterator over the given vectors
    pub fn new(a: &'a [u8], b: &'a [u8]) -> Self {
        Self { idx: 0, a, b }
    }
}

impl<'a> Iterator for CommonSequenceIterator<'a> {
    type Item = (&'a [u8], Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let next_common_index = self
            .a
            .iter()
            .zip(self.b.iter())
            .enumerate()
            .find_map(|(i, (a, b))| (a == b).then_some(i))?;
        let next_uncommon_index = self.a[next_common_index..]
            .iter()
            .zip(self.b[next_common_index..].iter())
            .enumerate()
            .find_map(|(i, (a, b))| (a != b).then_some(i + next_common_index))
            .unwrap_or_else(|| self.a.len().min(self.b.len()));
        let ret = &self.a[next_common_index..next_uncommon_index];
        self.a = &self.a[next_uncommon_index..];
        self.b = &self.b[next_uncommon_index..];
        let range = self.idx + next_common_index..self.idx + next_uncommon_index;
        self.idx += next_uncommon_index;
        Some((ret, range))
    }
}

impl SignedArray for CompVec {
    type Item = (Option<u8>, Option<u8>);

    fn bounds(&self) -> Range<isize> {
        match (self.xvec.len() as isize, self.yvec.len() as isize) {
            // if one of the lenghts is zero, we pretend the array doesn't exist
            // and return only the address bounds of the other array
            (0, 0) => 0..0,
            (0, y) => self.shift..(self.shift + y),
            (x, 0) => 0..x,
            (x, y) => {
                let lo = 0.min(self.shift);
                let hi = x.max(y + self.shift);
                lo..hi
            }
        }
    }

    fn get_signed(&self, index: isize) -> Self::Item {
        let x = if index < 0 {
            None
        } else {
            self.xvec.get(index as usize).cloned()
        };
        let y = if index < self.shift {
            None
        } else {
            self.yvec.get((index - self.shift) as usize).cloned()
        };
        (x, y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn doublevec() {
        let mut dv = DoubleVec::<i32>::new();
        assert_eq!(dv.bounds(), 0..0);
        assert_eq!(dv.get_range(0..0), vec![]);
        dv.extend_front(&[0, 1]);
        assert_eq!(dv.bounds(), -2..0);
        assert_eq!(dv.get_range(-1..0), vec![Some(1)]);
        assert_eq!(dv.get_range(-1..1), vec![Some(1), None]);
        assert_eq!(dv.get_range(-3..0), vec![None, Some(0), Some(1)]);
        dv.extend_end(&[2, 3, 4]);
        assert_eq!(dv.bounds(), -2..3);
        assert_eq!(
            dv.get_range(-2..2),
            vec![Some(0), Some(1), Some(2), Some(3)]
        );
        assert_eq!(
            dv.get_range(-3..4),
            vec![None, Some(0), Some(1), Some(2), Some(3), Some(4), None]
        );
        dv.extend_front(&[-3, -2, -1]);
        assert_eq!(dv.bounds(), -5..3);
        assert_eq!(
            dv.get_range(-5..3),
            vec![
                Some(-3),
                Some(-2),
                Some(-1),
                Some(0),
                Some(1),
                Some(2),
                Some(3),
                Some(4)
            ]
        );
    }
    #[test]
    fn bsearch() {
        let mut v = DoubleVec::new();
        v.extend_front(&[1, 2, 3, 7, 10, 13]);
        v.extend_end(&[15, 17, 20, 34, 100]);
        assert_eq!(
            v.signed_binary_search(&Some(0), Option::<i32>::cmp),
            Err(-6)
        );
        assert_eq!(v.signed_binary_search(&Some(1), Option::<i32>::cmp), Ok(-6));
        assert_eq!(v.signed_binary_search(&Some(15), Option::<i32>::cmp), Ok(0));
        assert_eq!(
            v.signed_binary_search(&Some(16), Option::<i32>::cmp),
            Err(1)
        );
        assert_eq!(v.signed_binary_search(&Some(17), Option::<i32>::cmp), Ok(1));
        assert_eq!(
            v.signed_binary_search(&Some(19), Option::<i32>::cmp),
            Err(2)
        );
        assert_eq!(
            v.signed_binary_search(&Some(35), Option::<i32>::cmp),
            Err(4)
        );
        assert_eq!(
            v.signed_binary_search(&Some(99), Option::<i32>::cmp),
            Err(4)
        );
        assert_eq!(
            v.signed_binary_search(&Some(100), Option::<i32>::cmp),
            Ok(4)
        );
        assert_eq!(
            v.signed_binary_search(&Some(101), Option::<i32>::cmp),
            Err(5)
        );
        let mut v = DoubleVec::new();
        v.extend_front(&[1, 3, 5]);
        assert_eq!(v.signed_binary_search(&Some(6), Option::<i32>::cmp), Err(0));
        assert_eq!(
            v.signed_binary_search(&Some(0), Option::<i32>::cmp),
            Err(-3)
        );
    }
    #[test]
    fn common_sequence_iter() {
        let common_sequences = CommonSequenceIterator::new(
            &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 12, 13, 14, 15, 16],
            &[0, 1, 0, 3, 4, 5, 0, 7, 8, 9, 0, 11, 12, 13, 14, 15, 16, 17],
        )
        .collect::<Vec<_>>();
        assert_eq!(
            common_sequences,
            vec![
                (&[0, 1][..], 0..2),
                (&[3, 4, 5][..], 3..6),
                (&[7, 8, 9][..], 7..10),
                (&[12, 13, 14, 15, 16][..], 12..17)
            ]
        )
    }
}

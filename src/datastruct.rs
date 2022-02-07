use std::ops::Range;

use crate::file::FileContent;

/// trait for arrays with signed index
pub trait SignedArray {
    type Item: Clone;
    fn bounds(&self) -> Range<isize>;
    fn get(&self, index: isize) -> Self::Item;
    fn get_range(&self, range: Range<isize>) -> Vec<Self::Item> {
        range.map(|x| self.get(x)).collect()
    }
    fn binary_search<F, T>(&self, key: &T, cmp: F) -> Result<isize, isize>
    where
        F: Fn(&T, &Self::Item) -> std::cmp::Ordering,
    {
        let mut search_range = self.bounds();
        let mut eq_idx = None;
        while search_range.end - search_range.start > 0 {
            let middle_index = (search_range.start + search_range.end) >> 1;
            match cmp(key, &self.get(middle_index)) {
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
}

impl<T: Clone> SignedArray for DoubleVec<T> {
    type Item = Option<T>;

    fn bounds(&self) -> Range<isize> {
        -(self.front.len() as isize)..self.end.len() as isize
    }

    fn get(&self, index: isize) -> Option<T> {
        if index < 0 {
            self.front.get((-1 - index) as usize).cloned()
        } else {
            self.end.get(index as usize).cloned()
        }
    }
}

/// A vector consisting of two arrays, the second one being at an
/// offset of the second one, with at least one byte of overlap
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
    pub fn get_data(&self) -> (FileContent, FileContent) {
        (self.xvec.clone(), self.yvec.clone())
    }
    pub fn first_bound(&self) -> Range<isize> {
        0..self.xvec.len() as isize
    }
    pub fn second_bound(&self) -> Range<isize> {
        self.shift..self.shift as isize + self.yvec.len() as isize
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

    fn get(&self, index: isize) -> Self::Item {
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
        assert_eq!(v.binary_search(&Some(0), Option::<i32>::cmp), Err(-6));
        assert_eq!(v.binary_search(&Some(1), Option::<i32>::cmp), Ok(-6));
        assert_eq!(v.binary_search(&Some(15), Option::<i32>::cmp), Ok(0));
        assert_eq!(v.binary_search(&Some(16), Option::<i32>::cmp), Err(1));
        assert_eq!(v.binary_search(&Some(17), Option::<i32>::cmp), Ok(1));
        assert_eq!(v.binary_search(&Some(19), Option::<i32>::cmp), Err(2));
        assert_eq!(v.binary_search(&Some(35), Option::<i32>::cmp), Err(4));
        assert_eq!(v.binary_search(&Some(99), Option::<i32>::cmp), Err(4));
        assert_eq!(v.binary_search(&Some(100), Option::<i32>::cmp), Ok(4));
        assert_eq!(v.binary_search(&Some(101), Option::<i32>::cmp), Err(5));
        let mut v = DoubleVec::new();
        v.extend_front(&[1, 3, 5]);
        assert_eq!(v.binary_search(&Some(6), Option::<i32>::cmp), Err(0));
        assert_eq!(v.binary_search(&Some(0), Option::<i32>::cmp), Err(-3));
    }
}

use memmap2::{Mmap, MmapOptions};
use std::{ffi::OsStr, fs::File, io::Read, ops::Deref, sync::Arc};

use crate::{search::SearchResults, util::ilog2};

pub type FileContent = Arc<MaybeMapped>;

#[derive(Debug)]
/// file that is either mmap'd or loaded into memory.
/// It implements an deref onto [u8] so that it can be
/// read as a slice.
pub enum MaybeMapped {
    Mapped(Mmap),
    Vector(Vec<u8>),
}

impl Deref for MaybeMapped {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeMapped::Mapped(m) => m,
            MaybeMapped::Vector(v) => v,
        }
    }
}

/// The bytes of a file along with its filename and an index pointing at a byte of the file
#[derive(Debug)]
pub struct FileState {
    pub name: String,
    pub content: FileContent,
    pub index: usize,
    pub search: Option<SearchResults>,
}

impl FileState {
    /// Reads a PointedFile from a path, with index 0.
    pub fn from_file(name: &OsStr) -> Result<Self, std::io::Error> {
        let mut file = File::open(name)?;
        let content = unsafe { MmapOptions::new().map(&file) }
            .map(MaybeMapped::Mapped)
            .or_else(|_| {
                let mut vec = Vec::new();
                file.read_to_end(&mut vec).map(|_| MaybeMapped::Vector(vec))
            })?;
        let f = Arc::new(content);
        Ok(FileState {
            name: name.to_string_lossy().to_string(),
            content: f,
            index: 0,
            search: None,
        })
    }
    /// gets the number of digits used to represent the file addresses
    /// (rounded up to be in pairs
    pub fn address_digits(&self) -> u8 {
        (ilog2((self.content.len() - 1).max(1)) / 8 + 1) * 2
    }
}

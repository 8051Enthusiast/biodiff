use std::{ffi::OsStr, fs::File, io::Read, sync::Arc};

use crate::{search::SearchResults, util::ilog2};

pub type FileContent = Arc<Vec<u8>>;

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
        // while the filesize might change between the metadata call and the read_to_end call,
        // in most cases it will make sure that the vec does not have too much capacity
        let filesize = file.metadata()?.len() as usize;
        let mut vec = Vec::with_capacity(filesize);
        file.read_to_end(&mut vec)?;
        let content = Arc::new(vec);
        Ok(FileState {
            name: name.to_string_lossy().to_string(),
            content,
            index: 0,
            search: None,
        })
    }
    /// gets the number of digits used to represent the file addresses
    /// (rounded up to be in pairs
    pub fn address_digits(&self) -> u8 {
        if self.content.is_empty() {
            return 2;
        }
        (ilog2((self.content.len() - 1).max(1)) / 8 + 1) * 2
    }
}

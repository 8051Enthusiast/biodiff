use std::{fs::File, io::Read, path::Path, sync::Arc};

use crate::search::SearchResults;

pub type FileContent = Arc<FileBytes>;

#[derive(Debug, Clone)]
pub struct FileBytes {
    path: Box<Path>,
    content: Vec<u8>,
}

impl FileBytes {
    pub fn from_file(path: &Path) -> Result<Self, std::io::Error> {
        let mut file = File::open(path)?;
        // while the filesize might change between the metadata call and the read_to_end call,
        // in most cases it will make sure that the vec does not have too much capacity
        let filesize = file.metadata()?.len() as usize;
        let mut vec = Vec::with_capacity(filesize);
        file.read_to_end(&mut vec)?;
        Ok(FileBytes {
            path: path.into(),
            content: vec,
        })
    }

    pub fn reread(&self) -> Result<FileContent, std::io::Error> {
        Ok(Arc::new(Self::from_file(&self.path)?))
    }

    /// gets the number of digits used to represent the file addresses
    /// (rounded up to be in pairs
    pub fn address_digits(&self) -> u8 {
        if self.is_empty() {
            return 2;
        }
        ((self.len() - 1).max(1).ilog2() as u8 / 8 + 1) * 2
    }
}

impl std::ops::Deref for FileBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl AsRef<[u8]> for FileBytes {
    fn as_ref(&self) -> &[u8] {
        &self.content
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
    pub fn from_file(path: &Path) -> Result<Self, std::io::Error> {
        let content = FileBytes::from_file(path)?;
        Ok(FileState {
            name: path.to_string_lossy().to_string(),
            content: Arc::new(content),
            index: 0,
            search: None,
        })
    }
}

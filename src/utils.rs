use std::{ffi::OsStr, sync::Arc};

/// The bytes of a file along with its filename and an index pointing at a byte of the file
#[derive(Clone, Debug)]
pub struct PointedFile {
    pub name: String,
    pub content: Arc<Vec<u8>>,
    pub index: usize,
}

impl PointedFile {
    /// Reads a PointedFile from a path, with index 0.
    pub fn from_file(name: &OsStr) -> Result<Self, std::io::Error> {
        let f = Arc::new(std::fs::read(name)?);
        Ok(PointedFile {
            name: name.to_string_lossy().to_string(),
            content: f,
            index: 0,
        })
    }
}

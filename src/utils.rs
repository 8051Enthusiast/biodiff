use memmap2::{Mmap, MmapOptions};
use std::{ffi::OsStr, fs::File, io::Read, ops::Deref, sync::Arc};

pub type FileContent = Arc<MaybeMapped>;

#[derive(Debug)]
pub enum MaybeMapped {
    Mapped(Mmap),
    Vector(Vec<u8>),
}

impl Deref for MaybeMapped {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeMapped::Mapped(m) => &m,
            MaybeMapped::Vector(v) => &v,
        }
    }
}

/// The bytes of a file along with its filename and an index pointing at a byte of the file
#[derive(Clone, Debug)]
pub struct PointedFile {
    pub name: String,
    pub content: FileContent,
    pub index: usize,
}

impl PointedFile {
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
        Ok(PointedFile {
            name: name.to_string_lossy().to_string(),
            content: f,
            index: 0,
        })
    }
}

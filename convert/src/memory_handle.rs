/// `MemoryHandle` describes where in memory a block of data is stored
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct MemoryHandle {
    /// The offset of the region in Extism linear memory
    pub offset: u64,

    /// The length of the memory region
    pub length: u64,
}

impl MemoryHandle {
    /// Create a new `MemoryHandle` from an offset in memory and length
    ///
    /// # Safety
    /// This function is unsafe because the specified memory region may not be valid.
    pub unsafe fn new(offset: u64, length: u64) -> MemoryHandle {
        MemoryHandle { offset, length }
    }

    /// `NULL` equivalent
    pub fn null() -> MemoryHandle {
        MemoryHandle {
            offset: 0,
            length: 0,
        }
    }

    /// Get the offset of a memory handle
    pub fn offset(&self) -> u64 {
        self.offset
    }

    /// Get the length of the memory region
    pub fn len(&self) -> usize {
        self.length as usize
    }

    /// Returns `true` when the length is 0
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}

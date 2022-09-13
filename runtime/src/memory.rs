use std::collections::BTreeMap;

use crate::*;

use pretty_hex::PrettyHex;

/// Handles memory for plugins
pub struct PluginMemory {
    pub store: Store<Internal>,
    pub memory: Memory,
    pub live_blocks: BTreeMap<usize, usize>,
    pub free: Vec<MemoryBlock>,
    pub position: usize,
}

const PAGE_SIZE: u32 = 65536;

// BLOCK_SIZE_THRESHOLD exists to ensure that free blocks are never split up any
// smaller than this value
const BLOCK_SIZE_THRESHOLD: usize = 32;

impl PluginMemory {
    pub fn new(store: Store<Internal>, memory: Memory) -> Self {
        PluginMemory {
            free: Vec::new(),
            live_blocks: BTreeMap::new(),
            store,
            memory,
            position: 1,
        }
    }

    pub(crate) fn store_u8(&mut self, offs: usize, data: u8) -> Result<(), MemoryAccessError> {
        trace!("store_u8: {data:x} at offset {offs}");
        if offs >= self.size() {
            // This should raise MemoryAccessError
            let buf = &mut [0];
            self.memory.read(&self.store, offs, buf)?;
            return Ok(());
        }
        self.memory.data_mut(&mut self.store)[offs] = data;
        Ok(())
    }

    /// Read from memory
    pub(crate) fn load_u8(&self, offs: usize) -> Result<u8, MemoryAccessError> {
        trace!("load_u8: offset {offs}");
        if offs >= self.size() {
            // This should raise MemoryAccessError
            let buf = &mut [0];
            self.memory.read(&self.store, offs, buf)?;
            return Ok(0);
        }
        Ok(self.memory.data(&self.store)[offs])
    }

    pub(crate) fn store_u32(&mut self, offs: usize, data: u32) -> Result<(), MemoryAccessError> {
        trace!("store_u32: {data:x} at offset {offs}");
        let handle = MemoryBlock {
            offset: offs,
            length: 4,
        };
        self.write(handle, &data.to_ne_bytes())?;
        Ok(())
    }

    /// Read from memory
    pub(crate) fn load_u32(&self, offs: usize) -> Result<u32, MemoryAccessError> {
        trace!("load_u32: offset {offs}");
        let mut buf = [0; 4];

        let handle = MemoryBlock {
            offset: offs,
            length: 4,
        };
        self.read(handle, &mut buf)?;
        Ok(u32::from_ne_bytes(buf))
    }

    pub(crate) fn store_u64(&mut self, offs: usize, data: u64) -> Result<(), MemoryAccessError> {
        trace!("store_u64: {data:x} at offset {offs}");
        let handle = MemoryBlock {
            offset: offs,
            length: 8,
        };
        self.write(handle, &data.to_ne_bytes())?;
        Ok(())
    }

    pub(crate) fn load_u64(&self, offs: usize) -> Result<u64, MemoryAccessError> {
        trace!("load_u64: offset {offs}");
        let mut buf = [0; 8];
        let handle = MemoryBlock {
            offset: offs,
            length: 8,
        };
        self.read(handle, &mut buf)?;
        Ok(u64::from_ne_bytes(buf))
    }

    /// Write to memory
    pub fn write(
        &mut self,
        pos: impl Into<MemoryBlock>,
        data: impl AsRef<[u8]>,
    ) -> Result<(), MemoryAccessError> {
        let pos = pos.into();
        assert!(data.as_ref().len() <= pos.length);
        self.memory
            .write(&mut self.store, pos.offset, data.as_ref())
    }

    /// Read from memory
    pub fn read(
        &self,
        pos: impl Into<MemoryBlock>,
        mut data: impl AsMut<[u8]>,
    ) -> Result<(), MemoryAccessError> {
        let pos = pos.into();
        assert!(data.as_mut().len() <= pos.length);
        self.memory.read(&self.store, pos.offset, data.as_mut())
    }

    /// Size of memory in bytes
    pub fn size(&self) -> usize {
        self.memory.data_size(&self.store)
    }

    /// Size of memory in pages
    pub fn pages(&self) -> u32 {
        self.memory.size(&self.store) as u32
    }

    /// Reserve `n` bytes of memory
    pub fn alloc(&mut self, n: usize) -> Result<MemoryBlock, Error> {
        debug!("Allocating {n} bytes");

        for (i, block) in self.free.iter_mut().enumerate() {
            if block.length == n {
                let block = self.free.swap_remove(i);
                self.live_blocks.insert(block.offset, block.length);
                debug!("Found block with exact size at offset {}", block.offset);
                return Ok(block);
            } else if block.length.saturating_sub(n) >= BLOCK_SIZE_THRESHOLD {
                let handle = MemoryBlock {
                    offset: block.offset,
                    length: n,
                };
                debug!(
                    "Using block with size {} at offset {}",
                    block.length, block.offset
                );

                block.offset += n;
                block.length -= n;
                self.live_blocks.insert(handle.offset, handle.length);
                return Ok(handle);
            }
        }

        let new_offset = self.position.saturating_add(n);

        // If there aren't enough bytes, try to grow the memory size
        if new_offset >= self.size() {
            debug!("Need more memory");

            let bytes_needed = (new_offset as f64 - self.size() as f64) / PAGE_SIZE as f64;
            let mut pages_needed = bytes_needed.ceil() as u64;
            if pages_needed == 0 {
                pages_needed = 1
            }

            info!("Requesting {pages_needed} more pages");
            // This will fail if we've already allocated the maximum amount of memory allowed
            self.memory.grow(&mut self.store, pages_needed)?;
        }

        let mem = MemoryBlock {
            offset: self.position,
            length: n,
        };

        info!(
            "Allocated new block: {} bytes at offset {}",
            mem.length, mem.offset
        );

        self.live_blocks.insert(mem.offset, mem.length);
        self.position += n;
        Ok(mem)
    }

    /// Allocate and copy `data` into the wasm memory
    pub fn alloc_bytes(&mut self, data: impl AsRef<[u8]>) -> Result<MemoryBlock, Error> {
        let handle = self.alloc(data.as_ref().len())?;
        self.write(handle, data)?;
        Ok(handle)
    }

    /// Free the block allocated at `offset`
    pub fn free(&mut self, offset: usize) {
        info!("Freeing block at {offset}");
        if let Some(length) = self.live_blocks.remove(&offset) {
            self.free.push(MemoryBlock { offset, length });
        } else {
            return;
        }

        let free_size: usize = self.free.iter().map(|x| x.length).sum();

        // Perform compaction if there is at least 1kb of free memory available
        if free_size >= 1024 {
            let mut last: Option<MemoryBlock> = None;
            let mut free = Vec::new();
            for block in self.free.iter() {
                match last {
                    None => {
                        free.push(*block);
                    }
                    Some(last) => {
                        if last.offset + last.length == block.offset {
                            free.push(MemoryBlock {
                                offset: last.offset,
                                length: last.length + block.length,
                            });
                        }
                    }
                }
                last = Some(*block);
            }
            self.free = free;
        }
    }

    pub fn dump(&self) {
        let data = self.memory.data(&self.store);

        trace!("{:?}", data[..self.position].hex_dump());
    }

    /// Reset memory
    pub fn reset(&mut self) {
        self.free.clear();
        self.live_blocks.clear();
        self.position = 1;
    }

    /// Get memory as a slice of bytes
    pub fn data(&self) -> &[u8] {
        self.memory.data(&self.store)
    }

    /// Get memory as a mutable slice of bytes
    pub fn data_mut(&mut self) -> &[u8] {
        self.memory.data_mut(&mut self.store)
    }

    /// Get bytes occupied by the provided memory handle
    pub fn get(&self, handle: impl Into<MemoryBlock>) -> &[u8] {
        let handle = handle.into();
        &self.memory.data(&self.store)[handle.offset..handle.offset + handle.length]
    }

    /// Get mutable bytes occupied by the provided memory handle
    pub fn get_mut(&mut self, handle: impl Into<MemoryBlock>) -> &mut [u8] {
        let handle = handle.into();
        &mut self.memory.data_mut(&mut self.store)[handle.offset..handle.offset + handle.length]
    }

    /// Get the length of the block starting at `offs`
    pub fn block_length(&self, offs: usize) -> Option<usize> {
        self.live_blocks.get(&offs).cloned()
    }
}

#[derive(Clone, Copy)]
pub struct MemoryBlock {
    pub offset: usize,
    pub length: usize,
}

impl From<(usize, usize)> for MemoryBlock {
    fn from(x: (usize, usize)) -> Self {
        MemoryBlock {
            offset: x.0,
            length: x.1,
        }
    }
}

impl MemoryBlock {
    pub fn new(offset: usize, length: usize) -> Self {
        MemoryBlock { offset, length }
    }
}

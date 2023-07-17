#![no_std]

use core::sync::atomic::*;

pub const PAGE_SIZE: usize = 65536;
pub const BLOCK_SPLIT_SIZE: usize = 128;
pub static mut INPUT_OFFSET: Pointer = 0;
pub static mut INPUT_LENGTH: Length = 0;
pub static mut OUTPUT_OFFSET: Pointer = 0;
pub static mut OUTPUT_LENGTH: Length = 0;
pub static mut INITIALIZED: AtomicBool = AtomicBool::new(false);
pub static mut ERROR: AtomicU64 = AtomicU64::new(0);
pub static mut START_PAGE: usize = 0;

pub type Pointer = u64;
pub type Length = u64;

#[repr(u8)]
#[derive(PartialEq)]
pub enum MemoryStatus {
    Unknown = 0,
    Active = 1,
    Free = 2,
}

#[repr(C)]
pub struct MemoryRegion {
    pub position: AtomicU64,
    pub length: AtomicU64,
    pub blocks: [MemoryBlock; 0],
}

#[repr(C)]
pub struct MemoryBlock {
    pub status: AtomicU8,
    pub size: usize,
    pub used: usize,
    pub data: [u8; 0],
}

pub fn num_pages(nbytes: u64) -> usize {
    let nbytes = nbytes as f64;
    let page = PAGE_SIZE as f64;
    ((nbytes / page) + 0.5) as usize
}

#[inline]
unsafe fn memory_region() -> &'static mut MemoryRegion {
    &mut *((START_PAGE * PAGE_SIZE) as *mut MemoryRegion)
}

impl MemoryRegion {
    pub unsafe fn new() -> &'static mut MemoryRegion {
        if INITIALIZED
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            return memory_region();
        }

        START_PAGE = core::arch::wasm32::memory_grow(0, 1);
        if START_PAGE == usize::MAX {
            panic!("Out of memory");
        }

        let region = memory_region();
        region.length.store(
            PAGE_SIZE as u64 - core::mem::size_of::<MemoryRegion>() as u64,
            Ordering::Release,
        );
        region.position.store(0, Ordering::Release);
        core::ptr::write_bytes(
            region.blocks.as_mut_ptr() as *mut _,
            MemoryStatus::Unknown as u8,
            core::mem::size_of::<MemoryBlock>(),
        );
        region
    }

    pub unsafe fn reset(&mut self) {
        core::ptr::write_bytes(
            self.blocks.as_mut_ptr() as *mut u8,
            0,
            self.length.load(Ordering::Acquire) as usize,
        );
        self.position.store(0, Ordering::Release);
    }

    unsafe fn find_free_block(
        &mut self,
        length: Length,
        self_position: u64,
    ) -> Option<&'static mut MemoryBlock> {
        // Get the first block
        let mut block = self.blocks.as_mut_ptr();

        // Only loop while the block pointer is less then the current position
        while (block as u64) < self.blocks.as_ptr() as u64 + self_position {
            let b = &mut *block;

            let status = b.status.load(Ordering::Acquire);

            // An unknown block is safe to use
            if status == MemoryStatus::Unknown as u8 {
                return Some(b);
            }

            // Re-use freed blocks when they're large enough
            if status == MemoryStatus::Free as u8 && b.size >= length as usize {
                // Split block if there is too much excess
                if b.size as usize - length as usize >= BLOCK_SPLIT_SIZE {
                    b.size -= length as usize;
                    b.used = 0;

                    let block1 = b.data.as_mut_ptr().add(b.size as usize) as *mut MemoryBlock;
                    let b1 = &mut *block1;
                    b1.size = length as usize;
                    b1.used = 0;
                    b1.status.store(MemoryStatus::Free as u8, Ordering::Release);
                    return Some(b1);
                }

                // Otherwise return the whole block
                return Some(b);
            }

            // Get the next block
            block = b.next_ptr();
        }

        None
    }

    pub unsafe fn alloc(&mut self, length: Length) -> Option<&'static mut MemoryBlock> {
        let self_position = self.position.load(Ordering::Acquire);
        let self_length = self.length.load(Ordering::Acquire);
        let b = self.find_free_block(length, self_position);

        // If there's a free block then re-use it
        if let Some(b) = b {
            b.used = length as usize;
            b.status
                .store(MemoryStatus::Active as u8, Ordering::Release);
            return Some(b);
        }

        // Get the current index for a new block
        let curr = self.blocks.as_ptr() as u64 + self_position;

        // Get the number of bytes available
        let mem_left = self_length - self_position;

        // When the allocation is larger than the number of bytes available
        // we will need to try to grow the memory
        if length >= mem_left {
            // Calculate the number of pages needed to cover the remaining bytes
            let npages = num_pages(length);
            let x = core::arch::wasm32::memory_grow(0, npages);
            if x == usize::MAX {
                return None;
            }
            self.length
                .fetch_add(npages as u64 * PAGE_SIZE as u64, Ordering::SeqCst);
        }

        // Bump the position by the size of the actual data + the size of the MemoryBlock structure
        self.position.fetch_add(
            length + core::mem::size_of::<MemoryBlock>() as u64,
            Ordering::SeqCst,
        );

        // Initialize a new block at the current position
        let ptr = curr as *mut MemoryBlock;
        let block = &mut *ptr;
        block
            .status
            .store(MemoryStatus::Active as u8, Ordering::Release);
        block.size = length as usize;
        block.used = length as usize;
        Some(block)
    }

    /// Finds the block at an offset in memory
    pub unsafe fn find_block(&mut self, offs: Pointer) -> Option<&mut MemoryBlock> {
        if offs >= self.blocks.as_ptr() as Pointer + self.length.load(Ordering::Acquire) as Pointer
        {
            return None;
        }
        let ptr = offs - core::mem::size_of::<MemoryBlock>() as u64;
        let ptr = ptr as *mut MemoryBlock;
        Some(&mut *ptr)
    }
}

impl MemoryBlock {
    #[inline]
    pub unsafe fn next_ptr(&mut self) -> *mut MemoryBlock {
        self.data
            .as_mut_ptr()
            .add(self.size as usize + core::mem::size_of::<MemoryBlock>())
            as *mut MemoryBlock
    }

    pub fn free(&mut self) {
        self.status
            .store(MemoryStatus::Free as u8, Ordering::Release);
    }
}

#[no_mangle]
pub unsafe fn extism_alloc(n: Length) -> Pointer {
    let region = MemoryRegion::new();
    let block = region.alloc(n);
    match block {
        Some(block) => block.data.as_mut_ptr() as Pointer,
        None => 0,
    }
}

#[no_mangle]
pub unsafe fn extism_free(p: Pointer) {
    let block = MemoryRegion::new().find_block(p);
    if let Some(block) = block {
        block.free();
    }
}

#[no_mangle]
pub unsafe fn extism_length(p: Pointer) -> Length {
    if p == 0 {
        return 0;
    }
    if let Some(block) = MemoryRegion::new().find_block(p) {
        block.used as Length
    } else {
        0
    }
}

#[no_mangle]
pub unsafe fn extism_load_u8(p: Pointer) -> u8 {
    *(p as *mut u8)
}

#[no_mangle]
pub unsafe fn extism_load_u64(p: Pointer) -> u64 {
    *(p as *mut u64)
}

#[no_mangle]
pub unsafe fn extism_input_load_u8(p: Pointer) -> u8 {
    *((INPUT_OFFSET + p) as *mut u8)
}

#[no_mangle]
pub unsafe fn extism_input_load_u64(p: Pointer) -> u64 {
    *((INPUT_OFFSET + p) as *mut u64)
}

#[no_mangle]
pub unsafe fn extism_store_u8(p: Pointer, x: u8) {
    *(p as *mut u8) = x;
}

#[no_mangle]
pub unsafe fn extism_store_u64(p: Pointer, x: u64) {
    unsafe {
        *(p as *mut u64) = x;
    }
}

#[no_mangle]
pub fn extism_input_set(p: Pointer, len: Length) {
    unsafe {
        INPUT_OFFSET = p;
        INPUT_LENGTH = len;
    }
}

#[no_mangle]
pub fn extism_output_set(p: Pointer, len: Length) {
    unsafe {
        OUTPUT_OFFSET = p;
        OUTPUT_LENGTH = len;
    }
}

#[no_mangle]
pub fn extism_input_length() -> Length {
    unsafe { INPUT_LENGTH }
}

#[no_mangle]
pub fn extism_input_offset() -> Length {
    unsafe { INPUT_OFFSET }
}

#[no_mangle]
pub fn extism_output_length() -> Length {
    unsafe { OUTPUT_LENGTH }
}

#[no_mangle]
pub fn extism_output_offset() -> Length {
    unsafe { OUTPUT_OFFSET }
}

#[no_mangle]
pub unsafe fn extism_reset() {
    ERROR.store(0, Ordering::SeqCst);
    MemoryRegion::new().reset()
}

#[no_mangle]
pub unsafe fn extism_error_set(ptr: Pointer) {
    ERROR.store(ptr, Ordering::SeqCst);
}

#[no_mangle]
pub unsafe fn extism_error_get() -> Pointer {
    ERROR.load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe fn extism_memory_bytes() -> Length {
    MemoryRegion::new().position.load(Ordering::Acquire)
}

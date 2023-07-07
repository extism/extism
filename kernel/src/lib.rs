#![no_std]

pub const PAGE_SIZE: usize = 65536;
pub const BLOCK_SPLIT_SIZE: usize = 128;
pub static mut INPUT_OFFSET: Pointer = 0;
pub static mut INPUT_LENGTH: Length = 0;
pub static mut OUTPUT_OFFSET: Pointer = 0;
pub static mut OUTPUT_LENGTH: Length = 0;
pub static mut INITIALIZED: bool = false;
pub static mut ERROR: Pointer = 0;
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
    pub position: u64,
    pub length: u64,
    pub blocks: [MemoryBlock; 0],
}

#[repr(C)]
pub struct MemoryBlock {
    pub status: MemoryStatus,
    pub size: usize,
    pub used: usize,
    pub data: [u8; 0],
}

pub fn num_pages(nbytes: u64) -> usize {
    let nbytes = nbytes as f64;
    let page = PAGE_SIZE as f64;
    ((nbytes / page) + 0.5) as usize
}

#[cfg(target_arch = "wasm32")]
pub fn memory_grow<const MEM: u32>(delta: usize) -> usize {
    core::arch::wasm32::memory_grow(MEM, delta)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn memory_grow<const MEM: u32>(_: usize) -> usize {
    unreachable!()
}

#[cfg(target_arch = "wasm32")]
pub fn memory_size<const MEM: u32>() -> usize {
    core::arch::wasm32::memory_size(MEM)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn memory_size<const MEM: u32>() -> usize {
    unreachable!()
}

impl MemoryRegion {
    pub unsafe fn new() -> &'static mut MemoryRegion {
        unsafe {
            if INITIALIZED {
                return &mut *((START_PAGE * PAGE_SIZE) as *mut MemoryRegion);
            }
        }

        START_PAGE = memory_grow::<0>(1);
        if START_PAGE == usize::MAX {
            panic!("Out of memory");
        }

        let ptr = (START_PAGE * PAGE_SIZE) as *mut MemoryRegion;
        let region = &mut *ptr;
        region.length = PAGE_SIZE as u64 - core::mem::size_of::<MemoryRegion>() as u64;
        region.position = 0;
        core::ptr::write_bytes(
            region.blocks.as_mut_ptr() as *mut _,
            MemoryStatus::Unknown as u8,
            core::mem::size_of::<MemoryBlock>(),
        );
        INITIALIZED = true;
        region
    }

    pub unsafe fn reset(&mut self) {
        self.position = 0;
        core::ptr::write_bytes(self.blocks.as_mut_ptr() as *mut u8, 0, self.length as usize);
    }

    pub unsafe fn find_free_block(&mut self, length: Length) -> Option<&'static mut MemoryBlock> {
        // Get the first block
        let mut block = self.blocks.as_mut_ptr();

        // Only loop while the block pointer is less then the current position
        while (block as u64) < self.blocks.as_ptr() as u64 + self.position {
            let b = &mut *block;

            // An unknown block is safe to use
            if b.status == MemoryStatus::Unknown {
                return Some(b);
            }

            // Re-use freed blocks when they're large enough
            if b.status == MemoryStatus::Free && b.size >= length as usize {
                // Split block if there is too much excess
                if b.size as usize - length as usize >= BLOCK_SPLIT_SIZE {
                    b.size -= length as usize;
                    b.used = 0;

                    let block1 = b.data.as_mut_ptr().add(b.size as usize) as *mut MemoryBlock;
                    let b1 = &mut *block1;
                    b1.size = length as usize;
                    b1.used = 0;
                    b1.status = MemoryStatus::Free;
                    return Some(b1);
                }

                // Otherwise return the whole block
                return Some(b);
            }

            // Get the next block
            block = block.add(b.size as usize + core::mem::size_of::<MemoryBlock>());
        }

        None
    }

    pub unsafe fn alloc(&mut self, length: Length) -> &'static mut MemoryBlock {
        let b = self.find_free_block(length);

        // If there's a free block then re-use it
        if let Some(b) = b {
            b.used = length as usize;
            b.status = MemoryStatus::Active;
            return b;
        }

        // Get the current index for a new block
        let curr = self.blocks.as_ptr() as u64 + self.position;

        // Get the number of bytes available
        let mem_left = self.length - self.position;

        // When the allocation is larger than the number of bytes available
        // we will need to try to grow the memory
        if length >= mem_left {
            // Calculate the number of pages needed to cover the remaining bytes
            let npages = num_pages(length);
            let x = memory_grow::<0>(npages);
            if x == usize::MAX {
                // TODO: how should an out of memory error be handled?
                panic!("Out of memory, cannot grow")
            }
            self.length += npages as u64 * PAGE_SIZE as u64;
        }

        // Initialize a new block at the current position
        let ptr = curr as *mut MemoryBlock;
        let block = &mut *ptr;
        block.status = MemoryStatus::Active;
        block.size = length as usize;
        block.used = length as usize;
        // Bump the position byte the size of the actual data + the size of the MemoryBlock structure
        self.position += length + core::mem::size_of::<MemoryBlock>() as u64;
        block
    }
}

impl MemoryBlock {
    /// Finds the block at an offset in memory
    pub unsafe fn find(offs: Pointer) -> &'static mut MemoryBlock {
        let ptr = offs - core::mem::size_of::<MemoryBlock>() as u64;
        let ptr = ptr as *mut MemoryBlock;
        &mut *ptr
    }

    pub fn free(&mut self) {
        self.status = MemoryStatus::Free;
    }
}

#[no_mangle]
pub unsafe fn extism_alloc(n: Length) -> Pointer {
    let region = MemoryRegion::new();
    let block = region.alloc(n);
    block.data.as_mut_ptr() as Pointer
}

#[no_mangle]
pub unsafe fn extism_free(p: Pointer) {
    let block = MemoryBlock::find(p);
    block.free();
}

#[no_mangle]
pub unsafe fn extism_length(p: Pointer) -> Length {
    if p == 0 {
        return 0;
    }
    let block = MemoryBlock::find(p);
    block.used as Length
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
    ERROR = 0;
    MemoryRegion::new().reset()
}

#[no_mangle]
pub unsafe fn extism_error_set(ptr: Pointer) {
    ERROR = ptr;
}

#[no_mangle]
pub unsafe fn extism_error_get() -> Pointer {
    ERROR
}

#[no_mangle]
pub unsafe fn extism_memory_bytes() -> Length {
    MemoryRegion::new().position
}

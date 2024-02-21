//! # Extism kernel
//!
//! - Isolated memory from both host and plugin
//! - An allocator for managing that memory
//! - Input/output handling
//! - Error message handling
//!
//! ## Allocator
//!
//! The Extism allocator is a bump allocator that tracks the `length` of the total number of bytes
//! available to the allocator and `position` to track how much of the data has been used. Things like memory
//! have not really been optimized at all. When a new allocation that is larger than the remaning size is made,
//! the allocator attempts to call `memory.grow` if that fails a `0` offset is returned, which should be interpreted
//! as a failed allocation.
//!
//! ## Input/Output
//!
//! Input and output are just allocated blocks of memory that are marked as either input or output using
//! the `input_set` or `output_set` functions. The MemoryRoot field `input_offset` contains
//! the offset in memory to the input data and `input_length` contains the size of the input data. `output_offset`
//! and `output_length` are used for the output data.
//!
//! ## Error handling
//!
//! The `error` field is used to track the current error message. If it is set to `0` then there is no error.
//! The length of the error message can be retreived using `length`.
//!
//! ## Memory offsets
//! An offset of `0` is similar to a `NULL` pointer in C - it implies an allocation failure or memory error
//! of some kind
//!
//! ## Extism functions
//!
//! These functions are backward compatible with the pre-kernel runtime, but a few new functions are added to
//! give runtimes more access to the internals necesarry to load data in and out of a plugin.
#![no_std]
#![allow(clippy::missing_safety_doc)]

use core::sync::atomic::*;

pub type Pointer = u64;
pub type Handle = u64;

/// WebAssembly page size
const PAGE_SIZE: usize = 65536;

/// Provides information about the usage status of a `MemoryBlock`
#[repr(u8)]
#[derive(PartialEq)]
pub enum MemoryStatus {
    /// Unused memory that is available b
    Unused = 0,
    /// In-use memory
    Active = 1,
    /// Free memory that is available for re-use
    Free = 2,
}

/// A single `MemoryRoot` exists at the start of the memory to track information about the total
/// size of the allocated memory and the position of the bump allocator.
///
/// The overall layout of the Extism-manged memory is organized like this:

/// |------|-------+---------|-------+--------------|
/// | Root | Block +  Data   | Block +     Data     | ...
/// |------|-------+---------|-------+--------------|
///
/// Where `Root` and `Block` are fixed to the size of the `MemoryRoot` and `MemoryBlock` structs. But
/// the size of `Data` is dependent on the allocation size.
///
/// This means that the offset of a `Block` is the size of `Root` plus the size of all existing `Blocks`
/// including their data.
#[repr(C)]
pub struct MemoryRoot {
    /// Set to true after initialization
    pub initialized: AtomicBool,
    /// Position of the bump allocator, relative to `blocks` field
    pub position: AtomicU64,
    /// The total size of all data allocated using this allocator
    pub length: AtomicU64,
    /// Offset of error block
    pub error: AtomicU64,
    /// Input position in memory
    pub input_offset: Handle,
    /// Input length
    pub input_length: u64,
    /// Output position in memory
    pub output_offset: Pointer,
    /// Output length
    pub output_length: u64,
    /// A pointer to the start of the first block
    pub blocks: [MemoryBlock; 0],
}

/// A `MemoryBlock` contains some metadata about a single allocation
#[repr(C)]
pub struct MemoryBlock {
    /// The usage status of the block, `Unused` or `Free` blocks can be re-used.
    pub status: AtomicU8,
    /// The total size of the allocation
    pub size: usize,
    /// The number of bytes currently being used. If this block is a fresh allocation then `size` and `used` will
    /// always be the same. If a block is re-used then these numbers may differ.
    pub used: usize,
    /// A pointer to the block data
    pub data: [u8; 0],
}

/// Returns the number of pages needed for the given number of bytes
pub fn num_pages(nbytes: u64) -> usize {
    let npages = nbytes / PAGE_SIZE as u64;
    let remainder = nbytes % PAGE_SIZE as u64;
    if remainder != 0 {
        (npages + 1) as usize
    } else {
        npages as usize
    }
}

// Get the `MemoryRoot`, this is always stored at offset 1 in memory
#[inline]
unsafe fn memory_root() -> &'static mut MemoryRoot {
    &mut *(1 as *mut MemoryRoot)
}

impl MemoryRoot {
    /// Initialize or load the `MemoryRoot` from the correct position in memory
    pub unsafe fn new() -> &'static mut MemoryRoot {
        let root = memory_root();

        // If this fails then `INITIALIZED` is already `true` and we can just return the
        // already initialized `MemoryRoot`
        if root
            .initialized
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return root;
        }

        // Ensure that at least one page is allocated to store the `MemoryRoot` data
        if core::arch::wasm32::memory_size(0) == 0
            && core::arch::wasm32::memory_grow(0, 1) == usize::MAX
        {
            core::arch::wasm32::unreachable()
        }

        root.input_offset = 0;
        root.input_length = 0;
        root.output_offset = 0;
        root.output_length = 0;
        root.error.store(0, Ordering::Release);

        // Initialize the `MemoryRoot` length, position and data
        root.length.store(
            PAGE_SIZE as u64 - core::mem::size_of::<MemoryRoot>() as u64,
            Ordering::Release,
        );
        root.position.store(0, Ordering::Release);

        // Ensure the first block is marked as `Unused`
        #[allow(clippy::size_of_in_element_count)]
        core::ptr::write_bytes(
            root.blocks.as_mut_ptr() as *mut _,
            MemoryStatus::Unused as u8,
            core::mem::size_of::<MemoryBlock>(),
        );
        root
    }

    /// Resets the position of the allocator and zeroes out all allocations
    pub unsafe fn reset(&mut self) {
        // Clear allocated data
        let self_position = self.position.fetch_and(0, Ordering::SeqCst);
        core::ptr::write_bytes(
            self.blocks.as_mut_ptr() as *mut u8,
            MemoryStatus::Unused as u8,
            self_position as usize,
        );

        // Clear extism runtime metadata
        self.error.store(0, Ordering::Release);
        self.input_offset = 0;
        self.input_length = 0;
        self.output_offset = 0;
        self.output_length = 0;
    }

    #[inline(always)]
    #[allow(unused)]
    fn pointer_in_bounds(&self, p: Pointer) -> bool {
        let start_ptr = self.blocks.as_ptr() as Pointer;
        p >= start_ptr && p < start_ptr + self.length.load(Ordering::Acquire) as Pointer
    }

    #[inline(always)]
    #[allow(unused)]
    fn pointer_in_bounds_fast(p: Pointer) -> bool {
        // Similar to `pointer_in_bounds` but less accurate on the upper bound. This uses the total memory size,
        // instead of checking `MemoryRoot::length`
        let end = (core::arch::wasm32::memory_size(0) as u64) << 16;
        p >= core::mem::size_of::<Self>() as Pointer && p <= end as u64
    }

    // Find a block that is free to use, this can be a new block or an existing freed block. The `self_position` argument
    // is used to avoid loading the allocators position more than once when performing an allocation.
    unsafe fn find_free_block(
        &mut self,
        length: u64,
        self_position: u64,
    ) -> Option<&'static mut MemoryBlock> {
        // Get the first block
        let mut block = self.blocks.as_mut_ptr();

        // Only loop while the block pointer is less then the current position
        while (block as u64) < self.blocks.as_ptr() as u64 + self_position {
            let b = &mut *block;

            // Get the block status, this lets us know if we are able to re-use it
            let status = b.status.load(Ordering::Acquire);

            // An unused block is safe to use
            if status == MemoryStatus::Unused as u8 {
                return Some(b);
            }

            // Re-use freed blocks when they're large enough
            if status == MemoryStatus::Free as u8 && b.size >= length as usize {
                // Split block if there is too much excess
                if b.size - length as usize >= 128 {
                    b.size -= length as usize + core::mem::size_of::<MemoryBlock>();
                    b.used = 0;

                    let block1 = b.data.as_mut_ptr().add(b.size) as *mut MemoryBlock;
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

    /// Create a new `MemoryBlock`, when `Some(block)` is returned, `block` will contain at least enough room for `length` bytes
    /// but may be as large as `length` + `BLOCK_SPLIT_SIZE` bytes. When `None` is returned the allocation has failed.
    pub unsafe fn alloc(&mut self, length: u64) -> Option<&'static mut MemoryBlock> {
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
        let mem_left = self_length - self_position - core::mem::size_of::<MemoryRoot>() as u64;
        let length_with_block = length + core::mem::size_of::<MemoryBlock>() as u64;

        // When the allocation is larger than the number of bytes available
        // we will need to try to grow the memory
        if length_with_block >= mem_left {
            // Calculate the number of pages needed to cover the remaining bytes
            let npages = num_pages(length_with_block - mem_left);
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
        if !Self::pointer_in_bounds_fast(offs) {
            return None;
        }

        // Get the first block
        let mut block = self.blocks.as_mut_ptr();

        // Only loop while the block pointer is less then the current position
        while (block as u64) < self.blocks.as_ptr() as u64 + offs {
            let b = &mut *block;

            // Get the block status, this lets us know if we are able to re-use it
            let status = b.status.load(Ordering::Acquire);

            if status == MemoryStatus::Active as u8 && b.data.as_ptr() as Pointer == offs {
                return Some(b);
            }

            // Get the next block
            block = b.next_ptr();
        }

        None
    }
}

impl MemoryBlock {
    /// Get a pointer to the next block
    ///
    /// NOTE: This does no checking to ensure the resulting pointer is valid, the offset
    /// is calculated based on metadata provided by the current block
    #[inline]
    pub unsafe fn next_ptr(&mut self) -> *mut MemoryBlock {
        self.data.as_mut_ptr().add(self.size) as *mut MemoryBlock
    }

    /// Mark a block as free
    pub fn free(&mut self) {
        self.status
            .store(MemoryStatus::Free as u8, Ordering::Release);
    }
}

// Extism functions

/// Allocate a block of memory and return the offset
#[no_mangle]
pub unsafe fn alloc(n: u64) -> Handle {
    if n == 0 {
        return 0;
    }
    let region = MemoryRoot::new();
    let block = region.alloc(n);
    match block {
        Some(block) => block.data.as_mut_ptr() as Handle,
        None => 0,
    }
}

/// Free allocated memory
#[no_mangle]
pub unsafe fn free(p: Handle) {
    if p == 0 {
        return;
    }
    let root = MemoryRoot::new();
    let block = root.find_block(p);
    if let Some(block) = block {
        block.free();

        // If the input pointer is freed for some reason, make sure the input length to 0
        // since the original data is gone
        if p == root.input_offset {
            root.input_length = 0;
        }
    }
}

/// Get the length of an allocated memory block
///
/// Note: this should only be called on memory handles returned
/// by a call to `alloc` - it will return garbage on invalid offsets
#[no_mangle]
pub unsafe fn length_unsafe(p: Handle) -> u64 {
    if p == 0 {
        return 0;
    }

    if !MemoryRoot::pointer_in_bounds_fast(p) {
        return 0;
    }

    let ptr = p - core::mem::size_of::<MemoryBlock>() as u64;
    let block = &mut *(ptr as *mut MemoryBlock);

    // Simplest sanity check to verify the pointer is a block
    if block.status.load(Ordering::Acquire) != MemoryStatus::Active as u8 {
        return 0;
    }

    block.used as u64
}

/// Get the length but returns 0 if the offset is not a valid handle.
///
/// Note: this function walks each node in the allocations list, which ensures correctness, but is also
/// slow
#[no_mangle]
pub unsafe fn length(p: Pointer) -> u64 {
    if p == 0 {
        return 0;
    }

    if let Some(block) = MemoryRoot::new().find_block(p) {
        block.used as u64
    } else {
        0
    }
}

/// Load a byte from Extism-managed memory
#[no_mangle]
pub unsafe fn load_u8(p: Pointer) -> u8 {
    #[cfg(feature = "bounds-checking")]
    if !MemoryRoot::pointer_in_bounds_fast(p) {
        return 0;
    }
    *(p as *mut u8)
}

/// Load a u64 from Extism-managed memory
#[no_mangle]
pub unsafe fn load_u64(p: Pointer) -> u64 {
    #[cfg(feature = "bounds-checking")]
    if !MemoryRoot::pointer_in_bounds_fast(p + core::mem::size_of::<u64>() as u64 - 1) {
        return 0;
    }
    *(p as *mut u64)
}

/// Load a byte from the input data
#[no_mangle]
pub unsafe fn input_load_u8(offset: u64) -> u8 {
    let root = MemoryRoot::new();
    #[cfg(feature = "bounds-checking")]
    if offset >= root.input_length {
        return 0;
    }
    *((root.input_offset + offset) as *mut u8)
}

/// Load a u64 from the input data
#[no_mangle]
pub unsafe fn input_load_u64(offset: u64) -> u64 {
    let root = MemoryRoot::new();
    #[cfg(feature = "bounds-checking")]
    if offset + core::mem::size_of::<u64>() as u64 > root.input_length {
        return 0;
    }
    *((root.input_offset + offset) as *mut u64)
}

/// Write a byte in Extism-managed memory
#[no_mangle]
pub unsafe fn store_u8(p: Pointer, x: u8) {
    #[cfg(feature = "bounds-checking")]
    if !MemoryRoot::pointer_in_bounds_fast(p) {
        return;
    }
    *(p as *mut u8) = x;
}

/// Write a u64 in Extism-managed memory
#[no_mangle]
pub unsafe fn store_u64(p: Pointer, x: u64) {
    #[cfg(feature = "bounds-checking")]
    if !MemoryRoot::pointer_in_bounds_fast(p + core::mem::size_of::<u64>() as u64 - 1) {
        return;
    }
    *(p as *mut u64) = x;
}

/// Set the range of the input data in memory
/// h must always be a handle so that length works on it
/// len must match length(handle)
#[no_mangle]
pub unsafe fn input_set(h: Handle, len: u64) {
    let root = MemoryRoot::new();
    #[cfg(feature = "bounds-checking")]
    {
        if !root.pointer_in_bounds(h) || !root.pointer_in_bounds(h + len - 1) {
            return;
        }
    }
    root.input_offset = h;
    root.input_length = len;
}

/// Set the range of the output data in memory
#[no_mangle]
pub unsafe fn output_set(p: Pointer, len: u64) {
    let root = MemoryRoot::new();
    #[cfg(feature = "bounds-checking")]
    {
        if !root.pointer_in_bounds(p) || !root.pointer_in_bounds(p + len - 1) {
            return;
        }
    }
    root.output_offset = p;
    root.output_length = len;
}

/// Get the input length
#[no_mangle]
pub fn input_length() -> u64 {
    unsafe { MemoryRoot::new().input_length }
}

/// Get the input offset in Exitsm-managed memory
#[no_mangle]
pub fn input_offset() -> Handle {
    unsafe { MemoryRoot::new().input_offset }
}

/// Get the output length
#[no_mangle]
pub fn output_length() -> u64 {
    unsafe { MemoryRoot::new().output_length }
}

/// Get the output offset in Extism-managed memory
#[no_mangle]
pub unsafe fn output_offset() -> Pointer {
    MemoryRoot::new().output_offset
}

/// Reset the allocator
#[no_mangle]
pub unsafe fn reset() {
    MemoryRoot::new().reset()
}

/// Set the error message offset
#[no_mangle]
pub unsafe fn error_set(h: Handle) {
    let root = MemoryRoot::new();

    // Allow ERROR to be set to 0
    if h == 0 {
        root.error.store(h, Ordering::SeqCst);
        return;
    }

    #[cfg(feature = "bounds-checking")]
    if !root.pointer_in_bounds(h) {
        return;
    }
    root.error.store(h, Ordering::SeqCst);
}

/// Get the error message offset, if it's `0` then no error has been set
#[no_mangle]
pub unsafe fn error_get() -> Handle {
    MemoryRoot::new().error.load(Ordering::SeqCst)
}

/// Get the position of the allocator, this can be used as an indication of how many bytes are currently in-use
#[no_mangle]
pub unsafe fn memory_bytes() -> u64 {
    MemoryRoot::new().length.load(Ordering::Acquire)
}

#[cfg(test)]
mod test {
    use crate::*;
    use wasm_bindgen_test::*;

    // See https://github.com/extism/extism/pull/659
    #[wasm_bindgen_test]
    fn test_659() {
        unsafe {
            // Warning: These offsets will need to change if we adjust the kernel memory layout at all
            reset();
            assert_eq!(alloc(1065), 77);
            assert_eq!(alloc(288), 1154);
            assert_eq!(alloc(128), 1454);
            assert_eq!(length(1154), 288);
            assert_eq!(length(1454), 128);
            free(1454);
            assert_eq!(alloc(213), 1594);
            length_unsafe(1594);
            assert_eq!(alloc(511), 1819);
            assert_eq!(alloc(4), 1454);
            assert_eq!(length(1454), 4);
            assert_eq!(length(1819), 511);
            assert_eq!(alloc(13), 2342);
            assert_eq!(length(2342), 13);
            assert_eq!(alloc(336), 2367);
            assert_eq!(alloc(1077), 2715);
            assert_eq!(length(2367), 336);
            assert_eq!(length(2715), 1077);
            free(2715);
            assert_eq!(alloc(1094), 3804);
            length_unsafe(3804);

            // Allocate 4 bytes, expect to receive address 3788
            assert_eq!(alloc(4), 3788);

            assert_eq!(alloc(4), 3772);
            assert_eq!(length(3772), 4);

            // Address 3788 has not been freed yet, so expect it to have 4 bytes allocated
            assert_eq!(length(3788), 4);
        }
    }

    #[wasm_bindgen_test]
    fn test_oom() {
        let size = 1024 * 1024 * 5;

        let mut last = 0;
        for _ in 0..1024 {
            unsafe {
                let ptr = alloc(size);
                last = ptr;
                if ptr == 0 {
                    break;
                }
                assert_eq!(length(ptr), size);
            }
        }

        assert_eq!(last, 0);
    }
}

//! # Extism kernel
//!
//! - Isolated memory from both host and plugin
//! - An allocator for managing that memory
//! - Input/output handling
//! - Error message handling
//! - Backward compatible `extism_*` functions
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
//! the `extism_input_set` or `extism_output_set` functions. The global variables `INPUT_OFFSET` contains
//! the offset in memory to the input data and `INPUT_LENGTH` contains the size of the input data. `OUTPUT_OFFSET`
//! and `OUTPUT_LENGTH` are used for the output data.
//!
//! ## Error handling
//!
//! The `ERROR` global is used to track the current error message. If it is set to `0` then there is no error.
//! The length of the error message can be retreived using `extism_length`.
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
pub type Length = u64;

/// WebAssembly page size
const PAGE_SIZE: usize = 65536;

/// Determines the amount of bytes that can be wasted by re-using a block. If more than this number is wasted by re-using
/// a block then it will be split into two smaller blocks.
const BLOCK_SPLIT_SIZE: usize = 128;

/// Offset to the input data
static mut INPUT_OFFSET: Pointer = 0;

/// Length of the input data
static mut INPUT_LENGTH: Length = 0;

/// Offset to the output data
static mut OUTPUT_OFFSET: Pointer = 0;

/// Offset to the input data
static mut OUTPUT_LENGTH: Length = 0;

/// Current error message
static mut ERROR: AtomicU64 = AtomicU64::new(0);

/// Determines if the kernel has been initialized already
static mut INITIALIZED: AtomicBool = AtomicBool::new(false);

/// A pointer to the first page that will be managed by Extism, this is set during initialization
static mut START_PAGE: usize = 0;

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
    /// Position of the bump allocator, relative to `START_PAGE`
    pub position: AtomicU64,
    /// The total size of all data allocated using this allocator
    pub length: AtomicU64,
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

// Get the `MemoryRoot` at the correct offset in memory
#[inline]
unsafe fn memory_root() -> &'static mut MemoryRoot {
    &mut *((START_PAGE * PAGE_SIZE) as *mut MemoryRoot)
}

impl MemoryRoot {
    /// Initialize or load the `MemoryRoot` from the correct position in memory
    pub unsafe fn new() -> &'static mut MemoryRoot {
        // If this fails then `INITIALIZED` is already `true` and we can just return the
        // already initialized `MemoryRoot`
        if INITIALIZED
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return memory_root();
        }

        // Ensure that at least one page is allocated to store the `MemoryRoot` data
        START_PAGE = core::arch::wasm32::memory_grow(0, 1);
        if START_PAGE == usize::MAX {
            panic!("Out of memory");
        }

        // Initialize the `MemoryRoot` length, position and data
        let root = memory_root();
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
        core::ptr::write_bytes(
            self.blocks.as_mut_ptr() as *mut u8,
            0,
            self.length.load(Ordering::Acquire) as usize,
        );
        self.position.store(0, Ordering::Release);
    }

    // Find a block that is free to use, this can be a new block or an existing freed block. The `self_position` argument
    // is used to avoid loading the allocators position more than once when performing an allocation.
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

            // Get the block status, this lets us know if we are able to re-use it
            let status = b.status.load(Ordering::Acquire);

            // An unused block is safe to use
            if status == MemoryStatus::Unused as u8 {
                return Some(b);
            }

            // Re-use freed blocks when they're large enough
            if status == MemoryStatus::Free as u8 && b.size >= length as usize {
                // Split block if there is too much excess
                if b.size - length as usize >= BLOCK_SPLIT_SIZE {
                    b.size -= length as usize;
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
        let mem_left = self_length - self_position - core::mem::size_of::<MemoryRoot>() as u64;

        // When the allocation is larger than the number of bytes available
        // we will need to try to grow the memory
        if length >= mem_left {
            // Calculate the number of pages needed to cover the remaining bytes
            let npages = num_pages(length - mem_left);
            let x = core::arch::wasm32::memory_grow(0, npages);
            if x == usize::MAX {
                return None;
            }
            self.length
                .fetch_add(npages as u64 * PAGE_SIZE as u64, Ordering::SeqCst);
        }

        // Bump the position by the size of the actual data + the size of the MemoryBlock structure
        self.position.fetch_add(
            length + core::mem::size_of::<MemoryBlock>() as u64 - 1,
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
    /// Get a pointer to the next block
    ///
    /// NOTE: This does no checking to ensure the resulting pointer is valid, the offset
    /// is calculated based on metadata provided by the current block
    #[inline]
    pub unsafe fn next_ptr(&mut self) -> *mut MemoryBlock {
        self.data
            .as_mut_ptr()
            .add(self.size + core::mem::size_of::<MemoryBlock>()) as *mut MemoryBlock
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
pub unsafe fn extism_alloc(n: Length) -> Pointer {
    if n == 0 {
        return 0;
    }
    let region = MemoryRoot::new();
    let block = region.alloc(n);
    match block {
        Some(block) => block.data.as_mut_ptr() as Pointer,
        None => 0,
    }
}

/// Free allocated memory
#[no_mangle]
pub unsafe fn extism_free(p: Pointer) {
    if p == 0 {
        return;
    } else if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }
    let block = MemoryRoot::new().find_block(p);
    if let Some(block) = block {
        block.free();
    }
}

/// Get the length of an allocated memory block
#[no_mangle]
pub unsafe fn extism_length(p: Pointer) -> Length {
    if p == 0 {
        return 0;
    }
    if let Some(block) = MemoryRoot::new().find_block(p) {
        block.used as Length
    } else {
        0
    }
}

/// Load a byte from Extism-managed memory
#[no_mangle]
pub unsafe fn extism_load_u8(p: Pointer) -> u8 {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return 0;
    }
    *(p as *mut u8)
}

/// Load a u64 from Extism-managed memory
#[no_mangle]
pub unsafe fn extism_load_u64(p: Pointer) -> u64 {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return 0;
    }
    *(p as *mut u64)
}

/// Load a byte from the input data
#[no_mangle]
pub unsafe fn extism_input_load_u8(p: Pointer) -> u8 {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return 0;
    }
    *((INPUT_OFFSET + p) as *mut u8)
}

/// Load a u64 from the input data
#[no_mangle]
pub unsafe fn extism_input_load_u64(p: Pointer) -> u64 {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return 0;
    }
    *((INPUT_OFFSET + p) as *mut u64)
}

/// Write a byte in Extism-managed memory
#[no_mangle]
pub unsafe fn extism_store_u8(p: Pointer, x: u8) {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }
    *(p as *mut u8) = x;
}

/// Write a u64 in Extism-managed memory
#[no_mangle]
pub unsafe fn extism_store_u64(p: Pointer, x: u64) {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }
    *(p as *mut u64) = x;
}

/// Set the range of the input data in memory
#[no_mangle]
pub unsafe fn extism_input_set(p: Pointer, len: Length) {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }
    INPUT_OFFSET = p;
    INPUT_LENGTH = len;
}

/// Set the range of the output data in memory
#[no_mangle]
pub unsafe fn extism_output_set(p: Pointer, len: Length) {
    if p <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }
    OUTPUT_OFFSET = p;
    OUTPUT_LENGTH = len;
}

/// Get the input length
#[no_mangle]
pub fn extism_input_length() -> Length {
    unsafe { INPUT_LENGTH }
}

/// Get the input offset in Exitsm-managed memory
#[no_mangle]
pub fn extism_input_offset() -> Length {
    unsafe { INPUT_OFFSET }
}

/// Get the output length
#[no_mangle]
pub fn extism_output_length() -> Length {
    unsafe { OUTPUT_LENGTH }
}

/// Get the output offset in Extism-managed memory
#[no_mangle]
pub fn extism_output_offset() -> Length {
    unsafe { OUTPUT_OFFSET }
}

/// Reset the allocator
#[no_mangle]
pub unsafe fn extism_reset() {
    ERROR.store(0, Ordering::SeqCst);
    MemoryRoot::new().reset()
}

/// Set the error message offset
#[no_mangle]
pub unsafe fn extism_error_set(ptr: Pointer) {
    if ptr <= MemoryRoot::new().blocks.as_ptr() as Pointer {
        return;
    }

    ERROR.store(ptr, Ordering::SeqCst);
}

/// Get the error message offset, if it's `0` then no error has been set
#[no_mangle]
pub unsafe fn extism_error_get() -> Pointer {
    ERROR.load(Ordering::SeqCst)
}

/// Get the position of the allocator, this can be used as an indication of how many bytes are currently in-use
#[no_mangle]
pub unsafe fn extism_memory_bytes() -> Length {
    MemoryRoot::new().position.load(Ordering::Acquire)
}

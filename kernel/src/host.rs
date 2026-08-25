//! Host-side Extism kernel.
//!
//! Same bump allocator and handle ABI as the wasm32 kernel, but backed by a `Vec<u8>` instead of
//! a Wasm linear memory. Plugin modules keep their own memory; they reach this kernel only through
//! host functions (`alloc`, `store_u8`, …) — which is how Extism already works today.
//!
//! This is the path a Wasm3 (or any engine without Wasm-to-Wasm linking + isolated memories)
//! backend should use.
//!
//! Wasm3 raw-function signatures for these methods (handles are `i64`, not memory pointers):
//!
//! | method | signature |
//! | --- | --- |
//! | `alloc` | `I(I)` |
//! | `free` | `v(I)` |
//! | `length` / `length_unsafe` | `I(I)` |
//! | `load_u8` | `i(I)` |
//! | `load_u64` | `I(I)` |
//! | `store_u8` | `v(Ii)` |
//! | `store_u64` | `v(II)` |
//! | `input_load_u8` | `i(I)` |
//! | `input_load_u64` | `I(I)` |
//! | `input_set` / `output_set` | `v(II)` |
//! | `input_length` / `input_offset` / `output_length` / `output_offset` / `error_get` / `memory_bytes` | `I()` |
//! | `reset` | `v()` |
//! | `error_set` | `v(I)` |

use crate::{num_pages, Handle, MemoryBlock, MemoryRoot, MemoryStatus, Pointer, PAGE_SIZE};
use core::sync::atomic::Ordering;

/// Byte offset of `MemoryRoot` in host linear memory.
///
/// The wasm kernel stores the root at offset `1` (so `0` stays a null handle). That address is
/// not aligned for `AtomicU64` on native targets, so the host kernel uses the first 8-byte
/// aligned offset instead. Handles are still offsets into this buffer, never native pointers.
const ROOT_OFFSET: usize = 8;

/// Native Extism kernel memory. Handles are offsets into this buffer, never native pointers.
pub struct Kernel {
    data: Vec<u8>,
    max_pages: Option<usize>,
}

impl Kernel {
    /// Create a kernel with one page of linear memory, matching `MemoryRoot::new` on first use.
    pub fn new() -> Self {
        let mut kernel = Self {
            data: vec![0u8; PAGE_SIZE],
            max_pages: None,
        };
        kernel.init_root();
        kernel
    }

    /// Limit growth, analogous to a Wasm memory's maximum page count.
    pub fn with_max_pages(mut self, pages: usize) -> Self {
        self.max_pages = Some(pages);
        self
    }

    fn init_root(&mut self) {
        let root = unsafe { self.root() };
        if root
            .initialized
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return;
        }

        root.input_offset = 0;
        root.input_length = 0;
        root.output_offset = 0;
        root.output_length = 0;
        root.error.store(0, Ordering::Release);
        root.length.store(
            PAGE_SIZE as u64 - ROOT_OFFSET as u64 - core::mem::size_of::<MemoryRoot>() as u64,
            Ordering::Release,
        );
        root.position.store(0, Ordering::Release);

        #[allow(clippy::size_of_in_element_count)]
        unsafe {
            core::ptr::write_bytes(
                root.blocks.as_mut_ptr() as *mut u8,
                MemoryStatus::Unused as u8,
                core::mem::size_of::<MemoryBlock>(),
            );
        }
    }

    #[inline]
    fn base(&self) -> *mut u8 {
        self.data.as_ptr() as *mut u8
    }

    #[inline]
    unsafe fn root(&mut self) -> &mut MemoryRoot {
        &mut *self.base().add(ROOT_OFFSET).cast::<MemoryRoot>()
    }

    #[inline]
    fn offset_of(&self, ptr: *const u8) -> Pointer {
        (ptr as usize - self.base() as usize) as Pointer
    }

    #[inline]
    fn ptr(&self, offset: Pointer) -> *mut u8 {
        unsafe { self.base().add(offset as usize) }
    }

    fn in_bounds_fast(&self, p: Pointer) -> bool {
        p >= (ROOT_OFFSET + core::mem::size_of::<MemoryRoot>()) as Pointer
            && (p as usize) <= self.data.len()
    }

    #[allow(dead_code)]
    fn in_bounds(&mut self, p: Pointer) -> bool {
        let blocks_ptr = unsafe { self.root() }.blocks.as_ptr() as *const u8;
        let start = self.offset_of(blocks_ptr);
        let len = unsafe { self.root() }.length.load(Ordering::Acquire);
        p >= start && p < start + len
    }

    fn align_block_bytes(length: u64) -> u64 {
        let align = core::mem::align_of::<MemoryBlock>() as u64;
        length.div_ceil(align).saturating_mul(align)
    }

    fn grow(&mut self, npages: usize) -> bool {
        if npages == 0 {
            return true;
        }
        let current = self.data.len() / PAGE_SIZE;
        if let Some(max) = self.max_pages {
            if current.saturating_add(npages) > max {
                return false;
            }
        }
        let additional = npages.saturating_mul(PAGE_SIZE);
        if self.data.try_reserve(additional).is_err() {
            return false;
        }
        self.data.resize(self.data.len() + additional, 0);
        true
    }

    unsafe fn find_free_block(
        &mut self,
        length: u64,
        self_position: u64,
    ) -> Option<*mut MemoryBlock> {
        let span = Self::align_block_bytes(length);
        let root = self.root();
        let mut block = root.blocks.as_mut_ptr();
        let end = root.blocks.as_ptr() as u64 + self_position;

        while (block as u64) < end {
            let b = &mut *block;
            let status = b.status.load(Ordering::Acquire);

            if status == MemoryStatus::Unused as u8 {
                return Some(block);
            }

            if status == MemoryStatus::Free as u8 && b.size as u64 >= span {
                if b.size as u64 - span >= 128 + core::mem::size_of::<MemoryBlock>() as u64 {
                    b.size -= span as usize + core::mem::size_of::<MemoryBlock>();
                    b.used = 0;

                    let block1 = b.data.as_mut_ptr().add(b.size) as *mut MemoryBlock;
                    let b1 = &mut *block1;
                    b1.size = span as usize;
                    b1.used = 0;
                    b1.status.store(MemoryStatus::Free as u8, Ordering::Release);
                    return Some(block1);
                }
                return Some(block);
            }

            block = b.next_ptr();
        }

        None
    }

    /// Allocate `length` bytes and return a handle (offset), or `0` on failure.
    pub fn alloc(&mut self, length: u64) -> Handle {
        if length == 0 {
            return 0;
        }

        let self_position = unsafe { self.root() }.position.load(Ordering::Acquire);
        let self_length = unsafe { self.root() }.length.load(Ordering::Acquire);
        let blocks_ptr = unsafe { self.root() }.blocks.as_ptr() as *const u8;
        let blocks_off = self.offset_of(blocks_ptr);
        let curr = blocks_off + self_position;

        let mem_left = self_length
            .saturating_sub(self_position)
            .saturating_sub(core::mem::size_of::<MemoryRoot>() as u64);
        let span = Self::align_block_bytes(length);
        let length_with_block = span + core::mem::size_of::<MemoryBlock>() as u64;

        if length_with_block >= mem_left {
            if length_with_block <= self_position {
                if let Some(b) = unsafe { self.find_free_block(length, self_position) } {
                    unsafe {
                        let b = &mut *b;
                        b.used = length as usize;
                        b.status
                            .store(MemoryStatus::Active as u8, Ordering::Release);
                        return self.offset_of(b.data.as_ptr());
                    }
                }
            }

            let npages = num_pages(length_with_block.saturating_sub(mem_left));
            if !self.grow(npages) {
                return 0;
            }
            unsafe { self.root() }
                .length
                .fetch_add(npages as u64 * PAGE_SIZE as u64, Ordering::SeqCst);
        }

        unsafe { self.root() }.position.fetch_add(
            span + core::mem::size_of::<MemoryBlock>() as u64,
            Ordering::SeqCst,
        );

        let ptr = self.ptr(curr) as *mut MemoryBlock;
        unsafe {
            let block = &mut *ptr;
            block
                .status
                .store(MemoryStatus::Active as u8, Ordering::Release);
            block.size = span as usize;
            block.used = length as usize;
            self.offset_of(block.data.as_ptr())
        }
    }

    /// Free the block at `p`. No-op for handle `0`.
    pub fn free(&mut self, p: Handle) {
        if p == 0 {
            return;
        }
        if let Some(block) = self.find_block(p) {
            block.free();
            let root = unsafe { self.root() };
            if p == root.input_offset {
                root.input_length = 0;
            }
        }
    }

    fn find_block(&mut self, offs: Pointer) -> Option<&mut MemoryBlock> {
        if !self.in_bounds_fast(offs) {
            return None;
        }

        // Same walk as the wasm kernel: `blocks.as_ptr() as u64 + offs` is a native
        // address bound here, a linear-memory bound on wasm32. Both terminate once
        // we have passed the candidate data pointer.
        let blocks_ptr = unsafe { self.root() }.blocks.as_mut_ptr();
        let mut block = blocks_ptr;
        while (block as u64) < blocks_ptr as u64 + offs {
            let b = unsafe { &mut *block };
            let status = b.status.load(Ordering::Acquire);
            if status == MemoryStatus::Active as u8 && self.offset_of(b.data.as_ptr()) == offs {
                return Some(b);
            }
            block = unsafe { b.next_ptr() };
        }
        None
    }

    /// Length of the allocation at `p`, or `0` if `p` is not an active handle.
    pub fn length(&mut self, p: Pointer) -> u64 {
        if p == 0 {
            return 0;
        }
        self.find_block(p).map(|b| b.used as u64).unwrap_or(0)
    }

    /// Fast length: trusts that `p` points at an active block's data.
    pub fn length_unsafe(&mut self, p: Handle) -> u64 {
        if p == 0 || !self.in_bounds_fast(p) {
            return 0;
        }
        let ptr = self.ptr(p - core::mem::size_of::<MemoryBlock>() as u64) as *mut MemoryBlock;
        let block = unsafe { &mut *ptr };
        if block.status.load(Ordering::Acquire) != MemoryStatus::Active as u8 {
            return 0;
        }
        block.used as u64
    }

    pub fn load_u8(&self, p: Pointer) -> u8 {
        #[cfg(feature = "bounds-checking")]
        if !self.in_bounds_fast(p) {
            return 0;
        }
        unsafe { *self.ptr(p) }
    }

    pub fn load_u64(&self, p: Pointer) -> u64 {
        #[cfg(feature = "bounds-checking")]
        if !self.in_bounds_fast(p + core::mem::size_of::<u64>() as u64 - 1) {
            return 0;
        }
        unsafe { *(self.ptr(p) as *mut u64) }
    }

    pub fn store_u8(&mut self, p: Pointer, x: u8) {
        #[cfg(feature = "bounds-checking")]
        if !self.in_bounds_fast(p) {
            return;
        }
        unsafe { *self.ptr(p) = x }
    }

    pub fn store_u64(&mut self, p: Pointer, x: u64) {
        #[cfg(feature = "bounds-checking")]
        if !self.in_bounds_fast(p + core::mem::size_of::<u64>() as u64 - 1) {
            return;
        }
        unsafe { *(self.ptr(p) as *mut u64) = x }
    }

    pub fn input_set(&mut self, h: Handle, len: u64) {
        #[cfg(feature = "bounds-checking")]
        {
            if !self.in_bounds(h) || (len > 0 && !self.in_bounds(h + len - 1)) {
                return;
            }
        }
        let root = unsafe { self.root() };
        root.input_offset = h;
        root.input_length = len;
    }

    pub fn output_set(&mut self, p: Pointer, len: u64) {
        #[cfg(feature = "bounds-checking")]
        {
            if !self.in_bounds(p) || (len > 0 && !self.in_bounds(p + len - 1)) {
                return;
            }
        }
        let root = unsafe { self.root() };
        root.output_offset = p;
        root.output_length = len;
    }

    pub fn input_length(&mut self) -> u64 {
        unsafe { self.root().input_length }
    }

    pub fn input_offset(&mut self) -> Handle {
        unsafe { self.root().input_offset }
    }

    pub fn output_length(&mut self) -> u64 {
        unsafe { self.root().output_length }
    }

    pub fn output_offset(&mut self) -> Pointer {
        unsafe { self.root().output_offset }
    }

    pub fn input_load_u8(&mut self, offset: u64) -> u8 {
        let (input_offset, input_length) = {
            let root = unsafe { self.root() };
            (root.input_offset, root.input_length)
        };
        #[cfg(feature = "bounds-checking")]
        if offset >= input_length {
            return 0;
        }
        self.load_u8(input_offset + offset)
    }

    pub fn input_load_u64(&mut self, offset: u64) -> u64 {
        let (input_offset, input_length) = {
            let root = unsafe { self.root() };
            (root.input_offset, root.input_length)
        };
        #[cfg(feature = "bounds-checking")]
        if offset + core::mem::size_of::<u64>() as u64 > input_length {
            return 0;
        }
        self.load_u64(input_offset + offset)
    }

    pub fn reset(&mut self) {
        let root = unsafe { self.root() };
        let self_position = root.position.fetch_and(0, Ordering::SeqCst);
        unsafe {
            core::ptr::write_bytes(
                root.blocks.as_mut_ptr() as *mut u8,
                MemoryStatus::Unused as u8,
                self_position as usize,
            );
        }
        root.error.store(0, Ordering::Release);
        root.input_offset = 0;
        root.input_length = 0;
        root.output_offset = 0;
        root.output_length = 0;
    }

    pub fn error_set(&mut self, h: Handle) {
        if h == 0 {
            unsafe { self.root() }.error.store(h, Ordering::SeqCst);
            return;
        }
        #[cfg(feature = "bounds-checking")]
        if !self.in_bounds(h) {
            return;
        }
        unsafe { self.root() }.error.store(h, Ordering::SeqCst);
    }

    pub fn error_get(&mut self) -> Handle {
        unsafe { self.root() }.error.load(Ordering::SeqCst)
    }

    /// Allocator-managed byte count (same as the wasm `memory_bytes` export).
    pub fn memory_bytes(&mut self) -> u64 {
        unsafe { self.root() }.length.load(Ordering::Acquire)
    }

    /// Host view of kernel linear memory. Offsets/handles index this slice.
    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    /// Mutable host view of kernel linear memory.
    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }

    /// Copy `bytes` into a new allocation. Returns a null handle if `bytes` is empty.
    pub fn copy_from_slice(&mut self, bytes: &[u8]) -> Handle {
        if bytes.is_empty() {
            return 0;
        }
        let handle = self.alloc(bytes.len() as u64);
        if handle == 0 {
            return 0;
        }
        let start = handle as usize;
        self.data[start..start + bytes.len()].copy_from_slice(bytes);
        handle
    }
}

impl Default for Kernel {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_zero_is_null() {
        let mut k = Kernel::new();
        assert_eq!(k.alloc(0), 0);
    }

    #[test]
    fn alloc_and_length() {
        let mut k = Kernel::new();
        let p = k.alloc(65535);
        assert!(p > 0);
        assert_eq!(k.length(p), 65535);
        assert_eq!(k.length(p + 1), 0);
        k.free(p);

        let q = k.alloc(65535);
        assert_eq!(q, p);
        assert_eq!(k.length(q), 65535);
        k.free(q);
    }

    #[test]
    fn reuse_and_split() {
        let mut k = Kernel::new();
        let p = k.alloc(65535);
        k.free(p);
        let r = k.alloc(65535 - 24);
        assert_eq!(r, p);
        assert_eq!(k.length(r), 65535 - 24);
        k.free(r);
    }

    #[test]
    fn small_allocs() {
        let mut k = Kernel::new();
        let p = k.alloc(1);
        assert!(p > 0);
        assert_eq!(k.length(p), 1);
        assert_eq!(k.length_unsafe(p), 1);
        k.free(p);

        let x = k.alloc(2);
        assert!(x > 0);
        assert!(x != p);
        assert_eq!(k.length(x), 2);
        k.free(x);

        for i in 0..64 {
            let p = k.alloc(64 - i);
            assert!(p > 0);
            assert_eq!(k.length(p), 64 - i);
            k.free(p);
        }
    }

    #[test]
    fn grow_pages() {
        let mut k = Kernel::new();
        let p = k.alloc(6553600);
        assert!(p > 0);
        assert_eq!(k.length(p), 6553600);
        k.free(p);

        let p = k.alloc(65536);
        assert!(p > 0);
        assert_eq!(k.length(p), 65536);

        let p = k.alloc(65536 + 1024);
        assert!(p > 0);
        assert_eq!(k.length(p), 65536 + 1024);
        k.free(p);
    }

    #[test]
    fn reset_invalidates_old_handles() {
        let mut k = Kernel::new();
        let first = k.alloc(65536 + 1024);
        let p = k.alloc(65536 + 1024);
        assert!(p > first);
        k.reset();
        let q = k.alloc(65536 + 1024);
        assert_eq!(q, first);
        assert!(q < p);
        assert_eq!(k.length(q), 65536 + 1024);
        assert_eq!(k.length(p), 0);
        k.free(q);
    }

    #[test]
    fn error_roundtrip() {
        let mut k = Kernel::new();
        let p = k.alloc(512);
        k.error_set(p);
        assert_eq!(k.error_get(), p);
    }

    #[test]
    fn load_store() {
        let mut k = Kernel::new();
        let p = k.alloc(8);
        k.store_u64(p, 999);
        assert_eq!(k.load_u64(p), 999);

        let mut buf = [0u8; 8];
        for (i, b) in buf.iter_mut().enumerate() {
            *b = k.load_u8(p + i as u64);
        }
        assert_eq!(u64::from_le_bytes(buf), 999);

        for i in 0..8u64 {
            k.store_u8(p + i, i as u8);
        }
        assert_eq!(k.load_u64(p), 0x0706050403020100);

        assert_eq!(k.load_u64(0xffffffffffff), 0);
        k.store_u8(0xffffffffffff, 0);
    }

    #[test]
    fn input_load() {
        let mut k = Kernel::new();
        let p = k.alloc(12345);
        for i in 0..12345 {
            k.store_u8(p + i, b'a');
        }
        k.input_set(p, 12345);
        for i in 0..12345 {
            assert_eq!(k.input_load_u8(i), b'a');
        }
        assert_eq!(k.input_load_u64(12346), 0);
    }

    #[test]
    fn copy_from_slice_and_output() {
        let mut k = Kernel::new();
        let h = k.copy_from_slice(b"hello wasm3");
        assert!(h > 0);
        assert_eq!(k.length(h), 11);
        k.output_set(h, 11);
        assert_eq!(k.output_offset(), h);
        assert_eq!(k.output_length(), 11);
        let start = h as usize;
        assert_eq!(&k.as_slice()[start..start + 11], b"hello wasm3");
    }

    #[test]
    fn max_pages_oom() {
        let mut k = Kernel::new().with_max_pages(2);
        // 2 pages is not enough for a 5MiB allocation plus metadata.
        let p = k.alloc(1024 * 1024 * 5);
        assert_eq!(p, 0);
    }

    #[test]
    fn handles_are_offsets_not_native_pointers() {
        let mut k = Kernel::new();
        let p = k.alloc(16);
        assert!(p > 0);
        assert!((p as usize) < k.as_slice().len());
        // A native pointer into the Vec would be much larger than linear-memory size.
        assert!(p < PAGE_SIZE as u64 * 4);
    }
}

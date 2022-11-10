type MemoryBlock = { offset: bigint; length: bigint };

export default class Allocator {
  currentIndex: bigint;
  active: Record<number, MemoryBlock>;
  freed: MemoryBlock[];
  memory: Uint8Array;

  constructor(n: number) {
    this.currentIndex = BigInt(1);
    this.active = {};
    this.freed = [];
    this.memory = new Uint8Array(n);
  }

  reset() {
    this.currentIndex = BigInt(1);
    this.active = {};
    this.freed = [];
  }

  alloc(length: bigint): bigint {
    for (var i = 0; i < this.freed.length; i++) {
      let block = this.freed[i];
      if (block.length === length) {
        this.active[Number(block.offset)] = block;
        this.freed.splice(i, 1);
        return block.offset;
      } else if (block.length > length + BigInt(64)) {
        const newBlock = { offset: block.offset, length };
        block.offset += length;
        block.length -= length;
        return newBlock.offset;
      }
    }

    // Resize memory if needed
    // TODO: put a limit on the memory size
    if (BigInt(this.memory.length) < this.currentIndex + length) {
      const tmp = new Uint8Array(Number(this.currentIndex + length + BigInt(64)));
      tmp.set(this.memory);
      this.memory = tmp;
    }

    const offset = this.currentIndex;
    this.currentIndex += length;
    this.active[Number(offset)] = { offset, length };
    return offset;
  }

  getBytes(offset: bigint): Uint8Array | null {
    const block = this.active[Number(offset)];
    if (!block) {
      return null;
    }

    return new Uint8Array(this.memory.buffer, Number(offset), Number(block.length));
  }

  getString(offset: bigint): string | null {
    const bytes = this.getBytes(offset);
    if (bytes === null) {
      return null;
    }

    return new TextDecoder().decode(bytes);
  }

  allocBytes(data: Uint8Array): bigint {
    const offs = this.alloc(BigInt(data.length));
    const bytes = this.getBytes(offs);
    if (bytes === null) {
      this.free(offs);
      return BigInt(0);
    }

    bytes.set(data);
    return offs;
  }

  allocString(data: string): bigint {
    const bytes = new TextEncoder().encode(data);
    return this.allocBytes(bytes);
  }

  getLength(offset: bigint): bigint {
    const block = this.active[Number(offset)];
    if (!block) {
      return BigInt(0);
    }

    return block.length;
  }

  free(offset: bigint) {
    const block = this.active[Number(offset)];
    if (!block) {
      return;
    }

    delete this.active[Number(offset)];
    this.freed.push(block);
  }
}

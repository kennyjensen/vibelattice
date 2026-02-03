/*
 * WASM-backed float32 LU/solve for ASETUP.
 */

function makeAllocator(start = 0) {
  let offset = start;
  const alloc = (bytes, align = 4) => {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
  const reset = () => {
    offset = start;
  };
  return { alloc, reset };
}

export async function loadAoperLinSolveF32Wasm() {
  const url = new URL('./aoper_exec_port.wasm', import.meta.url);
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
  const wasmBytes = await res.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP_COL_f32, BAKSUB_COL_f32 } = instance.exports;
  let f32 = new Float32Array(memory.buffer);
  const refreshViews = () => {
    f32 = new Float32Array(memory.buffer);
  };
  const ensureMemory = (bytes) => {
    const minBytes = 64 * 1024 * 1024;
    const target = Math.max(bytes, minBytes);
    const needed = target - memory.buffer.byteLength;
    if (needed <= 0) return;
    const pages = Math.ceil(needed / 65536);
    memory.grow(pages);
    refreshViews();
  };
  const allocator = makeAllocator(0);

  function factorInPlace(A, dim, n) {
    allocator.reset();
    const aPtr = allocator.alloc(A.length * 4, 4);
    ensureMemory(aPtr + A.length * 4);
    const indxPtr = allocator.alloc((n + 1) * 4, 4);
    const workPtr = allocator.alloc((n + 1) * 4, 4);
    f32.set(A, aPtr / 4);
    LUDCMP_COL_f32(dim, n, aPtr, indxPtr, workPtr);
    return { aPtr, indxPtr };
  }

  function solveWithLUInPlace(aPtr, indxPtr, B, dim, n) {
    allocator.reset();
    const bPtr = allocator.alloc(B.length * 4, 4);
    ensureMemory(bPtr + B.length * 4);
    f32.set(B, bPtr / 4);
    BAKSUB_COL_f32(dim, n, aPtr, indxPtr, bPtr);
    return Float32Array.from(f32.subarray(bPtr / 4, bPtr / 4 + B.length));
  }

  return { factorInPlace, solveWithLUInPlace, memory };
}

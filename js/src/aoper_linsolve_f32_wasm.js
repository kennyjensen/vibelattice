/*
 * WASM-backed float32 LU/solve for ASETUP.
 */
import fs from 'node:fs/promises';
import path from 'node:path';

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

async function readWasmBytes(wasmPath) {
  const isHttp = typeof wasmPath === 'string' && /^(https?:)?\/\//.test(wasmPath);
  if (typeof fetch === 'function' && isHttp) {
    const res = await fetch(wasmPath);
    if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
    return new Uint8Array(await res.arrayBuffer());
  }
  return fs.readFile(wasmPath);
}

export async function loadAoperLinSolveF32Wasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'aoper_exec_port.wasm');
  const wasmBytes = await readWasmBytes(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP_COL_f32, BAKSUB_COL_f32 } = instance.exports;
  let f32 = new Float32Array(memory.buffer);
  let i32 = new Int32Array(memory.buffer);
  const refreshViews = () => {
    f32 = new Float32Array(memory.buffer);
    i32 = new Int32Array(memory.buffer);
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
    ensureMemory(indxPtr + (n + 1) * 4);
    const workPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(workPtr + (n + 1) * 4);
    const bPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(bPtr + (n + 1) * 4);
    f32.set(A, aPtr / 4);
    LUDCMP_COL_f32(dim, n, aPtr, indxPtr, workPtr);
    return { aPtr, indxPtr, bPtr };
  }

  function uploadFactorization(A, INDX, dim, n) {
    allocator.reset();
    const aPtr = allocator.alloc(A.length * 4, 4);
    ensureMemory(aPtr + A.length * 4);
    const indxPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(indxPtr + (n + 1) * 4);
    const bPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(bPtr + (n + 1) * 4);
    f32.set(A, aPtr / 4);
    i32.set(INDX, indxPtr / 4);
    return { aPtr, indxPtr, bPtr, dim, n };
  }

  function solveWithLU(A, B, dim, n) {
    allocator.reset();
    const aPtr = allocator.alloc(A.length * 4, 4);
    ensureMemory(aPtr + A.length * 4);
    const bPtr = allocator.alloc(B.length * 4, 4);
    ensureMemory(bPtr + B.length * 4);
    const indxPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(indxPtr + (n + 1) * 4);
    const workPtr = allocator.alloc((n + 1) * 4, 4);
    ensureMemory(workPtr + (n + 1) * 4);
    f32.set(A, aPtr / 4);
    f32.set(B, bPtr / 4);
    LUDCMP_COL_f32(dim, n, aPtr, indxPtr, workPtr);
    BAKSUB_COL_f32(dim, n, aPtr, indxPtr, bPtr);
    return Float32Array.from(f32.subarray(bPtr / 4, bPtr / 4 + B.length));
  }

  function solveWithLUInPlace(aPtr, indxPtr, B, dim, n, bPtrHint = null) {
    const solveLen = Math.max(0, n + 1);
    const bPtr = Number.isFinite(bPtrHint)
      ? bPtrHint
      : allocator.alloc(solveLen * 4, 4);
    ensureMemory(bPtr + solveLen * 4);
    const inVec = B instanceof Float32Array ? B : Float32Array.from(B || []);
    f32.set(inVec.subarray(0, solveLen), bPtr / 4);
    BAKSUB_COL_f32(dim, n, aPtr, indxPtr, bPtr);
    return Float32Array.from(f32.subarray(bPtr / 4, bPtr / 4 + solveLen));
  }

  return {
    solveWithLU,
    factorInPlace,
    uploadFactorization,
    solveWithLUInPlace,
    memory,
    f32,
    i32,
  };
}

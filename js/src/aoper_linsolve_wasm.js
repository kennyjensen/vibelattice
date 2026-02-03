/*
 * WASM-backed linear solve for EXEC trim system (f64).
 * Uses the hand-written aoper_exec_port.wat exports.
 */
import fs from 'node:fs/promises';
import path from 'node:path';

function makeAllocator(start = 0) {
  let offset = start;
  const alloc = (bytes, align = 8) => {
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

export async function loadAoperLinSolveWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'aoper_exec_port.wasm');
  const wasmBytes = await readWasmBytes(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP_COL_f64, BAKSUB_COL_f64 } = instance.exports;
  let f64 = new Float64Array(memory.buffer);
  let i32 = new Int32Array(memory.buffer);
  const refreshViews = () => {
    f64 = new Float64Array(memory.buffer);
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

  function solveLinearSystem(vsys, vres, dim, n) {
    allocator.reset();
    const aPtr = allocator.alloc(vsys.length * 8, 8);
    ensureMemory(aPtr + vsys.length * 8);
    const bPtr = allocator.alloc(vres.length * 8, 8);
    ensureMemory(bPtr + vres.length * 8);
    const indxPtr = allocator.alloc((n + 1) * 4, 4);
    const workPtr = allocator.alloc((n + 1) * 8, 8);

    f64.set(vsys, aPtr / 8);
    f64.set(vres, bPtr / 8);

    LUDCMP_COL_f64(dim, n, aPtr, indxPtr, workPtr);
    BAKSUB_COL_f64(dim, n, aPtr, indxPtr, bPtr);

    const out = Float64Array.from(f64.subarray(bPtr / 8, bPtr / 8 + vres.length));
    return out;
  }

  return { solveLinearSystem };
}

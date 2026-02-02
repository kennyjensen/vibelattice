/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

export async function loadMatrixLapackDpWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'matrix-lapackdp.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP, BAKSUB } = instance.exports;
  const viewF64 = new Float64Array(memory.buffer);
  const viewI32 = new Int32Array(memory.buffer);

  function solve(A, B, n) {
    const aOffset = 0;
    const bOffset = aOffset + n * n * 8;
    const indxOffset = bOffset + n * 8;
    const workOffset = indxOffset + n * 4;

    viewF64.set(A, aOffset / 8);
    viewF64.set(B, bOffset / 8);

    LUDCMP(n, n, aOffset, indxOffset, workOffset);
    BAKSUB(n, n, aOffset, indxOffset, bOffset);

    return viewF64.slice(bOffset / 8, bOffset / 8 + n);
  }

  return { LUDCMP, BAKSUB, solve, memory };
}

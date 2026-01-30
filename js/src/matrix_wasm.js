import fs from 'node:fs/promises';
import path from 'node:path';

export async function loadMatrixWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'matrix.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP, BAKSUB } = instance.exports;
  const viewF32 = new Float32Array(memory.buffer);
  const viewI32 = new Int32Array(memory.buffer);

  function solve(A, B, n) {
    const aOffset = 0;
    const bOffset = aOffset + n * n * 4;
    const indxOffset = bOffset + n * 4;
    const workOffset = indxOffset + n * 4;

    viewF32.set(A, aOffset / 4);
    viewF32.set(B, bOffset / 4);

    LUDCMP(n, n, aOffset, indxOffset, workOffset);
    BAKSUB(n, n, aOffset, indxOffset, bOffset);

    return viewF32.slice(bOffset / 4, bOffset / 4 + n);
  }

  return { LUDCMP, BAKSUB, solve, memory };
}

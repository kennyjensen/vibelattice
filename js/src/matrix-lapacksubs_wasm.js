import fs from 'node:fs/promises';
import path from 'node:path';

export async function loadMatrixLapacksubsWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'matrix-lapacksubs.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, SGETRF, SGETRS, DGETRF, DGETRS } = instance.exports;
  const viewF32 = new Float32Array(memory.buffer);
  const viewF64 = new Float64Array(memory.buffer);
  const viewI32 = new Int32Array(memory.buffer);

  function solveSp(A, B, n) {
    const aOffset = 0;
    const bOffset = aOffset + n * n * 4;
    const ipivOffset = bOffset + n * 4;
    const infoOffset = ipivOffset + n * 4;

    viewF32.set(A, aOffset / 4);
    viewF32.set(B, bOffset / 4);

    SGETRF(n, n, aOffset, n, ipivOffset, infoOffset);
    SGETRS(78, n, 1, aOffset, n, ipivOffset, bOffset, n, infoOffset);

    return viewF32.slice(bOffset / 4, bOffset / 4 + n);
  }

  function solveDp(A, B, n) {
    const aOffset = 0;
    const bOffset = aOffset + n * n * 8;
    const ipivOffset = bOffset + n * 8;
    const infoOffset = ipivOffset + n * 4;

    viewF64.set(A, aOffset / 8);
    viewF64.set(B, bOffset / 8);

    DGETRF(n, n, aOffset, n, ipivOffset, infoOffset);
    DGETRS(78, n, 1, aOffset, n, ipivOffset, bOffset, n, infoOffset);

    return viewF64.slice(bOffset / 8, bOffset / 8 + n);
  }

  return { SGETRF, SGETRS, DGETRF, DGETRS, solveSp, solveDp, memory };
}

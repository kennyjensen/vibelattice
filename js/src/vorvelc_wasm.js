/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

const OUT_OFFSET = 0;

export async function loadVorvelcWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'vorvelc.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, VORVELC } = instance.exports;
  const view = new Float32Array(memory.buffer);

  function VORVELC_wasm(x, y, z, lbound, x1, y1, z1, x2, y2, z2, beta, rcore) {
    VORVELC(
      Math.fround(x), Math.fround(y), Math.fround(z),
      lbound ? 1 : 0,
      Math.fround(x1), Math.fround(y1), Math.fround(z1),
      Math.fround(x2), Math.fround(y2), Math.fround(z2),
      Math.fround(beta), Math.fround(rcore),
      OUT_OFFSET,
    );

    return {
      u: view[OUT_OFFSET / 4],
      v: view[OUT_OFFSET / 4 + 1],
      w: view[OUT_OFFSET / 4 + 2],
    };
  }

  return { VORVELC: VORVELC_wasm, memory };
}

/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

const A_OFFSET = 0;
const R_OFFSET = 64;
const R_A_OFFSET = 256;

export async function loadAutilWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'autil.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      cos: (x) => Math.fround(Math.cos(x)),
      sin: (x) => Math.fround(Math.sin(x)),
      tan: (x) => Math.fround(Math.tan(x)),
    },
  });

  const { memory, M3INV, RATEKI3, ROTENS3 } = instance.exports;
  const view = new Float32Array(memory.buffer);

  function M3INV_wasm(A) {
    view.set(A, A_OFFSET / 4);
    M3INV(A_OFFSET, R_OFFSET);
    return view.slice(R_OFFSET / 4, R_OFFSET / 4 + 9);
  }

  function RATEKI3_wasm(A) {
    view.set(A, A_OFFSET / 4);
    RATEKI3(A_OFFSET, R_OFFSET, R_A_OFFSET);
    const R = view.slice(R_OFFSET / 4, R_OFFSET / 4 + 9);
    const R_A = view.slice(R_A_OFFSET / 4, R_A_OFFSET / 4 + 27);
    return { R, R_A };
  }

  function ROTENS3_wasm(A) {
    view.set(A, A_OFFSET / 4);
    ROTENS3(A_OFFSET, R_OFFSET, R_A_OFFSET);
    const T = view.slice(R_OFFSET / 4, R_OFFSET / 4 + 9);
    const T_A = view.slice(R_A_OFFSET / 4, R_A_OFFSET / 4 + 27);
    return { T, T_A };
  }

  return { M3INV: M3INV_wasm, RATEKI3: RATEKI3_wasm, ROTENS3: ROTENS3_wasm, memory };
}

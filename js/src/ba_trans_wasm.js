/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

const P_OFFSET = 0;
const P_A_OFFSET = 64;
const P_B_OFFSET = 128;

export async function loadBaTransWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'ba_trans.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      sin: (x) => Math.fround(Math.sin(Math.fround(x))),
      cos: (x) => Math.fround(Math.cos(Math.fround(x))),
    },
  });
  const { memory, BA2WA_MAT, BA2SA_MAT } = instance.exports;
  const view = new Float32Array(memory.buffer);

  function copyMat(offset) {
    return Float32Array.from(view.subarray(offset / 4, offset / 4 + 9));
  }

  function BA2WA_MAT_wasm(alfa, beta, binv) {
    BA2WA_MAT(Math.fround(alfa), Math.fround(beta), Math.fround(binv), P_OFFSET, P_A_OFFSET, P_B_OFFSET);
    return {
      P: copyMat(P_OFFSET),
      P_A: copyMat(P_A_OFFSET),
      P_B: copyMat(P_B_OFFSET),
    };
  }

  function BA2SA_MAT_wasm(alfa) {
    BA2SA_MAT(Math.fround(alfa), P_OFFSET, P_A_OFFSET);
    return {
      P: copyMat(P_OFFSET),
      P_A: copyMat(P_A_OFFSET),
    };
  }

  return { BA2WA_MAT: BA2WA_MAT_wasm, BA2SA_MAT: BA2SA_MAT_wasm, memory };
}

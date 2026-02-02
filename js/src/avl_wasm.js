/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

export async function loadAvlWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'avl.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, AVL_INIT } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);

  function avlInitSnapshot() {
    const outPtr = 0;
    AVL_INIT(outPtr);

    return {
      VERSION: f32[0],
      PI: f32[1],
      DTR: f32[2],
      LUINP: i32[3],
      LURUN: i32[4],
      LUOUT: i32[5],
      LUSTD: i32[6],
      LUSYS: i32[7],
      RMASS0: f32[8],
      NDEFINI: i32[9],
      NMASINI: i32[10],
      NHEAP: i32[11],
      NPLINIT: i32[12],
      NPLCLOSE: i32[13],
    };
  }

  return { avlInitSnapshot };
}

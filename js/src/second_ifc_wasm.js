/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import path from 'node:path';
import { loadSecondWasm } from './second_wasm.js';

export async function loadSecondIfcWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'second_ifc.wasm');
  return loadSecondWasm({ ...options, wasmPath });
}

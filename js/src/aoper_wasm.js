/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';
import { EXEC } from './aoper.js';

export async function loadAoperWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'aoper.wasm');
  const wasmBytes = await fs.readFile(wasmPath);

  let currentState = null;
  const imports = {
    env: {
      exec_js: (niter, info, ir) => {
        if (!currentState) {
          throw new Error('EXEC wasm called without a bound state');
        }
        EXEC(currentState, niter, info, ir);
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const { EXEC: EXEC_WASM } = instance.exports;

  function EXEC_wasm(state, niter = 0, info = 0, ir = 1) {
    currentState = state;
    EXEC_WASM(niter, info, ir, 0);
    currentState = null;
    return state;
  }

  return { EXEC_wasm };
}

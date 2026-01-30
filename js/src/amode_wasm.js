import fs from 'node:fs/promises';
import path from 'node:path';
import { RUNCHK, SYSMAT, APPMAT, SYSSHO } from './amode.js';

export async function loadAmodeWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'amode.wasm');
  const wasmBytes = await fs.readFile(wasmPath);

  let currentState = null;
  let lastResult = null;

  const imports = {
    env: {
      runchk_js: (run) => {
        lastResult = RUNCHK(currentState, run);
      },
      sysmat_js: (ir) => {
        const { ASYS, BSYS, RSYS } = currentState.__amode;
        lastResult = SYSMAT(currentState, ir, ASYS, BSYS, RSYS);
      },
      appmat_js: (ir) => {
        const { ASYS, BSYS, RSYS } = currentState.__amode;
        lastResult = APPMAT(currentState, ir, ASYS, BSYS, RSYS);
      },
      syssho_js: (n) => {
        const { ASYS, BSYS, RSYS } = currentState.__amode;
        lastResult = SYSSHO(currentState, ASYS, BSYS, RSYS, n);
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const { RUNCHK: RUNCHK_WASM, SYSMAT: SYSMAT_WASM, APPMAT: APPMAT_WASM, SYSSHO: SYSSHO_WASM } = instance.exports;

  function bind(state) {
    currentState = state;
    if (!currentState.__amode) {
      currentState.__amode = {
        ASYS: new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1)),
        BSYS: new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1)),
        RSYS: new Float32Array(state.JEMAX + 1),
      };
    }
  }

  function RUNCHK_wasm(state, run) {
    bind(state);
    RUNCHK_WASM(run);
    return lastResult;
  }

  function SYSMAT_wasm(state, ir) {
    bind(state);
    SYSMAT_WASM(ir);
    return lastResult;
  }

  function APPMAT_wasm(state, ir) {
    bind(state);
    APPMAT_WASM(ir);
    return lastResult;
  }

  function SYSSHO_wasm(state, nsys) {
    bind(state);
    SYSSHO_WASM(nsys);
    return lastResult;
  }

  return { RUNCHK_wasm, SYSMAT_wasm, APPMAT_wasm, SYSSHO_wasm };
}

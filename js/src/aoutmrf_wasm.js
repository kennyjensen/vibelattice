import fs from 'node:fs/promises';
import path from 'node:path';
import { MRFTOT, MRFSURF, MRFBODY, MRFSTRP, MRFELE, MRFHINGE, MRFCNC, MRFVM } from './aoutmrf.js';

export async function loadAoutmrfWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'aoutmrf.wasm');
  const wasmBytes = await fs.readFile(wasmPath);

  let currentState = null;
  let currentFileid = '';
  let lastOutput = '';

  const imports = {
    env: {
      mrftot_js: (lun) => {
        lastOutput = MRFTOT(currentState, lun, currentFileid);
      },
      mrfsurf_js: (lun) => {
        lastOutput = MRFSURF(currentState, lun);
      },
      mrfbody_js: (lun) => {
        lastOutput = MRFBODY(currentState, lun);
      },
      mrfstrp_js: (lun) => {
        lastOutput = MRFSTRP(currentState, lun);
      },
      mrfele_js: (lun) => {
        lastOutput = MRFELE(currentState, lun);
      },
      mrfhinge_js: (lun) => {
        lastOutput = MRFHINGE(currentState, lun);
      },
      mrfcnd_js: (lun) => {
        lastOutput = MRFCNC(currentState, lun);
      },
      mrfvm_js: (lun) => {
        lastOutput = MRFVM(currentState, lun);
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const {
    MRFTOT: MRFTOT_WASM,
    MRFSURF: MRFSURF_WASM,
    MRFBODY: MRFBODY_WASM,
    MRFSTRP: MRFSTRP_WASM,
    MRFELE: MRFELE_WASM,
    MRFHINGE: MRFHINGE_WASM,
    MRFCNC: MRFCNC_WASM,
    MRFVM: MRFVM_WASM,
  } = instance.exports;

  function bind(state) {
    currentState = state;
  }

  function MRFTOT_wasm(state, lun = 6, fileid = '') {
    bind(state);
    currentFileid = fileid;
    MRFTOT_WASM(lun);
    return lastOutput;
  }
  function MRFSURF_wasm(state, lun = 6) {
    bind(state);
    MRFSURF_WASM(lun);
    return lastOutput;
  }
  function MRFBODY_wasm(state, lun = 6) {
    bind(state);
    MRFBODY_WASM(lun);
    return lastOutput;
  }
  function MRFSTRP_wasm(state, lun = 6) {
    bind(state);
    MRFSTRP_WASM(lun);
    return lastOutput;
  }
  function MRFELE_wasm(state, lun = 6) {
    bind(state);
    MRFELE_WASM(lun);
    return lastOutput;
  }
  function MRFHINGE_wasm(state, lun = 6) {
    bind(state);
    MRFHINGE_WASM(lun);
    return lastOutput;
  }
  function MRFCNC_wasm(state, lun = 6) {
    bind(state);
    MRFCNC_WASM(lun);
    return lastOutput;
  }
  function MRFVM_wasm(state, lun = 6) {
    bind(state);
    MRFVM_WASM(lun);
    return lastOutput;
  }

  return {
    MRFTOT_wasm,
    MRFSURF_wasm,
    MRFBODY_wasm,
    MRFSTRP_wasm,
    MRFELE_wasm,
    MRFHINGE_wasm,
    MRFCNC_wasm,
    MRFVM_wasm,
  };
}

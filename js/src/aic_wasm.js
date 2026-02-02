/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

const OFFSETS = {
  RV1: 0,
  RV2: 256,
  CHORDV: 512,
  NCOMPV: 640,
  RC: 768,
  NCOMPC: 1024,
  WC_GAM: 1280,
  XYZREF: 1792,
  LFRST: 1824,
  NL: 1856,
  RL: 1888,
  RADL: 2400,
  SRC_U: 2560,
  DBL_U: 2816,
  WC_U: 3584,
  UVWS: 4352,
  UVWD: 4384,
  OUT: 4480,
};

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function writeI32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadAicWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'aic.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, VORVELC, SRDVELC, SRDSET, VSRD, VVOR } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);

  function VORVELC_wasm(X, Y, Z, LBOUND, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE) {
    VORVELC(Math.fround(X), Math.fround(Y), Math.fround(Z), LBOUND ? 1 : 0,
      Math.fround(X1), Math.fround(Y1), Math.fround(Z1),
      Math.fround(X2), Math.fround(Y2), Math.fround(Z2),
      Math.fround(BETA), Math.fround(RCORE),
      OFFSETS.OUT);
    return readF32(f32, OFFSETS.OUT, 3);
  }

  function SRDVELC_wasm(X, Y, Z, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE) {
    SRDVELC(Math.fround(X), Math.fround(Y), Math.fround(Z),
      Math.fround(X1), Math.fround(Y1), Math.fround(Z1),
      Math.fround(X2), Math.fround(Y2), Math.fround(Z2),
      Math.fround(BETA), Math.fround(RCORE),
      OFFSETS.UVWS, OFFSETS.UVWD);
    return {
      UVWS: readF32(f32, OFFSETS.UVWS, 3),
      UVWD: readF32(f32, OFFSETS.UVWD, 9),
    };
  }

  function SRDSET_wasm(BETM, XYZREF, IYSYM, NBODY, LFRST, NLDIM, NL, RL, RADL) {
    writeF32(f32, OFFSETS.XYZREF, XYZREF);
    writeI32(i32, OFFSETS.LFRST, LFRST);
    writeI32(i32, OFFSETS.NL, NL);
    writeF32(f32, OFFSETS.RL, RL);
    writeF32(f32, OFFSETS.RADL, RADL);

    SRDSET(Math.fround(BETM), OFFSETS.XYZREF, IYSYM, NBODY, OFFSETS.LFRST, NLDIM,
      OFFSETS.NL, OFFSETS.RL, OFFSETS.RADL, OFFSETS.SRC_U, OFFSETS.DBL_U);

    return {
      SRC_U: readF32(f32, OFFSETS.SRC_U, NLDIM * 6),
      DBL_U: readF32(f32, OFFSETS.DBL_U, 3 * NLDIM * 6),
    };
  }

  function VSRD_wasm(BETM, IYSYM, YSYM, IZSYM, ZSYM, SRCORE,
    NBODY, LFRST, NLDIM, NL, RL, RADL,
    NU, SRC_U, DBL_U,
    NC, RC, NCDIM) {
    writeI32(i32, OFFSETS.LFRST, LFRST);
    writeI32(i32, OFFSETS.NL, NL);
    writeF32(f32, OFFSETS.RL, RL);
    writeF32(f32, OFFSETS.RADL, RADL);
    writeF32(f32, OFFSETS.SRC_U, SRC_U);
    writeF32(f32, OFFSETS.DBL_U, DBL_U);
    writeF32(f32, OFFSETS.RC, RC);

    VSRD(Math.fround(BETM), IYSYM, Math.fround(YSYM), IZSYM, Math.fround(ZSYM), Math.fround(SRCORE),
      NBODY, OFFSETS.LFRST, NLDIM, OFFSETS.NL, OFFSETS.RL, OFFSETS.RADL,
      NU, OFFSETS.SRC_U, OFFSETS.DBL_U,
      NC, OFFSETS.RC, OFFSETS.WC_U, NCDIM);

    return readF32(f32, OFFSETS.WC_U, 3 * NCDIM * NU);
  }

  function VVOR_wasm(BETM, IYSYM, YSYM, IZSYM, ZSYM, VRCOREC, VRCOREW,
    NV, RV1, RV2, NCOMPV, CHORDV,
    NC, RC, NCOMPC, LVTEST, NCDIM) {
    writeF32(f32, OFFSETS.RV1, RV1);
    writeF32(f32, OFFSETS.RV2, RV2);
    writeI32(i32, OFFSETS.NCOMPV, NCOMPV);
    writeF32(f32, OFFSETS.CHORDV, CHORDV);
    writeF32(f32, OFFSETS.RC, RC);
    writeI32(i32, OFFSETS.NCOMPC, NCOMPC);

    VVOR(Math.fround(BETM), IYSYM, Math.fround(YSYM), IZSYM, Math.fround(ZSYM),
      Math.fround(VRCOREC), Math.fround(VRCOREW),
      NV, OFFSETS.RV1, OFFSETS.RV2, OFFSETS.NCOMPV, OFFSETS.CHORDV,
      NC, OFFSETS.RC, OFFSETS.NCOMPC, LVTEST ? 1 : 0,
      OFFSETS.WC_GAM, NCDIM);

    return readF32(f32, OFFSETS.WC_GAM, 3 * NCDIM * NV);
  }

  return { VORVELC: VORVELC_wasm, SRDVELC: SRDVELC_wasm, SRDSET: SRDSET_wasm, VSRD: VSRD_wasm, VVOR: VVOR_wasm, memory };
}

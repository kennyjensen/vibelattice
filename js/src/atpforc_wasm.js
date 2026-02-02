/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

function makeAllocator(start = 1024) {
  let offset = start;
  return function alloc(bytes, align = 4) {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
}

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function writeI32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadAtpforcWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'atpforc.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const imports = {
    env: {
      sin_f32: (x) => Math.fround(Math.sin(x)),
      cos_f32: (x) => Math.fround(Math.cos(x)),
    },
  };
  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const { memory, TPFORC, PGMAT } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);

  function TPFORC_wasm(state) {
    const {
      PI,
      AMACH,
      YSYM,
      ZSYM,
      IYSYM,
      IZSYM,
      VRCOREC,
      VRCOREW,
      NSTRIP,
      NUMAX,
      SREF,
      BREF,
      IJFRST,
      NVSTRP,
      GAM,
      GAM_U,
      RV1,
      RV2,
      RC,
      CHORD,
      LSSURF,
      LNCOMP,
      LFLOAD,
    } = state;

    const alloc = makeAllocator();
    const IJFRST_OFF = alloc(IJFRST.length * 4);
    const NVSTRP_OFF = alloc(NVSTRP.length * 4);
    const GAM_OFF = alloc(GAM.length * 4);
    const GAM_U_OFF = alloc(GAM_U.length * 4);
    const RV1_OFF = alloc(RV1.length * 4);
    const RV2_OFF = alloc(RV2.length * 4);
    const RC_OFF = alloc(RC.length * 4);
    const CHORD_OFF = alloc(CHORD.length * 4);
    const LSSURF_OFF = alloc(LSSURF.length * 4);
    const LNCOMP_OFF = alloc(LNCOMP.length * 4);
    const LFLOAD_OFF = alloc(LFLOAD.length * 4);

    const GAMS_OFF = alloc(NSTRIP * 4);
    const GAMS_U_OFF = alloc(NSTRIP * NUMAX * 4);
    const RT1_OFF = alloc(3 * NSTRIP * 4);
    const RT2_OFF = alloc(3 * NSTRIP * 4);
    const RTC_OFF = alloc(3 * NSTRIP * 4);
    const VY_U_OFF = alloc(NUMAX * 4);
    const VZ_U_OFF = alloc(NUMAX * 4);
    const DWWAKE_OFF = alloc(NSTRIP * 4);
    const CLFF_U_OFF = alloc(NUMAX * 4);
    const CYFF_U_OFF = alloc(NUMAX * 4);
    const CDFF_U_OFF = alloc(NUMAX * 4);
    const SPANEF_U_OFF = alloc(NUMAX * 4);
    const OUT_OFF = alloc(16);

    writeI32(i32, IJFRST_OFF, IJFRST);
    writeI32(i32, NVSTRP_OFF, NVSTRP);
    writeF32(f32, GAM_OFF, GAM);
    writeF32(f32, GAM_U_OFF, GAM_U);
    writeF32(f32, RV1_OFF, RV1);
    writeF32(f32, RV2_OFF, RV2);
    writeF32(f32, RC_OFF, RC);
    writeF32(f32, CHORD_OFF, CHORD);
    writeI32(i32, LSSURF_OFF, LSSURF);
    writeI32(i32, LNCOMP_OFF, LNCOMP);
    writeI32(i32, LFLOAD_OFF, LFLOAD);

    TPFORC(
      Math.fround(PI),
      Math.fround(AMACH),
      Math.fround(YSYM),
      Math.fround(ZSYM),
      IYSYM | 0,
      IZSYM | 0,
      Math.fround(VRCOREC),
      Math.fround(VRCOREW),
      NSTRIP | 0,
      NUMAX | 0,
      Math.fround(SREF),
      Math.fround(BREF),
      IJFRST_OFF,
      NVSTRP_OFF,
      GAM_OFF,
      GAM_U_OFF,
      RV1_OFF,
      RV2_OFF,
      RC_OFF,
      CHORD_OFF,
      LSSURF_OFF,
      LNCOMP_OFF,
      LFLOAD_OFF,
      GAMS_OFF,
      GAMS_U_OFF,
      RT1_OFF,
      RT2_OFF,
      RTC_OFF,
      VY_U_OFF,
      VZ_U_OFF,
      DWWAKE_OFF,
      CLFF_U_OFF,
      CYFF_U_OFF,
      CDFF_U_OFF,
      SPANEF_U_OFF,
      OUT_OFF,
    );

    return {
      CLFF: f32[OUT_OFF / 4],
      CYFF: f32[OUT_OFF / 4 + 1],
      CDFF: f32[OUT_OFF / 4 + 2],
      SPANEF: f32[OUT_OFF / 4 + 3],
      DWWAKE: readF32(f32, DWWAKE_OFF, NSTRIP),
      CLFF_U: readF32(f32, CLFF_U_OFF, NUMAX),
      CYFF_U: readF32(f32, CYFF_U_OFF, NUMAX),
      CDFF_U: readF32(f32, CDFF_U_OFF, NUMAX),
      SPANEF_U: readF32(f32, SPANEF_U_OFF, NUMAX),
    };
  }

  return { TPFORC: TPFORC_wasm, PGMAT, memory };
}

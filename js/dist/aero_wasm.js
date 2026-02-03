/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import { CROSS, DOT } from './aic.js';
import { CDCL } from './cdcl.js';
import { TPFORC } from './atpforc.js';
import { SFFORC, BDFORC } from './aero.js';

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function writeI32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

function makeAllocator(start = 4096, ensure) {
  let offset = start;
  return function alloc(bytes, align = 4) {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    const next = aligned + bytes;
    if (ensure) ensure(next);
    offset = next;
    return aligned;
  };
}

function buildOffsets() {
  return {
    ALFA: 0,
    BETA: 4,
    MACH: 8,
    AMACH: 12,
    YSYM: 16,
    ZSYM: 20,
    VRCOREC: 24,
    VRCOREW: 28,
    SREF: 32,
    CREF: 36,
    BREF: 40,
    CDREF: 44,
    VINF: 48,
    VINF_A: 52,
    VINF_B: 56,
    UREF: 60,
    UREF_A: 64,
    UREF_B: 68,
    ALFAU: 72,
    BETAD: 76,
    CLTOT_U: 80,
    CDTOT_U: 104,
    CYTOT_U: 128,
    CFTOT_U: 152,
    CMTOT_U: 224,
    CLTOT_D: 296,
    CDTOT_D: 320,
    CYTOT_D: 344,
    CFTOT_D: 368,
    CMTOT_D: 440,
    DCP: 512,
    CNC: 516,
    CFSTRP: 520,
    CMSTRP: 524,
    CDSTRP: 528,
    CYSTRP: 532,
    CLSTRP: 536,
    CDVSURF: 540,
    CDSURF: 544,
    CYSURF: 548,
    CLSURF: 552,
    CDVTOT: 556,
    CFTOT: 560,
    CMTOT: 572,
    CDTOT: 584,
    CYTOT: 588,
    CLTOT: 592,
    CLTOT_A: 596,
    CDTOT_A: 600,
    CLFF: 604,
    CYFF: 608,
    CDFF: 612,
    SPANEF: 616,
    OUTPUT_PTR: 620,
  };
}

function allocState(state, ensureMemory) {
  const alloc = makeAllocator(4096, ensureMemory);
  const offsets = buildOffsets();
  const base = alloc(2048, 4);

  const arrayOffsets = {
    VINF: alloc(12, 4),
    VINF_A: alloc(12, 4),
    VINF_B: alloc(12, 4),
    UREF: alloc(12, 4),
    UREF_A: alloc(12, 4),
    UREF_B: alloc(12, 4),
    CLTOT_U: alloc(state.CLTOT_U.length * 4, 4),
    CDTOT_U: alloc(state.CDTOT_U.length * 4, 4),
    CYTOT_U: alloc(state.CYTOT_U.length * 4, 4),
    CFTOT_U: alloc(state.CFTOT_U.length * 4, 4),
    CMTOT_U: alloc(state.CMTOT_U.length * 4, 4),
    CLTOT_D: alloc(state.CLTOT_D.length * 4, 4),
    CDTOT_D: alloc(state.CDTOT_D.length * 4, 4),
    CYTOT_D: alloc(state.CYTOT_D.length * 4, 4),
    CFTOT_D: alloc(state.CFTOT_D.length * 4, 4),
    CMTOT_D: alloc(state.CMTOT_D.length * 4, 4),
    DCP: alloc(state.DCP.length * 4, 4),
    CNC: alloc(state.CNC.length * 4, 4),
    CFSTRP: alloc(state.CFSTRP.length * 4, 4),
    CMSTRP: alloc(state.CMSTRP.length * 4, 4),
    CDSTRP: alloc(state.CDSTRP.length * 4, 4),
    CYSTRP: alloc(state.CYSTRP.length * 4, 4),
    CLSTRP: alloc(state.CLSTRP.length * 4, 4),
    CDVSURF: alloc(state.CDVSURF.length * 4, 4),
    CDSURF: alloc(state.CDSURF.length * 4, 4),
    CYSURF: alloc(state.CYSURF.length * 4, 4),
    CLSURF: alloc(state.CLSURF.length * 4, 4),
  };

  return { base, offsets, arrayOffsets };
}

function buildSignature(state) {
  const parts = [];
  for (const name of [
    'VINF',
    'VINF_A',
    'VINF_B',
    'UREF',
    'UREF_A',
    'UREF_B',
    'CLTOT_U',
    'CDTOT_U',
    'CYTOT_U',
    'CFTOT_U',
    'CMTOT_U',
    'CLTOT_D',
    'CDTOT_D',
    'CYTOT_D',
    'CFTOT_D',
    'CMTOT_D',
    'DCP',
    'CNC',
    'CFSTRP',
    'CMSTRP',
    'CDSTRP',
    'CYSTRP',
    'CLSTRP',
    'CDVSURF',
    'CDSURF',
    'CYSURF',
    'CLSURF',
  ]) {
    const arr = state[name];
    const len = arr && typeof arr.length === 'number' ? arr.length : 0;
    parts.push(`${name}:${len}`);
  }
  return parts.join('|');
}

function syncStateToMemory(state, statePtr, offsets, arrayOffsets, mem) {
  const { f32, i32 } = mem;
  f32[(statePtr + offsets.ALFA) / 4] = state.ALFA;
  f32[(statePtr + offsets.BETA) / 4] = state.BETA;
  f32[(statePtr + offsets.MACH) / 4] = state.MACH;
  f32[(statePtr + offsets.AMACH) / 4] = state.AMACH;
  f32[(statePtr + offsets.YSYM) / 4] = state.YSYM;
  f32[(statePtr + offsets.ZSYM) / 4] = state.ZSYM;
  f32[(statePtr + offsets.VRCOREC) / 4] = state.VRCOREC;
  f32[(statePtr + offsets.VRCOREW) / 4] = state.VRCOREW;
  f32[(statePtr + offsets.SREF) / 4] = state.SREF;
  f32[(statePtr + offsets.CREF) / 4] = state.CREF;
  f32[(statePtr + offsets.BREF) / 4] = state.BREF;
  f32[(statePtr + offsets.CDREF) / 4] = state.CDREF;
  i32[(statePtr + offsets.VINF) / 4] = arrayOffsets.VINF;
  i32[(statePtr + offsets.VINF_A) / 4] = arrayOffsets.VINF_A;
  i32[(statePtr + offsets.VINF_B) / 4] = arrayOffsets.VINF_B;
  i32[(statePtr + offsets.UREF) / 4] = arrayOffsets.UREF;
  i32[(statePtr + offsets.UREF_A) / 4] = arrayOffsets.UREF_A;
  i32[(statePtr + offsets.UREF_B) / 4] = arrayOffsets.UREF_B;
  i32[(statePtr + offsets.CLTOT_U) / 4] = arrayOffsets.CLTOT_U;
  i32[(statePtr + offsets.CDTOT_U) / 4] = arrayOffsets.CDTOT_U;
  i32[(statePtr + offsets.CYTOT_U) / 4] = arrayOffsets.CYTOT_U;
  i32[(statePtr + offsets.CFTOT_U) / 4] = arrayOffsets.CFTOT_U;
  i32[(statePtr + offsets.CMTOT_U) / 4] = arrayOffsets.CMTOT_U;
  i32[(statePtr + offsets.CLTOT_D) / 4] = arrayOffsets.CLTOT_D;
  i32[(statePtr + offsets.CDTOT_D) / 4] = arrayOffsets.CDTOT_D;
  i32[(statePtr + offsets.CYTOT_D) / 4] = arrayOffsets.CYTOT_D;
  i32[(statePtr + offsets.CFTOT_D) / 4] = arrayOffsets.CFTOT_D;
  i32[(statePtr + offsets.CMTOT_D) / 4] = arrayOffsets.CMTOT_D;
  i32[(statePtr + offsets.DCP) / 4] = arrayOffsets.DCP;
  i32[(statePtr + offsets.CNC) / 4] = arrayOffsets.CNC;
  i32[(statePtr + offsets.CFSTRP) / 4] = arrayOffsets.CFSTRP;
  i32[(statePtr + offsets.CMSTRP) / 4] = arrayOffsets.CMSTRP;
  i32[(statePtr + offsets.CDSTRP) / 4] = arrayOffsets.CDSTRP;
  i32[(statePtr + offsets.CYSTRP) / 4] = arrayOffsets.CYSTRP;
  i32[(statePtr + offsets.CLSTRP) / 4] = arrayOffsets.CLSTRP;
  i32[(statePtr + offsets.CDVSURF) / 4] = arrayOffsets.CDVSURF;
  i32[(statePtr + offsets.CDSURF) / 4] = arrayOffsets.CDSURF;
  i32[(statePtr + offsets.CYSURF) / 4] = arrayOffsets.CYSURF;
  i32[(statePtr + offsets.CLSURF) / 4] = arrayOffsets.CLSURF;

  writeF32(f32, arrayOffsets.VINF, state.VINF);
  writeF32(f32, arrayOffsets.VINF_A, state.VINF_A);
  writeF32(f32, arrayOffsets.VINF_B, state.VINF_B);
  writeF32(f32, arrayOffsets.UREF, state.UREF);
  writeF32(f32, arrayOffsets.UREF_A, state.UREF_A);
  writeF32(f32, arrayOffsets.UREF_B, state.UREF_B);
  writeF32(f32, arrayOffsets.CLTOT_U, state.CLTOT_U);
  writeF32(f32, arrayOffsets.CDTOT_U, state.CDTOT_U);
  writeF32(f32, arrayOffsets.CYTOT_U, state.CYTOT_U);
  writeF32(f32, arrayOffsets.CFTOT_U, state.CFTOT_U);
  writeF32(f32, arrayOffsets.CMTOT_U, state.CMTOT_U);
  writeF32(f32, arrayOffsets.CLTOT_D, state.CLTOT_D);
  writeF32(f32, arrayOffsets.CDTOT_D, state.CDTOT_D);
  writeF32(f32, arrayOffsets.CYTOT_D, state.CYTOT_D);
  writeF32(f32, arrayOffsets.CFTOT_D, state.CFTOT_D);
  writeF32(f32, arrayOffsets.CMTOT_D, state.CMTOT_D);
  writeF32(f32, arrayOffsets.DCP, state.DCP);
  writeF32(f32, arrayOffsets.CNC, state.CNC);
  writeF32(f32, arrayOffsets.CFSTRP, state.CFSTRP);
  writeF32(f32, arrayOffsets.CMSTRP, state.CMSTRP);
  writeF32(f32, arrayOffsets.CDSTRP, state.CDSTRP);
  writeF32(f32, arrayOffsets.CYSTRP, state.CYSTRP);
  writeF32(f32, arrayOffsets.CLSTRP, state.CLSTRP);
  writeF32(f32, arrayOffsets.CDVSURF, state.CDVSURF);
  writeF32(f32, arrayOffsets.CDSURF, state.CDSURF);
  writeF32(f32, arrayOffsets.CYSURF, state.CYSURF);
  writeF32(f32, arrayOffsets.CLSURF, state.CLSURF);
}

function syncStateFromMemory(state, statePtr, offsets, arrayOffsets, mem) {
  const { f32 } = mem;
  state.CDTOT = f32[(statePtr + offsets.CDTOT) / 4];
  state.CYTOT = f32[(statePtr + offsets.CYTOT) / 4];
  state.CLTOT = f32[(statePtr + offsets.CLTOT) / 4];
  state.CDVTOT = f32[(statePtr + offsets.CDVTOT) / 4];
  state.CLTOT_A = f32[(statePtr + offsets.CLTOT_A) / 4];
  state.CDTOT_A = f32[(statePtr + offsets.CDTOT_A) / 4];
  state.CLFF = f32[(statePtr + offsets.CLFF) / 4];
  state.CYFF = f32[(statePtr + offsets.CYFF) / 4];
  state.CDFF = f32[(statePtr + offsets.CDFF) / 4];
  state.SPANEF = f32[(statePtr + offsets.SPANEF) / 4];
  state.CFTOT = readF32(f32, arrayOffsets.CFTOT, 3);
  state.CMTOT = readF32(f32, arrayOffsets.CMTOT, 3);
  state.DCP = readF32(f32, arrayOffsets.DCP, state.DCP.length);
  state.CNC = readF32(f32, arrayOffsets.CNC, state.CNC.length);
  state.CFSTRP = readF32(f32, arrayOffsets.CFSTRP, state.CFSTRP.length);
  state.CMSTRP = readF32(f32, arrayOffsets.CMSTRP, state.CMSTRP.length);
  state.CDSTRP = readF32(f32, arrayOffsets.CDSTRP, state.CDSTRP.length);
  state.CYSTRP = readF32(f32, arrayOffsets.CYSTRP, state.CYSTRP.length);
  state.CLSTRP = readF32(f32, arrayOffsets.CLSTRP, state.CLSTRP.length);
  state.CDVSURF = readF32(f32, arrayOffsets.CDVSURF, state.CDVSURF.length);
  state.CDSURF = readF32(f32, arrayOffsets.CDSURF, state.CDSURF.length);
  state.CYSURF = readF32(f32, arrayOffsets.CYSURF, state.CYSURF.length);
  state.CLSURF = readF32(f32, arrayOffsets.CLSURF, state.CLSURF.length);
}

export async function loadAeroWasm() {
  const url = new URL('./aero.wasm', import.meta.url);
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
  const wasmBytes = await res.arrayBuffer();
  const imports = {
    env: {
      __mem: {},
      sin_f32: (x) => Math.fround(Math.sin(x)),
      cos_f32: (x) => Math.fround(Math.cos(x)),
      sqrt_f32: (x) => Math.fround(Math.sqrt(x)),
      cross: (uPtr, vPtr, wPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const u = Float32Array.from(f32.subarray(uPtr / 4, uPtr / 4 + 3));
        const v = Float32Array.from(f32.subarray(vPtr / 4, vPtr / 4 + 3));
        const out = CROSS(u, v);
        f32[wPtr / 4] = out[0];
        f32[wPtr / 4 + 1] = out[1];
        f32[wPtr / 4 + 2] = out[2];
      },
      dot: (uPtr, vPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const u = Float32Array.from(f32.subarray(uPtr / 4, uPtr / 4 + 3));
        const v = Float32Array.from(f32.subarray(vPtr / 4, vPtr / 4 + 3));
        return DOT(u, v);
      },
      cdcl: (clcdPtr, clv, outPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const clcd = Float32Array.from(f32.subarray(clcdPtr / 4, clcdPtr / 4 + 6));
        const res = CDCL(clcd, clv);
        f32[outPtr / 4] = res.cd;
        f32[outPtr / 4 + 1] = res.cd_cl;
      },
      tpforc_js: () => {
        const mem = imports.env.__mem;
        if (typeof mem.syncStateFromMemory === 'function') {
          mem.syncStateFromMemory();
        }
        const state = mem.state;
        const res = TPFORC(state);
        state.CLFF = res.CLFF;
        state.CYFF = res.CYFF;
        state.CDFF = res.CDFF;
        state.SPANEF = res.SPANEF;
        state.DWWAKE.set(res.DWWAKE);
        state.CLFF_U.set(res.CLFF_U);
        state.CYFF_U.set(res.CYFF_U);
        state.CDFF_U.set(res.CDFF_U);
        state.SPANEF_U.set(res.SPANEF_U);
        mem.syncState();
      },
      sfforc_js: () => {
        const mem = imports.env.__mem;
        const state = mem.state;
        SFFORC(state);
        mem.syncState();
      },
      bdforc_js: () => {
        const mem = imports.env.__mem;
        const state = mem.state;
        BDFORC(state);
        mem.syncState();
      },
    },
  };
  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const { memory, AERO, VINFAB, set_sfforc_js } = instance.exports;
  let f32 = new Float32Array(memory.buffer);
  let i32 = new Int32Array(memory.buffer);
  const refreshViews = () => {
    f32 = new Float32Array(memory.buffer);
    i32 = new Int32Array(memory.buffer);
  };
  const ensureMemory = (bytes) => {
    const minBytes = 32 * 1024 * 1024;
    const target = Math.max(bytes, minBytes);
    const needed = target - memory.buffer.byteLength;
    if (needed <= 0) return;
    const pages = Math.ceil(needed / 65536);
    memory.grow(pages);
    refreshViews();
  };
  imports.env.__mem.f32 = f32;
  imports.env.__mem.i32 = i32;

  if (typeof set_sfforc_js === 'function') {
    set_sfforc_js(0);
  }

  const cacheByState = new WeakMap();

  function getStateCache(state) {
    const signature = buildSignature(state);
    const cached = cacheByState.get(state);
    if (cached && cached.signature === signature) return cached;
    const mem = allocState(state, ensureMemory);
    const outputBase = mem.base + 2048;
    writeI32(i32, mem.base + mem.offsets.OUTPUT_PTR, [outputBase]);
    const next = { ...mem, outputBase, signature };
    cacheByState.set(state, next);
    return next;
  }

  function VINFAB_wasm(state) {
    const mem = getStateCache(state);
    imports.env.__mem.state = state;
    imports.env.__mem.statePtr = mem.base;
    imports.env.__mem.offsets = mem.offsets;
    imports.env.__mem.arrayOffsets = mem.arrayOffsets;
    imports.env.__mem.syncState = () => syncStateToMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    imports.env.__mem.syncStateFromMemory = () => syncStateFromMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    syncStateToMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    VINFAB(mem.base);
    const vinfPtr = i32[(mem.base + mem.offsets.VINF) / 4];
    const vinfAPtr = i32[(mem.base + mem.offsets.VINF_A) / 4];
    const vinfBPtr = i32[(mem.base + mem.offsets.VINF_B) / 4];
    return {
      VINF: readF32(f32, vinfPtr, 3),
      VINF_A: readF32(f32, vinfAPtr, 3),
      VINF_B: readF32(f32, vinfBPtr, 3),
    };
  }

  function AERO_wasm(state) {
    const mem = getStateCache(state);
    imports.env.__mem.state = state;
    imports.env.__mem.statePtr = mem.base;
    imports.env.__mem.offsets = mem.offsets;
    imports.env.__mem.arrayOffsets = mem.arrayOffsets;
    imports.env.__mem.syncState = () => syncStateToMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    imports.env.__mem.syncStateFromMemory = () => syncStateFromMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    syncStateToMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    AERO(mem.base);
    syncStateFromMemory(state, mem.base, mem.offsets, mem.arrayOffsets, { f32, i32 });
    return state;
  }

  return { VINFAB: VINFAB_wasm, AERO: AERO_wasm, memory };
}

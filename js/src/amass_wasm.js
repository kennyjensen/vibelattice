/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

function makeAllocator(start = 0) {
  let offset = start;
  const alloc = (bytes, align = 4) => {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
  const reset = () => {
    offset = start;
  };
  return { alloc, reset };
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

function readI32(view, offset, length) {
  return Int32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadAmassWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'amass.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, MASINI, UNITSET, APPGET, MASPUT } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);
  const allocator = makeAllocator(0);

  function allocF32(len) {
    return allocator.alloc(len * 4);
  }

  function allocI32(len) {
    return allocator.alloc(len * 4);
  }

  function MASINI_wasm(state) {
    allocator.reset();
    const rmassPtr = allocF32(1);
    const rinerPtr = allocF32(9);
    const amassPtr = allocF32(9);
    const ainerPtr = allocF32(9);
    const xyzmassPtr = allocF32(3);
    const lmassPtr = allocI32(1);

    MASINI(rmassPtr, rinerPtr, amassPtr, ainerPtr, xyzmassPtr, lmassPtr);

    state.RMASS0 = readF32(f32, rmassPtr, 1)[0];
    state.RINER0 = readF32(f32, rinerPtr, 9);
    state.AMASS = readF32(f32, amassPtr, 9);
    state.AINER = readF32(f32, ainerPtr, 9);
    state.XYZMASS0 = readF32(f32, xyzmassPtr, 3);
    state.LMASS = readI32(i32, lmassPtr, 1)[0] !== 0;
    return state;
  }

  function UNITSET_wasm(state) {
    allocator.reset();
    const unitfPtr = allocF32(1);
    const unitsPtr = allocF32(1);
    const unitvPtr = allocF32(1);
    const unitaPtr = allocF32(1);
    const unitiPtr = allocF32(1);
    const unitdPtr = allocF32(1);

    UNITSET(Math.fround(state.UNITL), Math.fround(state.UNITM), Math.fround(state.UNITT),
      unitfPtr, unitsPtr, unitvPtr, unitaPtr, unitiPtr, unitdPtr);

    state.UNITF = readF32(f32, unitfPtr, 1)[0];
    state.UNITS = readF32(f32, unitsPtr, 1)[0];
    state.UNITV = readF32(f32, unitvPtr, 1)[0];
    state.UNITA = readF32(f32, unitaPtr, 1)[0];
    state.UNITI = readF32(f32, unitiPtr, 1)[0];
    state.UNITD = readF32(f32, unitdPtr, 1)[0];
    return state;
  }

  function APPGET_wasm(state) {
    allocator.reset();
    const chordPtr = allocF32(state.CHORD.length);
    const wstripPtr = allocF32(state.WSTRIP.length);
    const ensyPtr = allocF32(state.ENSY.length);
    const enszPtr = allocF32(state.ENSZ.length);
    const rle1Ptr = allocF32(state.RLE1.length);
    const rle2Ptr = allocF32(state.RLE2.length);
    const chord1Ptr = allocF32(state.CHORD1.length);
    const chord2Ptr = allocF32(state.CHORD2.length);
    const rlePtr = allocF32(state.RLE.length);
    const amassPtr = allocF32(9);
    const ainerPtr = allocF32(9);

    writeF32(f32, chordPtr, state.CHORD);
    writeF32(f32, wstripPtr, state.WSTRIP);
    writeF32(f32, ensyPtr, state.ENSY);
    writeF32(f32, enszPtr, state.ENSZ);
    writeF32(f32, rle1Ptr, state.RLE1);
    writeF32(f32, rle2Ptr, state.RLE2);
    writeF32(f32, chord1Ptr, state.CHORD1);
    writeF32(f32, chord2Ptr, state.CHORD2);
    writeF32(f32, rlePtr, state.RLE);

    APPGET(state.NSTRIP, Math.fround(state.UNITL), Math.fround(Math.PI),
      chordPtr, wstripPtr, ensyPtr, enszPtr,
      rle1Ptr, rle2Ptr, chord1Ptr, chord2Ptr, rlePtr,
      amassPtr, ainerPtr);

    state.AMASS = readF32(f32, amassPtr, 9);
    state.AINER = readF32(f32, ainerPtr, 9);
    return state;
  }

  function MASPUT_wasm(state, IR1, IR2) {
    allocator.reset();
    const rinerPtr = allocF32(9);
    const xyzmassPtr = allocF32(3);
    const parvalPtr = allocF32(state.PARVAL.length);

    writeF32(f32, rinerPtr, state.RINER0);
    writeF32(f32, xyzmassPtr, state.XYZMASS0);
    writeF32(f32, parvalPtr, state.PARVAL);

    MASPUT(Math.fround(state.RMASS0), rinerPtr,
      Math.fround(state.GEE0), Math.fround(state.RHO0),
      xyzmassPtr, Math.fround(state.UNITL),
      parvalPtr, state.IPTOT, IR1, IR2,
      state.IPMASS, state.IPIXX, state.IPIYY, state.IPIZZ,
      state.IPIXY, state.IPIYZ, state.IPIZX,
      state.IPGEE, state.IPRHO,
      state.IPXCG, state.IPYCG, state.IPZCG);

    state.PARVAL = readF32(f32, parvalPtr, state.PARVAL.length);
    return state;
  }

  return {
    MASINI: MASINI_wasm,
    UNITSET: UNITSET_wasm,
    APPGET: APPGET_wasm,
    MASPUT: MASPUT_wasm,
    memory,
  };
}

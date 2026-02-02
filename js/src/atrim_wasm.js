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

export async function loadAtrimWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'atrim.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      sin_f32: (x) => Math.fround(Math.sin(Math.fround(x))),
      cos_f32: (x) => Math.fround(Math.cos(Math.fround(x))),
    },
  });

  const { memory, TRMSET_CORE } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);
  const allocator = makeAllocator(0);

  function allocF32(len) {
    return allocator.alloc(len * 4);
  }

  function allocI32(len) {
    return allocator.alloc(len * 4);
  }

  function TRMSET_CORE_wasm(state, KTRIM, IR1, IR2, IR) {
    allocator.reset();
    const parvalPtr = allocF32(state.PARVAL.length);
    const convalPtr = allocF32(state.CONVAL.length);
    const iconPtr = allocI32(state.ICON.length);
    const itrimPtr = allocI32(state.ITRIM.length);

    writeF32(f32, parvalPtr, state.PARVAL);
    writeF32(f32, convalPtr, state.CONVAL);
    writeI32(i32, iconPtr, Int32Array.from(state.ICON));
    writeI32(i32, itrimPtr, Int32Array.from(state.ITRIM));

    TRMSET_CORE(
      KTRIM, IR1, IR2, IR,
      state.NVTOT, state.DTR, state.CREF, state.BREF, state.SREF, state.UNITL,
      state.RHO0, state.GEE0, state.RMASS0,
      state.IPTOT, state.ICMAX, state.IVTOT,
      parvalPtr, convalPtr, iconPtr, itrimPtr,
      state.IPPHI, state.IPTHE, state.IPCL, state.IPVEE, state.IPRAD,
      state.IPRHO, state.IPGEE, state.IPFAC, state.IPMASS,
      state.IVALFA, state.IVROTX, state.IVROTY, state.IVROTZ,
      state.ICCL, state.ICROTX, state.ICROTY, state.ICROTZ,
    );

    state.PARVAL = readF32(f32, parvalPtr, state.PARVAL.length);
    state.CONVAL = readF32(f32, convalPtr, state.CONVAL.length);
    state.ICON = readI32(i32, iconPtr, state.ICON.length);
    state.ITRIM = readI32(i32, itrimPtr, state.ITRIM.length);
    return state;
  }

  return { TRMSET_CORE_wasm };
}

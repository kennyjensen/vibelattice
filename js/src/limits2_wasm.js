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

function viewToArray(view) {
  return Float32Array.from([
    view.RINV,
    view.XIHAT, view.YIHAT, view.ZIHAT,
    view.XJHAT, view.YJHAT, view.ZJHAT,
    view.XKHAT, view.YKHAT, view.ZKHAT,
  ]);
}

export async function loadLimits2Wasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'limits2.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      sin_f32: (x) => Math.fround(Math.sin(Math.fround(x))),
      cos_f32: (x) => Math.fround(Math.cos(Math.fround(x))),
      log_f32: (x) => Math.fround(Math.log(Math.fround(x))),
      pow_f32: (x, y) => Math.fround(Math.pow(Math.fround(x), Math.fround(y))),
    },
  });

  const { memory, GLIMS, GRLIMS, AXLIMS, HIDINIT, HIDINITE } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const i32 = new Int32Array(memory.buffer);
  const allocator = makeAllocator(0);

  function allocF32(len) {
    return allocator.alloc(len * 4);
  }

  function allocI32(len) {
    return allocator.alloc(len * 4);
  }

  function GLIMS_wasm(state, LPROJ = false) {
    allocator.reset();
    const lpltsurfPtr = allocI32(state.LPLTSURF.length);
    const jfrstPtr = allocI32(state.JFRST.length);
    const njPtr = allocI32(state.NJ.length);
    const rle1Ptr = allocF32(state.RLE1.length);
    const chord1Ptr = allocF32(state.CHORD1.length);
    const lpltbodyPtr = allocI32(state.LPLTBODY.length);
    const lfrstPtr = allocI32(state.LFRST.length);
    const nlPtr = allocI32(state.NL.length);
    const rlPtr = allocF32(state.RL.length);
    const robPtr = allocF32(state.ROB.length);
    const viewPtr = allocF32(10);
    const ptsPtr = allocF32(12);
    const xyzminPtr = allocF32(3);
    const xyzmaxPtr = allocF32(3);

    writeI32(i32, lpltsurfPtr, Int32Array.from(state.LPLTSURF, (v) => (v ? 1 : 0)));
    writeI32(i32, jfrstPtr, Int32Array.from(state.JFRST));
    writeI32(i32, njPtr, Int32Array.from(state.NJ));
    writeF32(f32, rle1Ptr, state.RLE1);
    writeF32(f32, chord1Ptr, state.CHORD1);
    writeI32(i32, lpltbodyPtr, Int32Array.from(state.LPLTBODY, (v) => (v ? 1 : 0)));
    writeI32(i32, lfrstPtr, Int32Array.from(state.LFRST));
    writeI32(i32, nlPtr, Int32Array.from(state.NL));
    writeF32(f32, rlPtr, state.RL);
    writeF32(f32, robPtr, state.ROB);
    writeF32(f32, viewPtr, viewToArray(state.VIEW));

    GLIMS(LPROJ ? 1 : 0, state.NSURF, state.NBODY, state.NOB, state.LOBPLT ? 1 : 0,
      lpltsurfPtr, jfrstPtr, njPtr,
      rle1Ptr, chord1Ptr,
      lpltbodyPtr, lfrstPtr, nlPtr, rlPtr,
      robPtr, viewPtr, ptsPtr,
      xyzminPtr, xyzmaxPtr);

    return {
      XYZMIN: readF32(f32, xyzminPtr, 3),
      XYZMAX: readF32(f32, xyzmaxPtr, 3),
    };
  }

  function GRLIMS_wasm(state, LPROJ, TT, XYZR, DXYZ) {
    allocator.reset();
    const lpltsurfPtr = allocI32(state.LPLTSURF.length);
    const jfrstPtr = allocI32(state.JFRST.length);
    const njPtr = allocI32(state.NJ.length);
    const rle1Ptr = allocF32(state.RLE1.length);
    const chord1Ptr = allocF32(state.CHORD1.length);
    const rle2Ptr = allocF32(state.RLE2.length);
    const chord2Ptr = allocF32(state.CHORD2.length);
    const lpltbodyPtr = allocI32(state.LPLTBODY.length);
    const lfrstPtr = allocI32(state.LFRST.length);
    const nlPtr = allocI32(state.NL.length);
    const rlPtr = allocF32(state.RL.length);
    const viewPtr = allocF32(10);
    const ptsPtr = allocF32(12);
    const ttPtr = allocF32(9);
    const xyzrPtr = allocF32(3);
    const dxyzPtr = allocF32(3);
    const xyzminPtr = allocF32(3);
    const xyzmaxPtr = allocF32(3);

    writeI32(i32, lpltsurfPtr, Int32Array.from(state.LPLTSURF, (v) => (v ? 1 : 0)));
    writeI32(i32, jfrstPtr, Int32Array.from(state.JFRST));
    writeI32(i32, njPtr, Int32Array.from(state.NJ));
    writeF32(f32, rle1Ptr, state.RLE1);
    writeF32(f32, chord1Ptr, state.CHORD1);
    writeF32(f32, rle2Ptr, state.RLE2);
    writeF32(f32, chord2Ptr, state.CHORD2);
    writeI32(i32, lpltbodyPtr, Int32Array.from(state.LPLTBODY, (v) => (v ? 1 : 0)));
    writeI32(i32, lfrstPtr, Int32Array.from(state.LFRST));
    writeI32(i32, nlPtr, Int32Array.from(state.NL));
    writeF32(f32, rlPtr, state.RL);
    writeF32(f32, viewPtr, viewToArray(state.VIEW));
    writeF32(f32, ttPtr, TT);
    writeF32(f32, xyzrPtr, XYZR);
    writeF32(f32, dxyzPtr, DXYZ);

    GRLIMS(LPROJ ? 1 : 0, state.NSURF, state.NBODY,
      lpltsurfPtr, jfrstPtr, njPtr,
      rle1Ptr, chord1Ptr,
      rle2Ptr, chord2Ptr,
      lpltbodyPtr, lfrstPtr, nlPtr, rlPtr,
      viewPtr, ptsPtr,
      ttPtr, xyzrPtr, dxyzPtr,
      xyzminPtr, xyzmaxPtr);

    return {
      XYZMIN: readF32(f32, xyzminPtr, 3),
      XYZMAX: readF32(f32, xyzmaxPtr, 3),
    };
  }

  function AXLIMS_wasm(state) {
    allocator.reset();
    const gminPtr = allocF32(3);
    const gmaxPtr = allocF32(3);
    const axminPtr = allocF32(3);
    const axmaxPtr = allocF32(3);
    const axspanPtr = allocF32(3);
    const axdelPtr = allocF32(3);
    const naxannPtr = allocI32(3);

    writeF32(f32, gminPtr, state.GMIN);
    writeF32(f32, gmaxPtr, state.GMAX);

    AXLIMS(gminPtr, gmaxPtr, axminPtr, axmaxPtr, axspanPtr, axdelPtr, naxannPtr);

    state.AXMIN = readF32(f32, axminPtr, 3);
    state.AXMAX = readF32(f32, axmaxPtr, 3);
    state.AXSPAN = readF32(f32, axspanPtr, 3);
    state.AXDEL = readF32(f32, axdelPtr, 3);
    state.NAXANN = readI32(i32, naxannPtr, 3);
    return state;
  }

  function HIDINIT_wasm(state, LRESET) {
    allocator.reset();
    const lpltsurfPtr = allocI32(state.LPLTSURF.length);
    const jfrstPtr = allocI32(state.JFRST.length);
    const njPtr = allocI32(state.NJ.length);
    const rle1Ptr = allocF32(state.RLE1.length);
    const chord1Ptr = allocF32(state.CHORD1.length);
    const rle2Ptr = allocF32(state.RLE2.length);
    const chord2Ptr = allocF32(state.CHORD2.length);
    const viewPtr = allocF32(10);
    const ptsPtr = allocF32(12);
    const ntriPtr = allocI32(1);
    const triPtr = allocF32(state.TRI.length);

    writeI32(i32, lpltsurfPtr, Int32Array.from(state.LPLTSURF, (v) => (v ? 1 : 0)));
    writeI32(i32, jfrstPtr, Int32Array.from(state.JFRST));
    writeI32(i32, njPtr, Int32Array.from(state.NJ));
    writeF32(f32, rle1Ptr, state.RLE1);
    writeF32(f32, chord1Ptr, state.CHORD1);
    writeF32(f32, rle2Ptr, state.RLE2);
    writeF32(f32, chord2Ptr, state.CHORD2);
    writeF32(f32, viewPtr, viewToArray(state.VIEW));
    writeI32(i32, ntriPtr, Int32Array.from([state.NTRI]));
    writeF32(f32, triPtr, state.TRI);

    HIDINIT(LRESET ? 1 : 0, state.NSURF,
      lpltsurfPtr, jfrstPtr, njPtr,
      rle1Ptr, chord1Ptr, rle2Ptr, chord2Ptr,
      viewPtr, ptsPtr,
      ntriPtr, triPtr);

    state.NTRI = readI32(i32, ntriPtr, 1)[0];
    state.TRI = readF32(f32, triPtr, state.TRI.length);
    return state;
  }

  function HIDINITE_wasm(state, LRESET, ANG, POS, XYZR) {
    allocator.reset();
    const lpltsurfPtr = allocI32(state.LPLTSURF.length);
    const jfrstPtr = allocI32(state.JFRST.length);
    const njPtr = allocI32(state.NJ.length);
    const rle1Ptr = allocF32(state.RLE1.length);
    const chord1Ptr = allocF32(state.CHORD1.length);
    const rle2Ptr = allocF32(state.RLE2.length);
    const chord2Ptr = allocF32(state.CHORD2.length);
    const viewPtr = allocF32(10);
    const ptsPtr = allocF32(12);
    const ntriPtr = allocI32(1);
    const triPtr = allocF32(state.TRI.length);
    const angPtr = allocF32(3);
    const posPtr = allocF32(3);
    const xyzrPtr = allocF32(3);
    const ttPtr = allocF32(9);
    const ttangPtr = allocF32(27);

    writeI32(i32, lpltsurfPtr, Int32Array.from(state.LPLTSURF, (v) => (v ? 1 : 0)));
    writeI32(i32, jfrstPtr, Int32Array.from(state.JFRST));
    writeI32(i32, njPtr, Int32Array.from(state.NJ));
    writeF32(f32, rle1Ptr, state.RLE1);
    writeF32(f32, chord1Ptr, state.CHORD1);
    writeF32(f32, rle2Ptr, state.RLE2);
    writeF32(f32, chord2Ptr, state.CHORD2);
    writeF32(f32, viewPtr, viewToArray(state.VIEW));
    writeI32(i32, ntriPtr, Int32Array.from([state.NTRI]));
    writeF32(f32, triPtr, state.TRI);
    writeF32(f32, angPtr, ANG);
    writeF32(f32, posPtr, POS);
    writeF32(f32, xyzrPtr, XYZR);

    HIDINITE(LRESET ? 1 : 0, state.NSURF,
      lpltsurfPtr, jfrstPtr, njPtr,
      rle1Ptr, chord1Ptr, rle2Ptr, chord2Ptr,
      viewPtr, ptsPtr,
      ntriPtr, triPtr,
      angPtr, posPtr, xyzrPtr,
      ttPtr, ttangPtr);

    state.NTRI = readI32(i32, ntriPtr, 1)[0];
    state.TRI = readF32(f32, triPtr, state.TRI.length);
    return state;
  }

  return {
    GLIMS: GLIMS_wasm,
    GRLIMS: GRLIMS_wasm,
    AXLIMS: AXLIMS_wasm,
    HIDINIT: HIDINIT_wasm,
    HIDINITE: HIDINITE_wasm,
    memory,
  };
}

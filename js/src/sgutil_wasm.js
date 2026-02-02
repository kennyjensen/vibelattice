/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';

const OFFSETS = {
  X: 16384,
  Y: 20480,
  OUT: 24576,
  XPT: 28672,
  XVR: 32768,
  XSR: 36864,
  XCP: 40960,
};

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadSgutilWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'sgutil.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      cos: (x) => Math.fround(Math.cos(Math.fround(x))),
      sin: (x) => Math.fround(Math.sin(Math.fround(x))),
      atan: (x) => Math.fround(Math.atan(Math.fround(x))),
    },
  });
  const { memory, AKIMA, TRP1, NRMLIZ, SPACER, CSPACER } = instance.exports;
  const f32 = new Float32Array(memory.buffer);

  function AKIMA_wasm(X, Y, N, XX) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.Y, Y);
    AKIMA(OFFSETS.X, OFFSETS.Y, N, Math.fround(XX), OFFSETS.OUT, OFFSETS.OUT + 4);
    return {
      YY: f32[OFFSETS.OUT / 4],
      SLP: f32[OFFSETS.OUT / 4 + 1],
    };
  }

  function TRP1_wasm(N, X, Y, XTRP) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.Y, Y);
    return TRP1(N, OFFSETS.X, OFFSETS.Y, Math.fround(XTRP));
  }

  function NRMLIZ_wasm(N, X) {
    writeF32(f32, OFFSETS.X, X);
    NRMLIZ(N, OFFSETS.X);
    return readF32(f32, OFFSETS.X, N);
  }

  function SPACER_wasm(N, PSPACE) {
    SPACER(N, Math.fround(PSPACE), OFFSETS.X);
    return readF32(f32, OFFSETS.X, N);
  }

  function CSPACER_wasm(NVC, CSPACE, CLAF) {
    CSPACER(NVC, Math.fround(CSPACE), Math.fround(CLAF),
      OFFSETS.XPT, OFFSETS.XVR, OFFSETS.XSR, OFFSETS.XCP);
    return {
      XPT: readF32(f32, OFFSETS.XPT, NVC + 1),
      XVR: readF32(f32, OFFSETS.XVR, NVC),
      XSR: readF32(f32, OFFSETS.XSR, NVC),
      XCP: readF32(f32, OFFSETS.XCP, NVC),
    };
  }

  return {
    AKIMA: AKIMA_wasm,
    TRP1: TRP1_wasm,
    NRMLIZ: NRMLIZ_wasm,
    SPACER: SPACER_wasm,
    CSPACER: CSPACER_wasm,
    memory,
  };
}

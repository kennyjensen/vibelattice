import fs from 'node:fs/promises';
import path from 'node:path';

const OFFSETS = {
  X: 16384,
  XS: 20480,
  S: 24576,
  Y: 28672,
  YS: 32768,
  OUT: 36864,
};

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadSplineWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'spline.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, SPLINE, SPLIND, SPLINA, TRISOL, SEVAL, DEVAL, D2VAL, SEVALL, CURV } = instance.exports;
  const f32 = new Float32Array(memory.buffer);

  function SPLINE_wasm(X, S, N) {
    const XS = new Float32Array(N);
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.S, S);
    SPLINE(OFFSETS.X, OFFSETS.XS, OFFSETS.S, N);
    return readF32(f32, OFFSETS.XS, N);
  }

  function SPLIND_wasm(X, S, N, XS1, XS2) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.S, S);
    SPLIND(OFFSETS.X, OFFSETS.XS, OFFSETS.S, N, Math.fround(XS1), Math.fround(XS2));
    return readF32(f32, OFFSETS.XS, N);
  }

  function SPLINA_wasm(X, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.S, S);
    SPLINA(OFFSETS.X, OFFSETS.XS, OFFSETS.S, N);
    return readF32(f32, OFFSETS.XS, N);
  }

  function TRISOL_wasm(A, B, C, D, N) {
    writeF32(f32, OFFSETS.X, A);
    writeF32(f32, OFFSETS.S, B);
    writeF32(f32, OFFSETS.Y, C);
    writeF32(f32, OFFSETS.YS, D);
    TRISOL(OFFSETS.X, OFFSETS.S, OFFSETS.Y, OFFSETS.YS, N);
    return readF32(f32, OFFSETS.YS, N);
  }

  function SEVAL_wasm(SS, X, XS, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.XS, XS);
    writeF32(f32, OFFSETS.S, S);
    return SEVAL(Math.fround(SS), OFFSETS.X, OFFSETS.XS, OFFSETS.S, N);
  }

  function DEVAL_wasm(SS, X, XS, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.XS, XS);
    writeF32(f32, OFFSETS.S, S);
    return DEVAL(Math.fround(SS), OFFSETS.X, OFFSETS.XS, OFFSETS.S, N);
  }

  function D2VAL_wasm(SS, X, XS, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.XS, XS);
    writeF32(f32, OFFSETS.S, S);
    return D2VAL(Math.fround(SS), OFFSETS.X, OFFSETS.XS, OFFSETS.S, N);
  }

  function SEVALL_wasm(SS, X, XS, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.XS, XS);
    writeF32(f32, OFFSETS.S, S);
    SEVALL(Math.fround(SS), OFFSETS.X, OFFSETS.XS, OFFSETS.S, N, OFFSETS.OUT);
    return readF32(f32, OFFSETS.OUT, 3);
  }

  function CURV_wasm(SS, X, XS, Y, YS, S, N) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.XS, XS);
    writeF32(f32, OFFSETS.Y, Y);
    writeF32(f32, OFFSETS.YS, YS);
    writeF32(f32, OFFSETS.S, S);
    return CURV(Math.fround(SS), OFFSETS.X, OFFSETS.XS, OFFSETS.Y, OFFSETS.YS, OFFSETS.S, N);
  }

  return {
    SPLINE: SPLINE_wasm,
    SPLIND: SPLIND_wasm,
    SPLINA: SPLINA_wasm,
    TRISOL: TRISOL_wasm,
    SEVAL: SEVAL_wasm,
    DEVAL: DEVAL_wasm,
    D2VAL: D2VAL_wasm,
    SEVALL: SEVALL_wasm,
    CURV: CURV_wasm,
    memory,
  };
}

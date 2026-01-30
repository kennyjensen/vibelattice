import fs from 'node:fs/promises';
import path from 'node:path';

const OFFSETS = {
  X: 0,
  Y: 1024,
  XC: 2048,
  YC: 3072,
  TC: 4096,
};

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadAirutilWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'airutil.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      cos: (x) => Math.fround(Math.cos(Math.fround(x))),
      atan: (x) => Math.fround(Math.atan(Math.fround(x))),
    },
  });
  const { memory, GETCAM } = instance.exports;
  const f32 = new Float32Array(memory.buffer);

  function GETCAM_wasm(X, Y, N, NC, LNORM) {
    writeF32(f32, OFFSETS.X, X);
    writeF32(f32, OFFSETS.Y, Y);
    GETCAM(OFFSETS.X, OFFSETS.Y, N, OFFSETS.XC, OFFSETS.YC, OFFSETS.TC, NC, LNORM ? 1 : 0);
    return {
      XC: readF32(f32, OFFSETS.XC, NC),
      YC: readF32(f32, OFFSETS.YC, NC),
      TC: readF32(f32, OFFSETS.TC, NC),
    };
  }

  return { GETCAM: GETCAM_wasm, memory };
}

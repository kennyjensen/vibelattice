import fs from 'node:fs/promises';
import path from 'node:path';

const POL_OFFSET = 0;
const CL_OFFSET = 64;
const OUT_OFFSET = 128;

export async function loadCdclWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'cdcl.wasm');

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, CDCL } = instance.exports;
  const view = new Float32Array(memory.buffer);

  function CDCL_wasm(cdclpol, cl) {
    view.set(cdclpol, POL_OFFSET / 4);
    view[CL_OFFSET / 4] = Math.fround(cl);
    CDCL(POL_OFFSET, CL_OFFSET, OUT_OFFSET);
    return {
      cd: view[OUT_OFFSET / 4],
      cd_cl: view[OUT_OFFSET / 4 + 1],
    };
  }

  return { CDCL: CDCL_wasm, memory };
}

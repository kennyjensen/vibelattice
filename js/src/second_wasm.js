import fs from 'node:fs/promises';
import path from 'node:path';

function defaultNowSeconds() {
  if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
    return performance.now() / 1000;
  }
  return Date.now() / 1000;
}

export async function loadSecondWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'second.wasm');
  const nowFn = options.now ?? defaultNowSeconds;

  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    env: {
      now: () => nowFn(),
    },
  });

  const { SECONDS } = instance.exports;

  return { SECONDS: () => SECONDS() };
}

import path from 'node:path';
import { loadSecondWasm } from './second_wasm.js';

export async function loadSecondG77Wasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'second_g77.wasm');
  return loadSecondWasm({ ...options, wasmPath });
}

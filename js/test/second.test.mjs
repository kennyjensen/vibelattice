import test from 'node:test';
import assert from 'node:assert/strict';
import path from 'node:path';
import fs from 'node:fs';
import { performance } from 'node:perf_hooks';
import { SECONDS as SECONDS_JS } from '../src/second.js';
import { SECONDS as SECONDS_G77 } from '../src/second_g77.js';
import { SECONDS as SECONDS_IFC } from '../src/second_ifc.js';
import { loadSecondWasm } from '../src/second_wasm.js';
import { loadSecondG77Wasm } from '../src/second_g77_wasm.js';
import { loadSecondIfcWasm } from '../src/second_ifc_wasm.js';

if (typeof globalThis.performance === 'undefined') {
  globalThis.performance = performance;
}

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');

function assertMonotonic(fn) {
  const t1 = fn();
  const t2 = fn();
  assert.ok(Number.isFinite(t1) && Number.isFinite(t2), 'non-finite time');
  assert.ok(t2 >= t1 - 1e-6, `time went backwards: ${t1} -> ${t2}`);
}

function nowSeconds() {
  return performance.now() / 1000;
}

test('SECONDS JS variants return finite, monotonic values', () => {
  assertMonotonic(SECONDS_JS);
  assertMonotonic(SECONDS_G77);
  assertMonotonic(SECONDS_IFC);
});

test('SECONDS WASM variants return finite, monotonic values', async (t) => {
  const wasmPaths = {
    second: path.join(repoRoot, 'js', 'dist', 'second.wasm'),
    second_g77: path.join(repoRoot, 'js', 'dist', 'second_g77.wasm'),
    second_ifc: path.join(repoRoot, 'js', 'dist', 'second_ifc.wasm'),
  };

  if (!fs.existsSync(wasmPaths.second) || !fs.existsSync(wasmPaths.second_g77) || !fs.existsSync(wasmPaths.second_ifc)) {
    t.skip('WASM builds not found for second*');
    return;
  }

  const { SECONDS } = await loadSecondWasm({ wasmPath: wasmPaths.second, now: nowSeconds });
  const { SECONDS: SECONDS_G77_WASM } = await loadSecondG77Wasm({ wasmPath: wasmPaths.second_g77, now: nowSeconds });
  const { SECONDS: SECONDS_IFC_WASM } = await loadSecondIfcWasm({ wasmPath: wasmPaths.second_ifc, now: nowSeconds });

  assertMonotonic(SECONDS);
  assertMonotonic(SECONDS_G77_WASM);
  assertMonotonic(SECONDS_IFC_WASM);
});

test('SECONDS JS vs WASM are close in time', async (t) => {
  const wasmPath = path.join(repoRoot, 'js', 'dist', 'second.wasm');
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const { SECONDS } = await loadSecondWasm({ wasmPath, now: nowSeconds });
  const tJs = SECONDS_JS();
  const tWasm = SECONDS();
  assert.ok(Math.abs(tJs - tWasm) < 0.05, `JS/WASM time mismatch: ${tJs} vs ${tWasm}`);
});

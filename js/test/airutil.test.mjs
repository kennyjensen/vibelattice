import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { GETCAM as GETCAM_JS } from '../src/airutil.js';
import { loadAirutilWasm } from '../src/airutil_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'airutil.wasm');

function runFortranRef() {
  const bin = path.join(refDir, 'airutil_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  return proc.stdout.trim().split(/\s+/).map(Number);
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

const X = Float32Array.from([1.0, 0.8, 0.4, 0.0, 0.4, 0.8, 1.0]);
const Y = Float32Array.from([0.0, 0.05, 0.08, 0.0, -0.08, -0.05, 0.0]);
const N = 7;
const NC = 10;

test('airutil.f JS port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'airutil_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const XC = new Float32Array(NC);
  const YC = new Float32Array(NC);
  const TC = new Float32Array(NC);
  const out = GETCAM_JS(Float32Array.from(X), Float32Array.from(Y), N, XC, YC, TC, NC, true);
  const got = [...out.XC, ...out.YC, ...out.TC];
  assertCloseArray(got, ref);
});

test('airutil.f WASM port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'airutil_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const wasm = await loadAirutilWasm({ wasmPath });
  const out = wasm.GETCAM(Float32Array.from(X), Float32Array.from(Y), N, NC, true);
  const got = [...out.XC, ...out.YC, ...out.TC];
  assertCloseArray(got, ref);
});

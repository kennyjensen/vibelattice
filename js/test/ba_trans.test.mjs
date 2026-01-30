import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { BA2WA_MAT as BA2WA_MAT_JS, BA2SA_MAT as BA2SA_MAT_JS } from '../src/ba_trans.js';
import { loadBaTransWasm } from '../src/ba_trans_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'ba_trans.wasm');

function runFortranRef(binName, cases, valuesPerCase) {
  const bin = path.join(refDir, binName);
  const inputLines = [String(cases.length)];
  for (const vals of cases) {
    inputLines.push(Array.from(vals).join(' '));
  }

  const proc = spawnSync(bin, { input: inputLines.join('\n'), encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }

  const values = proc.stdout.trim().split(/\s+/).map(Number);
  const out = [];
  for (let i = 0; i < cases.length; i += 1) {
    out.push(Float32Array.from(values.slice(i * valuesPerCase, i * valuesPerCase + valuesPerCase)));
  }
  return out;
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

const cases = [
  Float32Array.from([0.1, 0.2, 1.0]),
  Float32Array.from([0.0, -0.5, 2.0]),
  Float32Array.from([1.2, 0.1, 0.8]),
  Float32Array.from([-0.7, 0.3, 1.5]),
];

const VALUES_PER_CASE = 45;

function compareCase(refVals, jsOut, jsSaOut) {
  assertCloseArray(jsOut.P, refVals.slice(0, 9));
  assertCloseArray(jsOut.P_A, refVals.slice(9, 18));
  assertCloseArray(jsOut.P_B, refVals.slice(18, 27));
  assertCloseArray(jsSaOut.P, refVals.slice(27, 36));
  assertCloseArray(jsSaOut.P_A, refVals.slice(36, 45));
}

test('BA2WA_MAT/BA2SA_MAT JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'ba_trans_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('ba_trans_ref', cases, VALUES_PER_CASE);
  cases.forEach((vals, i) => {
    const [alfa, beta, binv] = vals;
    const jsOut = BA2WA_MAT_JS(alfa, beta, binv);
    const jsSa = BA2SA_MAT_JS(alfa);
    compareCase(ref[i], jsOut, jsSa);
  });
});

test('BA2WA_MAT/BA2SA_MAT WASM matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'ba_trans_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const ref = runFortranRef('ba_trans_ref', cases, VALUES_PER_CASE);
  const { BA2WA_MAT, BA2SA_MAT } = await loadBaTransWasm({ wasmPath });
  cases.forEach((vals, i) => {
    const [alfa, beta, binv] = vals;
    const wasmOut = BA2WA_MAT(alfa, beta, binv);
    const wasmSa = BA2SA_MAT(alfa);
    compareCase(ref[i], wasmOut, wasmSa);
  });
});

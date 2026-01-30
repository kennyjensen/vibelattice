import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { M3INV as M3INV_JS, RATEKI3 as RATEKI3_JS, ROTENS3 as ROTENS3_JS } from '../src/autil.js';
import { loadAutilWasm } from '../src/autil_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'autil.wasm');

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

const m3invMatrices = [
  Float32Array.from([1, 0, 0, 0, 1, 0, 0, 0, 1]),
  Float32Array.from([2, 0, 0, 0, 3, 0, 0, 0, 4]),
  Float32Array.from([1, 2, 3, 0, 1, 4, 5, 6, 0]),
  Float32Array.from([0, 1, 0, 0, 0, 1, 1, 0, 0]),
];

const angleCases = [
  Float32Array.from([0.1, 0.2, -0.3]),
  Float32Array.from([0.5, -0.4, 0.25]),
  Float32Array.from([-1.2, 0.9, 0.0]),
  Float32Array.from([0.0, 0.0, 0.0]),
];

test('M3INV JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'm3inv_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('m3inv_ref', m3invMatrices, 9);
  m3invMatrices.forEach((mat, i) => {
    const jsOut = M3INV_JS(mat);
    assertCloseArray(jsOut, ref[i]);
  });
});

test('RATEKI3 JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'rateki3_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('rateki3_ref', angleCases, 36);
  angleCases.forEach((angles, i) => {
    const { R, R_A } = RATEKI3_JS(angles);
    const expected = ref[i];
    assertCloseArray(R, expected.slice(0, 9));
    assertCloseArray(R_A, expected.slice(9));
  });
});

test('ROTENS3 JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'rotens3_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('rotens3_ref', angleCases, 36);
  angleCases.forEach((angles, i) => {
    const { T, T_A } = ROTENS3_JS(angles);
    const expected = ref[i];
    assertCloseArray(T, expected.slice(0, 9));
    assertCloseArray(T_A, expected.slice(9));
  });
});

test('WASM matches Fortran reference for all autil routines', async (t) => {
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const m3invBin = path.join(refDir, 'm3inv_ref');
  const rateki3Bin = path.join(refDir, 'rateki3_ref');
  const rotens3Bin = path.join(refDir, 'rotens3_ref');
  if (!fs.existsSync(m3invBin) || !fs.existsSync(rateki3Bin) || !fs.existsSync(rotens3Bin)) {
    t.skip('Fortran reference binaries not found');
    return;
  }

  const { M3INV, RATEKI3, ROTENS3 } = await loadAutilWasm({ wasmPath });

  const refM3 = runFortranRef('m3inv_ref', m3invMatrices, 9);
  m3invMatrices.forEach((mat, i) => {
    const wasmOut = M3INV(mat);
    assertCloseArray(wasmOut, refM3[i]);
  });

  const refRate = runFortranRef('rateki3_ref', angleCases, 36);
  angleCases.forEach((angles, i) => {
    const { R, R_A } = RATEKI3(angles);
    const expected = refRate[i];
    assertCloseArray(R, expected.slice(0, 9));
    assertCloseArray(R_A, expected.slice(9));
  });

  const refRot = runFortranRef('rotens3_ref', angleCases, 36);
  angleCases.forEach((angles, i) => {
    const { T, T_A } = ROTENS3(angles);
    const expected = refRot[i];
    assertCloseArray(T, expected.slice(0, 9));
    assertCloseArray(T_A, expected.slice(9));
  });
});

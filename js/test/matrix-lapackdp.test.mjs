import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { LUSOLVE as LUSOLVE_DP } from '../src/matrix-lapackdp.js';
import { loadMatrixLapackDpWasm } from '../src/matrix-lapackdp_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'matrix-lapackdp.wasm');

function runFortranRef(cases) {
  const bin = path.join(refDir, 'matrix_ref_dp');
  const inputLines = [String(cases.length)];
  for (const c of cases) {
    inputLines.push(String(c.n));
    inputLines.push(Array.from(c.A).join(' '));
    inputLines.push(Array.from(c.B).join(' '));
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
  let offset = 0;
  for (const c of cases) {
    out.push(Float64Array.from(values.slice(offset, offset + c.n)));
    offset += c.n;
  }
  return out;
}

function assertCloseArray(actual, expected, tol = 1e-9) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

const cases = [
  {
    n: 3,
    A: Float64Array.from([
      3, 2, -1,
      2, -2, 4,
      -1, 0.5, -1,
    ]),
    B: Float64Array.from([1, -2, 0]),
  },
  {
    n: 4,
    A: Float64Array.from([
      4, -2, 1, 3,
      3, 6, -4, 2,
      2, 1, 8, -5,
      1, -3, 2, 7,
    ]),
    B: Float64Array.from([20, -7, 4, 15]),
  },
];

test('matrix-lapackdp JS matches Fortran DP reference', async (t) => {
  const refBin = path.join(refDir, 'matrix_ref_dp');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef(cases);
  cases.forEach((c, i) => {
    const sol = LUSOLVE_DP(c.A, c.B, c.n);
    assertCloseArray(sol, ref[i]);
  });
});

test('matrix-lapackdp WASM matches Fortran DP reference', async (t) => {
  const refBin = path.join(refDir, 'matrix_ref_dp');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const ref = runFortranRef(cases);
  const { solve } = await loadMatrixLapackDpWasm({ wasmPath });
  cases.forEach((c, i) => {
    const sol = solve(c.A, c.B, c.n);
    assertCloseArray(sol, ref[i]);
  });
});

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import {
  SGETRF,
  SGETRS,
  DGETRF,
  DGETRS,
} from '../src/matrix-lapacksubs.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef(binName, cases) {
  const bin = path.join(refDir, binName);
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
    out.push(values.slice(offset, offset + c.n));
    offset += c.n;
  }
  return out;
}

function assertCloseArray(actual, expected, tol) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

const casesSp = [
  {
    n: 3,
    A: Float32Array.from([
      3, 2, -1,
      2, -2, 4,
      -1, 0.5, -1,
    ]),
    B: Float32Array.from([1, -2, 0]),
  },
  {
    n: 4,
    A: Float32Array.from([
      4, -2, 1, 3,
      3, 6, -4, 2,
      2, 1, 8, -5,
      1, -3, 2, 7,
    ]),
    B: Float32Array.from([20, -7, 4, 15]),
  },
];

const casesDp = [
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

test('matrix-lapacksubs SGETRF/SGETRS JS matches Fortran', async (t) => {
  const refBin = path.join(refDir, 'lapacksubs_s_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('lapacksubs_s_ref', casesSp);
  casesSp.forEach((c, i) => {
    const a = toColMajor(c.A, c.n, Float32Array);
    const b = Float32Array.from(c.B);
    const ipiv = new Int32Array(c.n);
    const info = { value: 0 };
    SGETRF(c.n, c.n, a, c.n, ipiv, info);
    SGETRS('N', c.n, 1, a, c.n, ipiv, b, c.n, info);
    assertCloseArray(b, ref[i], 1e-4);
  });
});

test('matrix-lapacksubs DGETRF/DGETRS JS matches Fortran', async (t) => {
  const refBin = path.join(refDir, 'lapacksubs_d_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('lapacksubs_d_ref', casesDp);
  casesDp.forEach((c, i) => {
    const a = toColMajor(c.A, c.n, Float64Array);
    const b = Float64Array.from(c.B);
    const ipiv = new Int32Array(c.n);
    const info = { value: 0 };
    DGETRF(c.n, c.n, a, c.n, ipiv, info);
    DGETRS('N', c.n, 1, a, c.n, ipiv, b, c.n, info);
    assertCloseArray(b, ref[i], 1e-9);
  });
});

function toColMajor(rowMajor, n, ArrayType) {
  const out = new ArrayType(n * n);
  for (let r = 0; r < n; r += 1) {
    for (let c = 0; c < n; c += 1) {
      out[c * n + r] = rowMajor[r * n + c];
    }
  }
  return out;
}

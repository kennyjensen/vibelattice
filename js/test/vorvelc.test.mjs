import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { VORVELC as VORVELC_JS } from '../src/vorvelc.js';
import { loadVorvelcWasm } from '../src/vorvelc_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'vorvelc.wasm');

function runFortranRef(cases) {
  const bin = path.join(refDir, 'vorvelc_ref');
  const inputLines = [String(cases.length)];
  for (const c of cases) {
    inputLines.push([
      c.x, c.y, c.z, c.lbound ? 1 : 0,
      c.x1, c.y1, c.z1,
      c.x2, c.y2, c.z2,
      c.beta, c.rcore,
    ].join(' '));
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
    out.push({
      u: values[i * 3],
      v: values[i * 3 + 1],
      w: values[i * 3 + 2],
    });
  }
  return out;
}

function assertClose(a, b, tol = 1e-5) {
  const diff = Math.abs(a - b);
  assert.ok(diff <= tol, `diff ${diff} > ${tol}`);
}

const cases = [
  { x: 0, y: 0, z: 0, lbound: true, x1: 1, y1: 0.2, z1: -0.1, x2: 2, y2: 0.3, z2: 0.4, beta: 1.0, rcore: 0.05 },
  { x: 0.5, y: -0.2, z: 0.1, lbound: false, x1: 1.2, y1: 0.4, z1: -0.2, x2: 2.5, y2: -0.1, z2: 0.6, beta: 0.9, rcore: 0.02 },
  { x: -0.1, y: 0.3, z: -0.2, lbound: true, x1: -0.5, y1: 0.9, z1: 0.1, x2: 0.7, y2: -0.4, z2: -0.3, beta: 1.2, rcore: 0.1 },
];

test('VORVELC JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'vorvelc_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef(cases);
  cases.forEach((c, i) => {
    const { u, v, w } = VORVELC_JS(
      c.x, c.y, c.z, c.lbound,
      c.x1, c.y1, c.z1,
      c.x2, c.y2, c.z2,
      c.beta, c.rcore,
    );
    assertClose(u, ref[i].u);
    assertClose(v, ref[i].v);
    assertClose(w, ref[i].w);
  });
});

test('VORVELC WASM matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'vorvelc_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const ref = runFortranRef(cases);
  const { VORVELC } = await loadVorvelcWasm({ wasmPath });
  cases.forEach((c, i) => {
    const { u, v, w } = VORVELC(
      c.x, c.y, c.z, c.lbound,
      c.x1, c.y1, c.z1,
      c.x2, c.y2, c.z2,
      c.beta, c.rcore,
    );
    assertClose(u, ref[i].u);
    assertClose(v, ref[i].v);
    assertClose(w, ref[i].w);
  });
});

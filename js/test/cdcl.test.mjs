import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { CDCL as CDCL_JS } from '../src/cdcl.js';
import { loadCdclWasm } from '../src/cdcl_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'cdcl.wasm');

function runFortranRef(cases) {
  const bin = path.join(refDir, 'cdcl_ref');
  const inputLines = [String(cases.length)];
  for (const c of cases) {
    inputLines.push([...c.cdclpol, c.cl].join(' '));
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
    out.push({ cd: values[i * 2], cd_cl: values[i * 2 + 1] });
  }
  return out;
}

function assertClose(a, b, tol = 1e-5) {
  const diff = Math.abs(a - b);
  assert.ok(diff <= tol, `diff ${diff} > ${tol}`);
}

const cases = [
  {
    cdclpol: Float32Array.from([-1.0, 0.08, 0.0, 0.02, 1.0, 0.09]),
    cl: -1.5,
  },
  {
    cdclpol: Float32Array.from([-1.0, 0.08, 0.0, 0.02, 1.0, 0.09]),
    cl: -0.5,
  },
  {
    cdclpol: Float32Array.from([-1.0, 0.08, 0.0, 0.02, 1.0, 0.09]),
    cl: 0.2,
  },
  {
    cdclpol: Float32Array.from([-1.0, 0.08, 0.0, 0.02, 1.0, 0.09]),
    cl: 1.3,
  },
  {
    cdclpol: Float32Array.from([-0.5, 0.06, 0.2, 0.015, 1.4, 0.11]),
    cl: 0.7,
  },
];

test('CDCL JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'cdcl_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef(cases);
  cases.forEach((c, i) => {
    const { cd, cd_cl } = CDCL_JS(c.cdclpol, c.cl);
    assertClose(cd, ref[i].cd);
    assertClose(cd_cl, ref[i].cd_cl);
  });
});

test('CDCL WASM matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'cdcl_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const ref = runFortranRef(cases);
  const { CDCL } = await loadCdclWasm({ wasmPath });
  cases.forEach((c, i) => {
    const { cd, cd_cl } = CDCL(c.cdclpol, c.cl);
    assertClose(cd, ref[i].cd);
    assertClose(cd_cl, ref[i].cd_cl);
  });
});

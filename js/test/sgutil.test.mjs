import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { AKIMA, TRP1, NRMLIZ, SPACER, CSPACER } from '../src/sgutil.js';
import { loadSgutilWasm } from '../src/sgutil_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'sgutil.wasm');

function runFortranRef() {
  const bin = path.join(refDir, 'sgutil_ref');
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

const N = 6;
const X = Float32Array.from([0.0, 1.0, 2.5, 3.0, 4.5, 6.0]);
const Y = Float32Array.from([0.0, 1.2, 0.8, 1.6, 1.1, 2.0]);
const XX = 2.7;
const XTRP = 3.4;
const PSPACE = 1.5;
const NVC = 4;
const CSPACE = 1.7;
const CLAF = 0.25;

function buildOutputs() {
  const ak = AKIMA(X, Y, N, XX);
  const trp = TRP1(N, X, Y, XTRP);
  const xn = Float32Array.from(X);
  NRMLIZ(N, xn);
  const xsp = new Float32Array(N + 1);
  SPACER(N, PSPACE, xsp);
  const xpt = new Float32Array(NVC + 2);
  const xvr = new Float32Array(NVC + 1);
  const xsr = new Float32Array(NVC + 1);
  const xcp = new Float32Array(NVC + 1);
  CSPACER(NVC, CSPACE, CLAF, xpt, xvr, xsr, xcp);

  return [
    ak.YY,
    ak.SLP,
    trp,
    ...xn,
    ...xsp.slice(1),
    ...xpt.slice(1, NVC + 2),
    ...xvr.slice(1, NVC + 1),
    ...xsr.slice(1, NVC + 1),
    ...xcp.slice(1, NVC + 1),
  ];
}

async function buildOutputsWasm() {
  const wasm = await loadSgutilWasm({ wasmPath });
  const ak = wasm.AKIMA(X, Y, N, XX);
  const trp = wasm.TRP1(N, X, Y, XTRP);
  const xn = wasm.NRMLIZ(N, X);
  const xsp = wasm.SPACER(N, PSPACE);
  const csp = wasm.CSPACER(NVC, CSPACE, CLAF);

  return [
    ak.YY,
    ak.SLP,
    trp,
    ...xn,
    ...xsp,
    ...csp.XPT,
    ...csp.XVR,
    ...csp.XSR,
    ...csp.XCP,
  ];
}

test('sgutil.f JS port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'sgutil_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const out = buildOutputs();
  assertCloseArray(out, ref);
});

test('sgutil.f WASM port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'sgutil_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const out = await buildOutputsWasm();
  assertCloseArray(out, ref);
});

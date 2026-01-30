import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { SPLINE, SPLIND, SPLINA, SEVAL, DEVAL, D2VAL, SEVALL, CURV } from '../src/spline.js';
import { loadSplineWasm } from '../src/spline_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'spline.wasm');

function runFortranRef() {
  const bin = path.join(refDir, 'spline_ref');
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

const N = 5;
const S = Float32Array.from([0.0, 0.7, 1.4, 2.2, 3.0]);
const X = Float32Array.from([0.0, 1.0, 0.5, 1.5, 1.0]);
const Y = Float32Array.from([0.0, 0.2, 0.8, 0.6, 1.2]);
const SS = 1.05;

function buildOutputs() {
  const xsSpline = new Float32Array(N);
  const xsSplind = new Float32Array(N);
  const xsSplina = new Float32Array(N);
  const ysSpline = new Float32Array(N);

  SPLINE(X, xsSpline, S, N);
  SPLIND(X, xsSplind, S, N, 999.0, -999.0);
  SPLINA(X, xsSplina, S, N);

  const se = SEVAL(SS, X, xsSpline, S, N);
  const de = DEVAL(SS, X, xsSpline, S, N);
  const d2 = D2VAL(SS, X, xsSpline, S, N);
  const sevall = SEVALL(SS, X, xsSpline, S, N);

  SPLINE(Y, ysSpline, S, N);
  const cv = CURV(SS, X, xsSpline, Y, ysSpline, S, N);

  return [
    ...xsSpline,
    ...xsSplind,
    ...xsSplina,
    se,
    de,
    d2,
    sevall.XX,
    sevall.XXS,
    sevall.XXSS,
    cv,
  ];
}

async function buildOutputsWasm() {
  const wasm = await loadSplineWasm({ wasmPath });
  const xsSpline = wasm.SPLINE(X, S, N);
  const xsSplind = wasm.SPLIND(X, S, N, 999.0, -999.0);
  const xsSplina = wasm.SPLINA(X, S, N);
  const se = wasm.SEVAL(SS, X, xsSpline, S, N);
  const de = wasm.DEVAL(SS, X, xsSpline, S, N);
  const d2 = wasm.D2VAL(SS, X, xsSpline, S, N);
  const sevall = wasm.SEVALL(SS, X, xsSpline, S, N);
  const ysSpline = wasm.SPLINE(Y, S, N);
  const cv = wasm.CURV(SS, X, xsSpline, Y, ysSpline, S, N);

  return [
    ...xsSpline,
    ...xsSplind,
    ...xsSplina,
    se,
    de,
    d2,
    sevall[0],
    sevall[1],
    sevall[2],
    cv,
  ];
}

test('spline.f JS port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'spline_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const out = buildOutputs();
  assertCloseArray(out, ref);
});

test('spline.f WASM port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'spline_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef();
  const out = await buildOutputsWasm();
  assertCloseArray(out, ref);
});

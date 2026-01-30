import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { GLIMS, GRLIMS, AXLIMS, HIDINIT, HIDINITE } from '../src/limits.js';
import { loadLimitsWasm } from '../src/limits_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'limits.wasm');

function runFortranRef() {
  const bin = path.join(refDir, 'limits_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  return proc.stdout.trim().split(/\s+/).map(Number);
}

function parseRef(values) {
  let idx = 0;
  const glimsMin = values.slice(idx, idx + 3); idx += 3;
  const glimsMax = values.slice(idx, idx + 3); idx += 3;
  const grlimsMin = values.slice(idx, idx + 3); idx += 3;
  const grlimsMax = values.slice(idx, idx + 3); idx += 3;
  const axmin = values.slice(idx, idx + 3); idx += 3;
  const axmax = values.slice(idx, idx + 3); idx += 3;
  const axspan = values.slice(idx, idx + 3); idx += 3;
  const axdel = values.slice(idx, idx + 3); idx += 3;
  const naxann = values.slice(idx, idx + 3).map((v) => Math.trunc(v)); idx += 3;
  const ntri1 = Math.trunc(values[idx]); idx += 1;
  const tri1First = values.slice(idx, idx + 16); idx += 16;
  const tri1Last = values.slice(idx, idx + 16); idx += 16;
  const ntri2 = Math.trunc(values[idx]); idx += 1;
  const tri2First = values.slice(idx, idx + 16); idx += 16;
  const tri2Last = values.slice(idx, idx + 16); idx += 16;

  return {
    glimsMin,
    glimsMax,
    grlimsMin,
    grlimsMax,
    axmin,
    axmax,
    axspan,
    axdel,
    naxann,
    ntri1,
    tri1First,
    tri1Last,
    ntri2,
    tri2First,
    tri2Last,
  };
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function makeState() {
  const state = {
    NSURF: 1,
    NBODY: 1,
    NOB: 3,
    LOBPLT: true,
    LPLTSURF: [true],
    LPLTBODY: [true],
    JFRST: Int32Array.from([0]),
    NJ: Int32Array.from([3]),
    RLE1: Float32Array.from([
      0.0, 0.0, 0.0,
      1.0, 0.5, 0.2,
      2.0, 1.0, 0.4,
    ]),
    RLE2: Float32Array.from([
      0.0, 0.0, 1.0,
      1.0, 0.5, 1.2,
      2.0, 1.0, 1.4,
    ]),
    CHORD1: Float32Array.from([1.0, 0.8, 0.6]),
    CHORD2: Float32Array.from([1.1, 0.9, 0.7]),
    LFRST: Int32Array.from([1]),
    NL: Int32Array.from([2]),
    RL: Float32Array.from([
      0.0, 0.0, 0.0,
      -1.0, -0.5, 0.2,
      2.5, 0.4, -0.1,
    ]),
    ROB: Float32Array.from([
      0.2, 1.5, 0.0,
      1.1, -2.0, 0.3,
      -0.5, 0.7, -0.2,
    ]),
    GMIN: Float32Array.from([-1.0, -2.0, -0.2]),
    GMAX: Float32Array.from([2.6, 1.5, 1.4]),
    AXMIN: new Float32Array(3),
    AXMAX: new Float32Array(3),
    AXSPAN: new Float32Array(3),
    AXDEL: new Float32Array(3),
    NAXANN: new Int32Array(3),
    NTRI: 0,
    TRI: new Float32Array(16 * 12),
    VIEW: {
      RINV: 0.0,
      XIHAT: 1.0,
      YIHAT: 0.0,
      ZIHAT: 0.0,
      XJHAT: 0.0,
      YJHAT: 1.0,
      ZJHAT: 0.0,
      XKHAT: 0.0,
      YKHAT: 0.0,
      ZKHAT: 1.0,
    },
  };
  return state;
}

function runJs() {
  const state = makeState();
  const tt = Float32Array.from([
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0,
  ]);
  const xyzr = Float32Array.from([0.0, 0.0, 0.0]);
  const dxyz = Float32Array.from([0.5, -0.25, 1.0]);
  const ang = Float32Array.from([0.0, 0.0, 0.0]);
  const pos = Float32Array.from([1.0, 2.0, 3.0]);

  const glims = GLIMS(state, new Float32Array(3), new Float32Array(3), false);
  const grlims = GRLIMS(state, new Float32Array(3), new Float32Array(3), false, tt, xyzr, dxyz);
  AXLIMS(state);

  HIDINIT(state, true);
  const ntri1 = state.NTRI;
  const tri1First = Array.from(state.TRI.subarray(0, 16));
  const tri1Last = Array.from(state.TRI.subarray((ntri1 - 1) * 16, ntri1 * 16));

  state.NTRI = 0;
  HIDINITE(state, true, ang, pos, xyzr);
  const ntri2 = state.NTRI;
  const tri2First = Array.from(state.TRI.subarray(0, 16));
  const tri2Last = Array.from(state.TRI.subarray((ntri2 - 1) * 16, ntri2 * 16));

  return {
    glimsMin: Array.from(glims.XYZMIN),
    glimsMax: Array.from(glims.XYZMAX),
    grlimsMin: Array.from(grlims.XYZMIN),
    grlimsMax: Array.from(grlims.XYZMAX),
    axmin: Array.from(state.AXMIN),
    axmax: Array.from(state.AXMAX),
    axspan: Array.from(state.AXSPAN),
    axdel: Array.from(state.AXDEL),
    naxann: Array.from(state.NAXANN),
    ntri1,
    tri1First,
    tri1Last,
    ntri2,
    tri2First,
    tri2Last,
  };
}

async function runWasm() {
  const state = makeState();
  const tt = Float32Array.from([
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0,
  ]);
  const xyzr = Float32Array.from([0.0, 0.0, 0.0]);
  const dxyz = Float32Array.from([0.5, -0.25, 1.0]);
  const ang = Float32Array.from([0.0, 0.0, 0.0]);
  const pos = Float32Array.from([1.0, 2.0, 3.0]);

  const wasm = await loadLimitsWasm({ wasmPath });
  const glims = wasm.GLIMS(state, false);
  const grlims = wasm.GRLIMS(state, false, tt, xyzr, dxyz);
  wasm.AXLIMS(state);

  wasm.HIDINIT(state, true);
  const ntri1 = state.NTRI;
  const tri1First = Array.from(state.TRI.subarray(0, 16));
  const tri1Last = Array.from(state.TRI.subarray((ntri1 - 1) * 16, ntri1 * 16));

  state.NTRI = 0;
  wasm.HIDINITE(state, true, ang, pos, xyzr);
  const ntri2 = state.NTRI;
  const tri2First = Array.from(state.TRI.subarray(0, 16));
  const tri2Last = Array.from(state.TRI.subarray((ntri2 - 1) * 16, ntri2 * 16));

  return {
    glimsMin: Array.from(glims.XYZMIN),
    glimsMax: Array.from(glims.XYZMAX),
    grlimsMin: Array.from(grlims.XYZMIN),
    grlimsMax: Array.from(grlims.XYZMAX),
    axmin: Array.from(state.AXMIN),
    axmax: Array.from(state.AXMAX),
    axspan: Array.from(state.AXSPAN),
    axdel: Array.from(state.AXDEL),
    naxann: Array.from(state.NAXANN),
    ntri1,
    tri1First,
    tri1Last,
    ntri2,
    tri2First,
    tri2Last,
  };
}

test('limits.f JS port matches Fortran reference', () => {
  const refBin = path.join(refDir, 'limits_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }

  const ref = parseRef(runFortranRef());
  const got = runJs();

  assertCloseArray(got.glimsMin, ref.glimsMin);
  assertCloseArray(got.glimsMax, ref.glimsMax);
  assertCloseArray(got.grlimsMin, ref.grlimsMin);
  assertCloseArray(got.grlimsMax, ref.grlimsMax);
  assertCloseArray(got.axmin, ref.axmin);
  assertCloseArray(got.axmax, ref.axmax);
  assertCloseArray(got.axspan, ref.axspan);
  assertCloseArray(got.axdel, ref.axdel);
  assert.deepEqual(got.naxann, ref.naxann);
  assert.equal(got.ntri1, ref.ntri1);
  assertCloseArray(got.tri1First, ref.tri1First);
  assertCloseArray(got.tri1Last, ref.tri1Last);
  assert.equal(got.ntri2, ref.ntri2);
  assertCloseArray(got.tri2First, ref.tri2First);
  assertCloseArray(got.tri2Last, ref.tri2Last);
});

test('limits.f WASM port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'limits_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }

  const ref = parseRef(runFortranRef());
  const got = await runWasm();

  assertCloseArray(got.glimsMin, ref.glimsMin);
  assertCloseArray(got.glimsMax, ref.glimsMax);
  assertCloseArray(got.grlimsMin, ref.grlimsMin);
  assertCloseArray(got.grlimsMax, ref.grlimsMax);
  assertCloseArray(got.axmin, ref.axmin);
  assertCloseArray(got.axmax, ref.axmax);
  assertCloseArray(got.axspan, ref.axspan);
  assertCloseArray(got.axdel, ref.axdel);
  assert.deepEqual(got.naxann, ref.naxann);
  assert.equal(got.ntri1, ref.ntri1);
  assertCloseArray(got.tri1First, ref.tri1First);
  assertCloseArray(got.tri1Last, ref.tri1Last);
  assert.equal(got.ntri2, ref.ntri2);
  assertCloseArray(got.tri2First, ref.tri2First);
  assertCloseArray(got.tri2Last, ref.tri2Last);
});

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { GAMSUM, VELSUM } from '../src/asetup.js';
import { loadAsetupWasm } from '../src/asetup_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'asetup_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  const matches = proc.stdout.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g) || [];
  return matches.map((v) => Number(v.replace(/d/i, 'e')));
}

function assertCloseArray(actual, expected, tol = 1e-4) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx3(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

function makeState() {
  const NVOR = 2;
  const NLNODE = 2;
  const NCONTROL = 1;
  const NDESIGN = 1;
  const NUMAX = 6;
  const NDMAX = 30;
  const NGMAX = 20;
  const DIM_N = NVOR + 1;
  const DIM_U = NUMAX + 1;
  const DIM_C = NDMAX + 1;
  const DIM_G = NGMAX + 1;
  const DIM_L = NLNODE + 1;
  const DIM_K = 4;

  const state = {
    NVOR,
    NLNODE,
    NCONTROL,
    NDESIGN,
    NUMAX,
    NDMAX,
    NGMAX,
    DIM_N,
    DIM_U,
    DIM_C,
    DIM_G,
    DIM_L,
    DIM_K,
    VINF: new Float32Array(4),
    WROT: new Float32Array(4),
    DELCON: new Float32Array(DIM_C),
    DELDES: new Float32Array(DIM_G),
    GAM_U_0: new Float32Array(DIM_N * DIM_U),
    GAM_U_D: new Float32Array(DIM_N * DIM_U * DIM_C),
    GAM_U_G: new Float32Array(DIM_N * DIM_U * DIM_G),
    GAM_U: new Float32Array(DIM_N * DIM_U),
    GAM_D: new Float32Array(DIM_N * DIM_C),
    GAM_G: new Float32Array(DIM_N * DIM_G),
    GAM: new Float32Array(DIM_N),
    SRC_U: new Float32Array(DIM_L * DIM_U),
    DBL_U: new Float32Array(DIM_K * DIM_L * DIM_U),
    SRC: new Float32Array(DIM_L),
    DBL: new Float32Array(DIM_K * DIM_L),
    WC_GAM: new Float32Array(DIM_K * DIM_N * DIM_N),
    WV_GAM: new Float32Array(DIM_K * DIM_N * DIM_N),
    VC: new Float32Array(DIM_K * DIM_N),
    VV: new Float32Array(DIM_K * DIM_N),
    WC: new Float32Array(DIM_K * DIM_N),
    WV: new Float32Array(DIM_K * DIM_N),
    WCSRD: new Float32Array(DIM_K * DIM_N),
    WVSRD: new Float32Array(DIM_K * DIM_N),
    VC_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    VV_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    WC_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    WV_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    VC_D: new Float32Array(DIM_K * DIM_N * DIM_C),
    VV_D: new Float32Array(DIM_K * DIM_N * DIM_C),
    WC_D: new Float32Array(DIM_K * DIM_N * DIM_C),
    WV_D: new Float32Array(DIM_K * DIM_N * DIM_C),
    VC_G: new Float32Array(DIM_K * DIM_N * DIM_G),
    VV_G: new Float32Array(DIM_K * DIM_N * DIM_G),
    WC_G: new Float32Array(DIM_K * DIM_N * DIM_G),
    WV_G: new Float32Array(DIM_K * DIM_N * DIM_G),
    WCSRD_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    WVSRD_U: new Float32Array(DIM_K * DIM_N * DIM_U),
  };

  state.VINF[1] = Math.fround(10.0);
  state.VINF[2] = Math.fround(-2.0);
  state.VINF[3] = Math.fround(1.0);
  state.WROT[1] = Math.fround(0.1);
  state.WROT[2] = Math.fround(-0.2);
  state.WROT[3] = Math.fround(0.3);

  state.DELCON[1] = Math.fround(0.2);
  state.DELDES[1] = Math.fround(-0.1);

  for (let k = 1; k <= 3; k += 1) {
    for (let i = 1; i <= NVOR; i += 1) {
      for (let j = 1; j <= NVOR; j += 1) {
        state.WC_GAM[idx3(k, i, j, DIM_K, DIM_N)] = Math.fround(0.01 * k + 0.001 * i + 0.0001 * j);
        state.WV_GAM[idx3(k, i, j, DIM_K, DIM_N)] = Math.fround(0.02 * k + 0.002 * i + 0.0002 * j);
      }
    }
  }

  for (let i = 1; i <= NVOR; i += 1) {
    for (let iu = 1; iu <= NUMAX; iu += 1) {
      state.GAM_U_0[idx2(i, iu, DIM_N)] = Math.fround(0.1 * i + 0.01 * iu);
      state.GAM_U_D[idx3(i, iu, 1, DIM_N, DIM_U)] = Math.fround(0.02 * i + 0.001 * iu);
      state.GAM_U_G[idx3(i, iu, 1, DIM_N, DIM_U)] = Math.fround(-0.03 * i + 0.002 * iu);
    }
  }

  for (let l = 1; l <= NLNODE; l += 1) {
    for (let iu = 1; iu <= NUMAX; iu += 1) {
      state.SRC_U[idx2(l, iu, DIM_L)] = Math.fround(0.05 * l + 0.002 * iu);
      for (let k = 1; k <= 3; k += 1) {
        state.DBL_U[idx3(k, l, iu, DIM_K, DIM_L)] = Math.fround(0.01 * k + 0.001 * l + 0.0005 * iu);
      }
    }
  }

  for (let k = 1; k <= 3; k += 1) {
    for (let i = 1; i <= NVOR; i += 1) {
      for (let iu = 1; iu <= NUMAX; iu += 1) {
        state.WCSRD_U[idx3(k, i, iu, DIM_K, DIM_N)] = Math.fround(0.03 * k + 0.001 * i + 0.0001 * iu);
        state.WVSRD_U[idx3(k, i, iu, DIM_K, DIM_N)] = Math.fround(0.04 * k + 0.0015 * i + 0.0002 * iu);
      }
    }
  }

  return state;
}

function extractOutputs(state) {
  const { NVOR, NLNODE, DIM_K, DIM_N } = state;
  const out = [];
  for (let i = 1; i <= NVOR; i += 1) out.push(state.GAM[i]);
  for (let i = 1; i <= NVOR; i += 1) out.push(state.GAM_D[idx2(i, 1, state.DIM_N)]);
  for (let i = 1; i <= NVOR; i += 1) out.push(state.GAM_G[idx2(i, 1, state.DIM_N)]);
  for (let l = 1; l <= NLNODE; l += 1) out.push(state.SRC[l]);
  for (let l = 1; l <= NLNODE; l += 1) {
    for (let k = 1; k <= 3; k += 1) {
      out.push(state.DBL[idx2(k, l, DIM_K)]);
    }
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.WC[idx2(k, i, DIM_K)]);
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.WV[idx2(k, i, DIM_K)]);
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.WC_U[idx3(k, i, 1, DIM_K, DIM_N)]);
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.WV_U[idx3(k, i, 1, DIM_K, DIM_N)]);
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.VC_D[idx3(k, i, 1, DIM_K, DIM_N)]);
  }
  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) out.push(state.VC_G[idx3(k, i, 1, DIM_K, DIM_N)]);
  }
  return out;
}

test('GAMSUM/VELSUM JS matches Fortran reference', () => {
  const refBin = path.join(refDir, 'asetup_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();
  const state = makeState();
  GAMSUM(state);
  VELSUM(state);
  const actual = extractOutputs(state);
  const expected = ref.slice(ref.length - actual.length);
  assertCloseArray(actual, expected, 1e-4);
});

test('GAMSUM/VELSUM WASM matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'asetup_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();
  const { GAMSUM_wasm, VELSUM_wasm } = await loadAsetupWasm();
  let state = makeState();
  state = GAMSUM_wasm(state);
  state = VELSUM_wasm(state);
  const actual = extractOutputs(state);
  const expected = ref.slice(ref.length - actual.length);
  assertCloseArray(actual, expected, 1e-4);
});

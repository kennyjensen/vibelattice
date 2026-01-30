import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { VVOR, VSRD, SRDSET, VORVELC, SRDVELC } from '../src/aic.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef(binName) {
  const bin = path.join(refDir, binName);
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  const values = proc.stdout.trim().split(/\s+/).map(Number);
  return values;
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function idx3(r, c) {
  return r + 3 * c;
}

function idx3c(r, i, j, dim1) {
  return r + 3 * (i + dim1 * j);
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx4(r, c) {
  return (r + 1) + 4 * c;
}

function idx3f(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

test('aic.f JS port matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'aic_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('aic_ref');
  let offset = 0;

  const refVorvelc = ref.slice(offset, offset + 3); offset += 3;
  const refSrdvelcV = ref.slice(offset, offset + 3); offset += 3;
  const refSrdvelcM = ref.slice(offset, offset + 9); offset += 9;
  const refSrdsetSrc = ref.slice(offset, offset + 12); offset += 12;
  const refSrdsetDbl = ref.slice(offset, offset + 36); offset += 36;
  const refVsrd = ref.slice(offset, offset + 36); offset += 36;
  const refVvor = ref.slice(offset, offset + 12); offset += 12;

  const BETM = 0.9;
  const IYSYM = 0;
  const YSYM = 0.2;
  const IZSYM = 0;
  const ZSYM = -0.1;
  const VRCOREC = 0.01;
  const VRCOREW = 0.02;
  const SRCORE = 0.1;

  const NV = 2;
  const NC = 2;
  const NCDIM = 2;

  const RV1p = new Float32Array(3 * NV);
  const RV2p = new Float32Array(3 * NV);
  const RCp = new Float32Array(3 * NC);

  RV1p[idx3(0, 0)] = 0.0; RV1p[idx3(1, 0)] = 0.0; RV1p[idx3(2, 0)] = 0.0;
  RV2p[idx3(0, 0)] = 1.0; RV2p[idx3(1, 0)] = 0.5; RV2p[idx3(2, 0)] = 0.2;
  RV1p[idx3(0, 1)] = 0.2; RV1p[idx3(1, 1)] = -0.3; RV1p[idx3(2, 1)] = 0.1;
  RV2p[idx3(0, 1)] = 1.2; RV2p[idx3(1, 1)] = 0.2; RV2p[idx3(2, 1)] = -0.1;

  RCp[idx3(0, 0)] = 0.5; RCp[idx3(1, 0)] = 0.1; RCp[idx3(2, 0)] = 0.05;
  RCp[idx3(0, 1)] = 0.8; RCp[idx3(1, 1)] = -0.1; RCp[idx3(2, 1)] = 0.2;

  const RV1 = new Float32Array(4 * (NV + 1));
  const RV2 = new Float32Array(4 * (NV + 1));
  const RC = new Float32Array(4 * (NC + 1));
  const CHORDV = new Float32Array(NV + 1);
  const NCOMPV = new Int32Array(NV + 1);
  const NCOMPC = new Int32Array(NC + 1);

  RV1[idx4(0, 1)] = 0.0; RV1[idx4(1, 1)] = 0.0; RV1[idx4(2, 1)] = 0.0;
  RV2[idx4(0, 1)] = 1.0; RV2[idx4(1, 1)] = 0.5; RV2[idx4(2, 1)] = 0.2;
  RV1[idx4(0, 2)] = 0.2; RV1[idx4(1, 2)] = -0.3; RV1[idx4(2, 2)] = 0.1;
  RV2[idx4(0, 2)] = 1.2; RV2[idx4(1, 2)] = 0.2; RV2[idx4(2, 2)] = -0.1;

  RC[idx4(0, 1)] = 0.5; RC[idx4(1, 1)] = 0.1; RC[idx4(2, 1)] = 0.05;
  RC[idx4(0, 2)] = 0.8; RC[idx4(1, 2)] = -0.1; RC[idx4(2, 2)] = 0.2;

  CHORDV[1] = 1.0;
  CHORDV[2] = 0.8;
  NCOMPV[1] = 1;
  NCOMPV[2] = 2;
  NCOMPC[1] = 1;
  NCOMPC[2] = 2;

  const wcGam = VVOR(BETM, IYSYM, YSYM, IZSYM, ZSYM,
    VRCOREC, VRCOREW,
    NV, RV1, RV2, NCOMPV, CHORDV,
    NC, RC, NCOMPC, true,
    new Float32Array(4 * (NCDIM + 1) * (NV + 1)), NCDIM);

  const X = 0.55;
  const Y = 0.15;
  const Z = -0.02;
  const X1 = RV1p[idx3(0, 0)];
  const Y1 = RV1p[idx3(1, 0)];
  const Z1 = RV1p[idx3(2, 0)];
  const X2 = RV2p[idx3(0, 0)];
  const Y2 = RV2p[idx3(1, 0)];
  const Z2 = RV2p[idx3(2, 0)];

  const vor = VORVELC(X, Y, Z, true, X1, Y1, Z1, X2, Y2, Z2, BETM, 0.03);
  assertCloseArray([vor.U, vor.V, vor.W], refVorvelc);

  const srd = SRDVELC(X, Y, Z, X1, Y1, Z1, X2, Y2, Z2, BETM, 0.05);
  assertCloseArray(Array.from(srd.UVWS), refSrdvelcV);
  const srdMat = [];
  for (let k = 0; k < 3; k += 1) {
    for (let j = 0; j < 3; j += 1) {
      srdMat.push(srd.UVWD[idx3(k, j)]);
    }
  }
  assertCloseArray(srdMat, refSrdvelcM);

  const NBODY = 1;
  const NLDIM = 3;
  const NU = 6;
  const LFRST = Int32Array.from([1]);
  const NL = Int32Array.from([3]);
  const RL = new Float32Array(3 * NLDIM);
  const RADL = Float32Array.from([0.2, 0.25, 0.3]);
  const XYZREF = Float32Array.from([0.0, 0.0, 0.0]);

  RL[idx3(0, 0)] = 0.0; RL[idx3(1, 0)] = 0.0; RL[idx3(2, 0)] = 0.0;
  RL[idx3(0, 1)] = 1.0; RL[idx3(1, 1)] = 0.1; RL[idx3(2, 1)] = 0.0;
  RL[idx3(0, 2)] = 2.0; RL[idx3(1, 2)] = 0.1; RL[idx3(2, 2)] = 0.1;

  const srdset = SRDSET(BETM, XYZREF, IYSYM,
    NBODY, LFRST, NLDIM,
    NL, RL, RADL);

  const srcOut = [];
  for (let L = 0; L < 2; L += 1) {
    for (let iu = 0; iu < 6; iu += 1) {
      srcOut.push(srdset.SRC_U[idx2(L, iu, NLDIM)]);
    }
  }
  const dblOut = [];
  for (let k = 0; k < 3; k += 1) {
    for (let L = 0; L < 2; L += 1) {
      for (let iu = 0; iu < 6; iu += 1) {
        dblOut.push(srdset.DBL_U[idx3c(k, L, iu, NLDIM)]);
      }
    }
  }
  assertCloseArray(srcOut, refSrdsetSrc);
  assertCloseArray(dblOut, refSrdsetDbl);

  const wcU = VSRD(BETM, IYSYM, YSYM, IZSYM, ZSYM, SRCORE,
    NBODY, LFRST, NLDIM,
    NL, RL, RADL,
    NU, srdset.SRC_U, srdset.DBL_U,
    NC, RCp,
    undefined, NCDIM);

  const wcOut = [];
  for (let i = 0; i < NC; i += 1) {
    for (let iu = 0; iu < 6; iu += 1) {
      for (let k = 0; k < 3; k += 1) {
        wcOut.push(wcU[idx3c(k, i, iu, NCDIM)]);
      }
    }
  }
  assertCloseArray(wcOut, refVsrd);

  const vvorOut = [];
  for (let i = 1; i <= NC; i += 1) {
    for (let j = 1; j <= NV; j += 1) {
      for (let k = 1; k <= 3; k += 1) {
        vvorOut.push(wcGam[idx3f(k, i, j, 4, NCDIM + 1)]);
      }
    }
  }
  assertCloseArray(vvorOut, refVvor);
});

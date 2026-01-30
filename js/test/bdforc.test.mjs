import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { BDFORC } from '../src/aero.js';

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
  return proc.stdout.trim().split(/\s+/).map(Number);
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

function idx4(r, c) {
  return (r + 1) + 4 * c;
}

test('BDFORC JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'bdforc_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('bdforc_ref');
  let offset = 0;
  const refCDB = ref.slice(offset, offset + 3); offset += 3;
  const refCFB = ref.slice(offset, offset + 3); offset += 3;
  const refCMB = ref.slice(offset, offset + 3); offset += 3;
  const refDCPB = ref.slice(offset, offset + 3); offset += 3;
  const refTotals = ref.slice(offset, offset + 3); offset += 3;
  const refCFTOT = ref.slice(offset, offset + 3); offset += 3;
  const refCMTOT = ref.slice(offset, offset + 3); offset += 3;
  const refCDTOT_U = ref.slice(offset, offset + 6); offset += 6;
  const refCYTOT_U = ref.slice(offset, offset + 6); offset += 6;
  const refCLTOT_U = ref.slice(offset, offset + 6); offset += 6;
  const refCFTOT_U = ref.slice(offset, offset + 18); offset += 18;
  const refCMTOT_U = ref.slice(offset, offset + 18); offset += 18;

  const NUMAX = 6;
  const NBODY = 1;
  const NLNODE = 2;

  const srcU = new Float32Array(NLNODE * NUMAX + 1);
  for (let iu = 0; iu < NUMAX; iu += 1) {
    srcU[idx2(1, iu, NLNODE)] = Math.fround(0.01 * (iu + 1));
    srcU[idx2(2, iu, NLNODE)] = 0.0;
  }

  const state = {
    ALFA: Math.fround(0.1),
    MACH: Math.fround(0.3),
    SREF: Math.fround(1.5),
    CREF: Math.fround(1.0),
    BREF: Math.fround(2.0),
    XYZREF: Float32Array.from([0.0, 0.0, 0.0]),
    VINF: Float32Array.from([0.9, 0.1, 0.05]),
    WROT: Float32Array.from([0.01, -0.02, 0.03]),

    NBODY,
    NLNODE,
    NUMAX,
    NL: Int32Array.from([2]),
    LFRST: Int32Array.from([1]),
    RL: new Float32Array(4 * (NLNODE + 1)),
    RADL: Float32Array.from([0.0, 0.10, 0.12]),
    SRC: Float32Array.from([0.0, 0.8, 0.0]),
    SRC_U: srcU,

    CDBDY: new Float32Array(NBODY),
    CYBDY: new Float32Array(NBODY),
    CLBDY: new Float32Array(NBODY),
    CFBDY: new Float32Array(3 * NBODY),
    CMBDY: new Float32Array(3 * NBODY),
    DCPB: new Float32Array(3 * (NLNODE + 1)),

    CDTOT: Math.fround(0.0),
    CYTOT: Math.fround(0.0),
    CLTOT: Math.fround(0.0),
    CFTOT: new Float32Array(3),
    CMTOT: new Float32Array(3),
    CDTOT_U: new Float32Array(NUMAX),
    CYTOT_U: new Float32Array(NUMAX),
    CLTOT_U: new Float32Array(NUMAX),
    CFTOT_U: new Float32Array(3 * NUMAX),
    CMTOT_U: new Float32Array(3 * NUMAX),
  };

  state.RL[idx4(0, 1)] = 0.0;
  state.RL[idx4(1, 1)] = 0.0;
  state.RL[idx4(2, 1)] = 0.0;
  state.RL[idx4(0, 2)] = 1.0;
  state.RL[idx4(1, 2)] = 0.5;
  state.RL[idx4(2, 2)] = 0.2;

  BDFORC(state);

  const out = [];
  out.push(state.CDBDY[0], state.CYBDY[0], state.CLBDY[0]);
  for (let l = 0; l < 3; l += 1) out.push(state.CFBDY[idx2(l, 0, 3)]);
  for (let l = 0; l < 3; l += 1) out.push(state.CMBDY[idx2(l, 0, 3)]);
  for (let l = 0; l < 3; l += 1) out.push(state.DCPB[idx2(l, 1, 3)]);
  out.push(state.CDTOT, state.CYTOT, state.CLTOT);
  for (let l = 0; l < 3; l += 1) out.push(state.CFTOT[l]);
  for (let l = 0; l < 3; l += 1) out.push(state.CMTOT[l]);
  out.push(...state.CDTOT_U);
  out.push(...state.CYTOT_U);
  out.push(...state.CLTOT_U);
  for (let iu = 0; iu < NUMAX; iu += 1) {
    for (let l = 0; l < 3; l += 1) out.push(state.CFTOT_U[idx2(l, iu, 3)]);
  }
  for (let iu = 0; iu < NUMAX; iu += 1) {
    for (let l = 0; l < 3; l += 1) out.push(state.CMTOT_U[idx2(l, iu, 3)]);
  }

  assertCloseArray(out.slice(0, 3), refCDB);
  assertCloseArray(out.slice(3, 6), refCFB);
  assertCloseArray(out.slice(6, 9), refCMB);
  assertCloseArray(out.slice(9, 12), refDCPB);
  assertCloseArray(out.slice(12, 15), refTotals);
  assertCloseArray(out.slice(15, 18), refCFTOT);
  assertCloseArray(out.slice(18, 21), refCMTOT);
  assertCloseArray(out.slice(21, 27), refCDTOT_U);
  assertCloseArray(out.slice(27, 33), refCYTOT_U);
  assertCloseArray(out.slice(33, 39), refCLTOT_U);
  assertCloseArray(out.slice(39, 57), refCFTOT_U);
  assertCloseArray(out.slice(57, 75), refCMTOT_U);
});

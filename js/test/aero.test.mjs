import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { AERO, VINFAB } from '../src/aero.js';

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

function idx3(r, c) {
  return r + 3 * c;
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx4(r, c) {
  return (r + 1) + 4 * c;
}

test('AERO JS matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'aero_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('aero_ref');
  let offset = 0;
  const refScalars = ref.slice(offset, offset + 3); offset += 3; // CDTOT CYTOT CLTOT
  const refCFTOT = ref.slice(offset, offset + 3); offset += 3;
  const refCMTOT = ref.slice(offset, offset + 3); offset += 3;
  const refMore = ref.slice(offset, offset + 5); offset += 5; // CDVTOT CLFF CYFF CDFF SPANEF
  const refDCP = ref.slice(offset, offset + 2); offset += 2; // DCP(1) CNC(1)
  const refCFSTRP = ref.slice(offset, offset + 3); offset += 3;
  const refCMSTRP = ref.slice(offset, offset + 3); offset += 3;
  const refStrip = ref.slice(offset, offset + 3); offset += 3; // CDSTRP CYSTRP CLSTRP
  const refSurf = ref.slice(offset, offset + 4); offset += 4; // CDVSURF CDSURF CYSURF CLSURF

  const NSTRIP = 1;
  const NVOR = 1;
  const NSURF = 1;
  const NUMAX = 6;

  const state = {
    PI: Math.fround(3.14159265),
    ALFA: Math.fround(0.1),
    BETA: Math.fround(0.05),
    MACH: Math.fround(0.3),
    AMACH: Math.fround(0.3),
    IYSYM: 0,
    IZSYM: 0,
    YSYM: Math.fround(0.0),
    ZSYM: Math.fround(0.0),
    VRCOREC: Math.fround(0.01),
    VRCOREW: Math.fround(0.02),
    SREF: Math.fround(1.5),
    CREF: Math.fround(1.0),
    BREF: Math.fround(2.0),
    CDREF: Math.fround(0.02),
    XYZREF: Float32Array.from([0.0, 0.0, 0.0]),
    VINF: new Float32Array(3),
    VINF_A: new Float32Array(3),
    VINF_B: new Float32Array(3),
    WROT: Float32Array.from([0.01, -0.02, 0.03]),

    NSTRIP,
    NVOR,
    NSURF,
    NBODY: 0,
    NCONTROL: 0,
    NDESIGN: 0,
    NUMAX,

    LTRFORCE: false,
    LNFLD_WV: false,
    LVISC: false,
    LVISCSTRP: new Uint8Array(NSTRIP + 1),
    LSTRIPOFF: new Uint8Array(NSTRIP + 1),

    IJFRST: Int32Array.from([0, 1]),
    NVSTRP: Int32Array.from([0, 1]),
    JFRST: Int32Array.from([0, 1]),
    NJ: Int32Array.from([0, 1]),
    LSSURF: Int32Array.from([0, 1]),
    IMAGS: Int32Array.from([0, 1]),
    LFLOAD: [false, true],
    LNCOMP: Int32Array.from([0, 1]),

    CHORD: Float32Array.from([0.0, 1.0]),
    WSTRIP: Float32Array.from([0.0, 0.5]),
    CHORD1: Float32Array.from([0.0, 1.0]),
    CHORD2: Float32Array.from([0.0, 1.0]),
    RLE1: new Float32Array(4 * (NSTRIP + 1)),
    RLE2: new Float32Array(4 * (NSTRIP + 1)),
    RLE: new Float32Array(4 * (NSTRIP + 1)),
    ENSY: Float32Array.from([0.0, 0.0]),
    ENSZ: Float32Array.from([0.0, 1.0]),
    ESS: new Float32Array(4 * (NSTRIP + 1)),
    AINC: Float32Array.from([0.0, 0.0]),
    XSREF: Float32Array.from([0.0, 0.25]),
    YSREF: Float32Array.from([0.0, 0.0]),
    ZSREF: Float32Array.from([0.0, 0.0]),
    SSURF: Float32Array.from([0.0, 0.5]),
    CAVESURF: Float32Array.from([0.0, 1.0]),

    RV1: new Float32Array(4 * (NVOR + 1)),
    RV2: new Float32Array(4 * (NVOR + 1)),
    RV: new Float32Array(4 * (NVOR + 1)),
    RC: new Float32Array(4 * (NVOR + 1)),
    DXV: Float32Array.from([0.0, 1.0]),
    ENV: new Float32Array(4 * (NVOR + 1)),

    VV: new Float32Array(4 * (NVOR + 1)),
    VV_U: new Float32Array(4 * (NVOR + 1) * NUMAX),
    VV_D: new Float32Array(3 * NVOR * 0),
    VV_G: new Float32Array(3 * NVOR * 0),
    WV: new Float32Array(4 * (NVOR + 1)),
    WV_U: new Float32Array(4 * (NVOR + 1) * NUMAX),
    WV_D: new Float32Array(3 * NVOR * 0),
    WV_G: new Float32Array(3 * NVOR * 0),

    GAM: Float32Array.from([0.0, 0.4]),
    GAM_U: (() => {
      const out = new Float32Array((NVOR + 1) * NUMAX);
      for (let n = 0; n < NUMAX; n += 1) {
        out[idx2(1, n, NVOR + 1)] = Math.fround(0.01 * (n + 1));
      }
      return out;
    })(),
    GAM_D: new Float32Array(NVOR * 0),
    GAM_G: new Float32Array(NVOR * 0),

    DCP: new Float32Array(NVOR + 1),
    DCP_U: new Float32Array((NVOR + 1) * NUMAX),
    DCP_D: new Float32Array(NVOR * 0),
    DCP_G: new Float32Array(NVOR * 0),

    CNC: new Float32Array(NSTRIP + 1),
    CNC_U: new Float32Array((NSTRIP + 1) * NUMAX),
    CNC_D: new Float32Array(NSTRIP * 0),
    CNC_G: new Float32Array(NSTRIP * 0),

    PHINGE: new Float32Array(3 * NSTRIP * 0),
    VHINGE: new Float32Array(3 * NSTRIP * 0),
    DCONTROL: new Float32Array(NVOR * 0),

    CF_LSTRP: new Float32Array(3 * (NSTRIP + 1)),
    CM_LSTRP: new Float32Array(3 * (NSTRIP + 1)),
    CFSTRP: new Float32Array(3 * (NSTRIP + 1)),
    CMSTRP: new Float32Array(3 * (NSTRIP + 1)),
    CDSTRP: new Float32Array(NSTRIP + 1),
    CYSTRP: new Float32Array(NSTRIP + 1),
    CLSTRP: new Float32Array(NSTRIP + 1),
    CDST_A: new Float32Array(NSTRIP + 1),
    CYST_A: new Float32Array(NSTRIP + 1),
    CLST_A: new Float32Array(NSTRIP + 1),
    CDST_U: new Float32Array((NSTRIP + 1) * NUMAX),
    CYST_U: new Float32Array((NSTRIP + 1) * NUMAX),
    CLST_U: new Float32Array((NSTRIP + 1) * NUMAX),
    CFST_U: new Float32Array(3 * (NSTRIP + 1) * NUMAX),
    CMST_U: new Float32Array(3 * (NSTRIP + 1) * NUMAX),
    CDST_D: new Float32Array(0),
    CYST_D: new Float32Array(0),
    CLST_D: new Float32Array(0),
    CFST_D: new Float32Array(0),
    CMST_D: new Float32Array(0),
    CDST_G: new Float32Array(0),
    CYST_G: new Float32Array(0),
    CLST_G: new Float32Array(0),
    CFST_G: new Float32Array(0),
    CMST_G: new Float32Array(0),

    CL_LSTRP: new Float32Array(NSTRIP + 1),
    CD_LSTRP: new Float32Array(NSTRIP + 1),
    CMC4_LSTRP: new Float32Array(NSTRIP + 1),
    CA_LSTRP: new Float32Array(NSTRIP + 1),
    CN_LSTRP: new Float32Array(NSTRIP + 1),
    CLT_LSTRP: new Float32Array(NSTRIP + 1),
    CLA_LSTRP: new Float32Array(NSTRIP + 1),
    CMLE_LSTRP: new Float32Array(NSTRIP + 1),
    CDV_LSTRP: new Float32Array(NSTRIP + 1),

    CF_LSRF: new Float32Array(3 * (NSURF + 1)),
    CM_LSRF: new Float32Array(3 * (NSURF + 1)),
    CDSURF: new Float32Array(NSURF + 1),
    CYSURF: new Float32Array(NSURF + 1),
    CLSURF: new Float32Array(NSURF + 1),
    CFSURF: new Float32Array(3 * (NSURF + 1)),
    CMSURF: new Float32Array(3 * (NSURF + 1)),
    CDVSURF: new Float32Array(NSURF + 1),
    CDS_A: new Float32Array(NSURF + 1),
    CYS_A: new Float32Array(NSURF + 1),
    CLS_A: new Float32Array(NSURF + 1),
    CDS_U: new Float32Array((NSURF + 1) * NUMAX),
    CYS_U: new Float32Array((NSURF + 1) * NUMAX),
    CLS_U: new Float32Array((NSURF + 1) * NUMAX),
    CFS_U: new Float32Array(3 * (NSURF + 1) * NUMAX),
    CMS_U: new Float32Array(3 * (NSURF + 1) * NUMAX),
    CDS_D: new Float32Array(0),
    CYS_D: new Float32Array(0),
    CLS_D: new Float32Array(0),
    CFS_D: new Float32Array(0),
    CMS_D: new Float32Array(0),
    CDS_G: new Float32Array(0),
    CYS_G: new Float32Array(0),
    CLS_G: new Float32Array(0),
    CFS_G: new Float32Array(0),
    CMS_G: new Float32Array(0),

    CL_LSRF: new Float32Array(NSURF + 1),
    CD_LSRF: new Float32Array(NSURF + 1),

    CLCD: new Float32Array(6 * (NSTRIP + 1)),

    CHINGE: new Float32Array(0),
    CHINGE_U: new Float32Array(0),
    CHINGE_D: new Float32Array(0),
    CHINGE_G: new Float32Array(0),

    CDTOT: 0.0,
    CYTOT: 0.0,
    CLTOT: 0.0,
    CFTOT: new Float32Array(3),
    CMTOT: new Float32Array(3),
    CDVTOT: 0.0,
    CDTOT_A: 0.0,
    CLTOT_A: 0.0,
    CDTOT_U: new Float32Array(NUMAX),
    CYTOT_U: new Float32Array(NUMAX),
    CLTOT_U: new Float32Array(NUMAX),
    CDTOT_D: new Float32Array(0),
    CYTOT_D: new Float32Array(0),
    CLTOT_D: new Float32Array(0),
    CDTOT_G: new Float32Array(0),
    CYTOT_G: new Float32Array(0),
    CLTOT_G: new Float32Array(0),
    CFTOT_U: new Float32Array(3 * NUMAX),
    CMTOT_U: new Float32Array(3 * NUMAX),
    CFTOT_D: new Float32Array(0),
    CMTOT_D: new Float32Array(0),
    CFTOT_G: new Float32Array(0),
    CMTOT_G: new Float32Array(0),

    CLFF: 0.0,
    CYFF: 0.0,
    CDFF: 0.0,
    SPANEF: 0.0,
    CLFF_U: new Float32Array(NUMAX),
    CYFF_U: new Float32Array(NUMAX),
    CDFF_U: new Float32Array(NUMAX),
    SPANEF_U: new Float32Array(NUMAX),
    DWWAKE: new Float32Array(NSTRIP + 1),
  };

  state.RLE1[idx4(0, 1)] = 0.0;
  state.RLE1[idx4(1, 1)] = 0.0;
  state.RLE1[idx4(2, 1)] = 0.0;
  state.RLE2[idx4(0, 1)] = 0.0;
  state.RLE2[idx4(1, 1)] = 1.0;
  state.RLE2[idx4(2, 1)] = 0.0;
  state.RLE[idx4(0, 1)] = 0.0;
  state.RLE[idx4(1, 1)] = 0.0;
  state.RLE[idx4(2, 1)] = 0.0;

  state.ESS[idx4(0, 1)] = 0.0;
  state.ESS[idx4(1, 1)] = 1.0;
  state.ESS[idx4(2, 1)] = 0.0;

  state.RV1[idx4(0, 1)] = 0.0;
  state.RV1[idx4(1, 1)] = -0.5;
  state.RV1[idx4(2, 1)] = 0.0;
  state.RV2[idx4(0, 1)] = 1.0;
  state.RV2[idx4(1, 1)] = 0.5;
  state.RV2[idx4(2, 1)] = 0.0;
  state.RV[idx4(0, 1)] = 0.5;
  state.RV[idx4(1, 1)] = 0.0;
  state.RV[idx4(2, 1)] = 0.0;
  state.RC[idx4(0, 1)] = 0.25;
  state.RC[idx4(1, 1)] = 0.0;
  state.RC[idx4(2, 1)] = 0.0;
  state.ENV[idx4(0, 1)] = 0.0;
  state.ENV[idx4(1, 1)] = 0.0;
  state.ENV[idx4(2, 1)] = 1.0;

  state.VV[idx4(0, 1)] = 0.01;
  state.VV[idx4(1, 1)] = 0.02;
  state.VV[idx4(2, 1)] = 0.03;

  VINFAB(state);
  AERO(state);

  const scalars = [state.CDTOT, state.CYTOT, state.CLTOT];
  assertCloseArray(scalars, refScalars);
  assertCloseArray(Array.from(state.CFTOT), refCFTOT);
  assertCloseArray(Array.from(state.CMTOT), refCMTOT);
  assertCloseArray([
    state.CDVTOT,
    state.CLFF,
    state.CYFF,
    state.CDFF,
    state.SPANEF,
  ], refMore);
  assertCloseArray([state.DCP[1], state.CNC[1]], refDCP);
  assertCloseArray(Array.from(state.CFSTRP.slice(3, 6)), refCFSTRP);
  assertCloseArray(Array.from(state.CMSTRP.slice(3, 6)), refCMSTRP);
  assertCloseArray([state.CDSTRP[1], state.CYSTRP[1], state.CLSTRP[1]], refStrip);
  assertCloseArray([state.CDVSURF[1], state.CDSURF[1], state.CYSURF[1], state.CLSURF[1]], refSurf);
});

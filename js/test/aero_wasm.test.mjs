import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { loadAeroWasm } from '../src/aero_wasm.js';
import { VINFAB } from '../src/aero.js';

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

test('AERO WASM matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'aero_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('aero_ref');
  let offset = 0;
  const refScalars = ref.slice(offset, offset + 3); offset += 3;
  const refCFTOT = ref.slice(offset, offset + 3); offset += 3;
  const refCMTOT = ref.slice(offset, offset + 3); offset += 3;
  const refMore = ref.slice(offset, offset + 5); offset += 5;
  const refDCP = ref.slice(offset, offset + 2); offset += 2;
  const refCFSTRP = ref.slice(offset, offset + 3); offset += 3;
  const refCMSTRP = ref.slice(offset, offset + 3); offset += 3;
  const refStrip = ref.slice(offset, offset + 3); offset += 3;
  const refSurf = ref.slice(offset, offset + 4); offset += 4;

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
    LVISCSTRP: [false],

    IJFRST: Int32Array.from([0]),
    NVSTRP: Int32Array.from([1]),
    JFRST: Int32Array.from([0]),
    NJ: Int32Array.from([1]),
    LSSURF: Int32Array.from([0]),
    IMAGS: Int32Array.from([1]),
    LFLOAD: [true],
    LNCOMP: Int32Array.from([1]),

    CHORD: Float32Array.from([1.0]),
    WSTRIP: Float32Array.from([0.5]),
    CHORD1: Float32Array.from([1.0]),
    CHORD2: Float32Array.from([1.0]),
    RLE1: Float32Array.from([0.0, 0.0, 0.0]),
    RLE2: Float32Array.from([0.0, 1.0, 0.0]),
    RLE: Float32Array.from([0.0, 0.0, 0.0]),
    ENSY: Float32Array.from([0.0]),
    ENSZ: Float32Array.from([1.0]),
    ESS: Float32Array.from([0.0, 1.0, 0.0]),
    AINC: Float32Array.from([0.0]),
    XSREF: Float32Array.from([0.25]),
    YSREF: Float32Array.from([0.0]),
    ZSREF: Float32Array.from([0.0]),
    SSURF: Float32Array.from([0.5]),
    CAVESURF: Float32Array.from([1.0]),

    RV1: Float32Array.from([0.0, -0.5, 0.0]),
    RV2: Float32Array.from([1.0, 0.5, 0.0]),
    RV: Float32Array.from([0.5, 0.0, 0.0]),
    RC: Float32Array.from([0.25, 0.0, 0.0]),
    DXV: Float32Array.from([1.0]),
    ENV: Float32Array.from([0.0, 0.0, 1.0]),

    VV: Float32Array.from([0.01, 0.02, 0.03]),
    VV_U: new Float32Array(3 * NVOR * NUMAX),
    VV_D: new Float32Array(3 * NVOR * 0),
    VV_G: new Float32Array(3 * NVOR * 0),
    WV: new Float32Array(3 * NVOR),
    WV_U: new Float32Array(3 * NVOR * NUMAX),
    WV_D: new Float32Array(3 * NVOR * 0),
    WV_G: new Float32Array(3 * NVOR * 0),

    GAM: Float32Array.from([0.4]),
    GAM_U: (() => {
      const out = new Float32Array(NVOR * NUMAX);
      for (let n = 0; n < NUMAX; n += 1) {
        out[n] = Math.fround(0.01 * (n + 1));
      }
      return out;
    })(),
    GAM_D: new Float32Array(NVOR * 0),
    GAM_G: new Float32Array(NVOR * 0),

    DCP: new Float32Array(NVOR),
    DCP_U: new Float32Array(NVOR * NUMAX),
    DCP_D: new Float32Array(NVOR * 0),
    DCP_G: new Float32Array(NVOR * 0),

    CNC: new Float32Array(NSTRIP),
    CNC_U: new Float32Array(NSTRIP * NUMAX),
    CNC_D: new Float32Array(NSTRIP * 0),
    CNC_G: new Float32Array(NSTRIP * 0),

    PHINGE: new Float32Array(3 * NSTRIP * 0),
    VHINGE: new Float32Array(3 * NSTRIP * 0),
    DCONTROL: new Float32Array(NVOR * 0),

    CF_LSTRP: new Float32Array(3 * NSTRIP),
    CM_LSTRP: new Float32Array(3 * NSTRIP),
    CFSTRP: new Float32Array(3 * NSTRIP),
    CMSTRP: new Float32Array(3 * NSTRIP),
    CDSTRP: new Float32Array(NSTRIP),
    CYSTRP: new Float32Array(NSTRIP),
    CLSTRP: new Float32Array(NSTRIP),
    CDST_A: new Float32Array(NSTRIP),
    CYST_A: new Float32Array(NSTRIP),
    CLST_A: new Float32Array(NSTRIP),
    CDST_U: new Float32Array(NSTRIP * NUMAX),
    CYST_U: new Float32Array(NSTRIP * NUMAX),
    CLST_U: new Float32Array(NSTRIP * NUMAX),
    CFST_U: new Float32Array(3 * NSTRIP * NUMAX),
    CMST_U: new Float32Array(3 * NSTRIP * NUMAX),
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

    CL_LSTRP: new Float32Array(NSTRIP),
    CD_LSTRP: new Float32Array(NSTRIP),
    CMC4_LSTRP: new Float32Array(NSTRIP),
    CA_LSTRP: new Float32Array(NSTRIP),
    CN_LSTRP: new Float32Array(NSTRIP),
    CLT_LSTRP: new Float32Array(NSTRIP),
    CLA_LSTRP: new Float32Array(NSTRIP),
    CMLE_LSTRP: new Float32Array(NSTRIP),
    CDV_LSTRP: new Float32Array(NSTRIP),

    CF_LSRF: new Float32Array(3 * NSURF),
    CM_LSRF: new Float32Array(3 * NSURF),
    CDSURF: new Float32Array(NSURF),
    CYSURF: new Float32Array(NSURF),
    CLSURF: new Float32Array(NSURF),
    CFSURF: new Float32Array(3 * NSURF),
    CMSURF: new Float32Array(3 * NSURF),
    CDVSURF: new Float32Array(NSURF),
    CDS_A: new Float32Array(NSURF),
    CYS_A: new Float32Array(NSURF),
    CLS_A: new Float32Array(NSURF),
    CDS_U: new Float32Array(NSURF * NUMAX),
    CYS_U: new Float32Array(NSURF * NUMAX),
    CLS_U: new Float32Array(NSURF * NUMAX),
    CFS_U: new Float32Array(3 * NSURF * NUMAX),
    CMS_U: new Float32Array(3 * NSURF * NUMAX),
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

    CL_LSRF: new Float32Array(NSURF),
    CD_LSRF: new Float32Array(NSURF),

    CLCD: new Float32Array(6 * NSTRIP),

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
    DWWAKE: new Float32Array(NSTRIP),
  };

  VINFAB(state);
  const wasm = await loadAeroWasm();
  const out = wasm.AERO(state);

  assertCloseArray([out.CDTOT, out.CYTOT, out.CLTOT], refScalars);
  assertCloseArray(Array.from(out.CFTOT), refCFTOT);
  assertCloseArray(Array.from(out.CMTOT), refCMTOT);
  assertCloseArray([
    out.CDVTOT,
    out.CLFF,
    out.CYFF,
    out.CDFF,
    out.SPANEF,
  ], refMore);
  assertCloseArray([out.DCP[0], out.CNC[0]], refDCP);
  assertCloseArray(Array.from(out.CFSTRP), refCFSTRP);
  assertCloseArray(Array.from(out.CMSTRP), refCMSTRP);
  assertCloseArray([out.CDSTRP[0], out.CYSTRP[0], out.CLSTRP[0]], refStrip);
  assertCloseArray([out.CDVSURF[0], out.CDSURF[0], out.CYSURF[0], out.CLSURF[0]], refSurf);
});

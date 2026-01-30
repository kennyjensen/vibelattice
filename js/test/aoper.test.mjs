import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { EXEC } from '../src/aoper.js';
import { loadAoperWasm } from '../src/aoper_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'exec_ref');
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

function idx2_0(i, j, dim1) {
  return i + dim1 * j;
}

function idx3(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

function makeState() {
  const IVALFA = 1;
  const IVBETA = 2;
  const IVROTX = 3;
  const IVROTY = 4;
  const IVROTZ = 5;
  const IVTOT = 5;

  const ICALFA = 1;
  const ICBETA = 2;
  const ICROTX = 3;
  const ICROTY = 4;
  const ICROTZ = 5;
  const ICCL = 6;
  const ICCY = 7;
  const ICMOMX = 8;
  const ICMOMY = 9;
  const ICMOMZ = 10;
  const ICTOT = 10;

  const IPALFA = 1;
  const IPBETA = 2;
  const IPROTX = 3;
  const IPROTY = 4;
  const IPROTZ = 5;
  const IPCL = 6;
  const IPCD0 = 7;
  const IPMACH = 11;
  const IPXCG = 17;
  const IPYCG = 18;
  const IPZCG = 19;
  const IPTOT = 30;

  const NVOR = 1;
  const NLNODE = 0;
  const NCONTROL = 0;
  const NDESIGN = 0;
  const NUMAX = 6;
  const NDMAX = 1;
  const NGMAX = 1;
  const NVMAX = 1;
  const IVMAX = IVTOT + NDMAX;
  const ICMAX = ICTOT + NDMAX;
  const NRMAX = 1;

  const DIM_N = NVOR + 1;
  const DIM_U = NUMAX + 1;
  const DIM_C = NDMAX + 1;
  const DIM_G = NGMAX + 1;
  const DIM_L = NLNODE + 1;
  const DIM_K = 4;

  const state = {
    IVALFA,
    IVBETA,
    IVROTX,
    IVROTY,
    IVROTZ,
    IVTOT,
    ICALFA,
    ICBETA,
    ICROTX,
    ICROTY,
    ICROTZ,
    ICCL,
    ICCY,
    ICMOMX,
    ICMOMY,
    ICMOMZ,
    ICTOT,
    IPALFA,
    IPBETA,
    IPROTX,
    IPROTY,
    IPROTZ,
    IPCL,
    IPCD0,
    IPMACH,
    IPXCG,
    IPYCG,
    IPZCG,
    IPTOT,
    NVOR,
    NLNODE,
    NCONTROL,
    NDESIGN,
    NUMAX,
    NDMAX,
    NGMAX,
    NVMAX,
    IVMAX,
    ICMAX,
    NRMAX,
    NVTOT: IVTOT,
    NSTRIP: 0,
    NSURF: 0,
    NBODY: 0,
    DIM_N,
    DIM_U,
    DIM_C,
    DIM_G,
    DIM_L,
    DIM_K,

    PI: Math.fround(3.14159274),
    DTR: Math.fround(3.14159274 / 180.0),
    IYSYM: 0,
    IZSYM: 0,
    YSYM: 0.0,
    ZSYM: 0.0,
    VRCOREC: 0.0,
    VRCOREW: 0.0,
    SRCORE: 0.0,

    LNASA_SA: false,
    LSA_RATES: false,
    LAIC: true,
    LSRD: true,
    LVEL: true,
    LSOL: false,
    LSEN: false,
    LOBAIC: false,
    LOBVEL: false,

    LTRFORCE: false,
    LNFLD_WV: false,
    LVISC: false,
    LBFORCE: false,

    ALFA: Math.fround(0.1),
    BETA: Math.fround(0.05),
    MACH: Math.fround(0.2),
    AMACH: Math.fround(0.2),
    BETM: 0.0,
    VINF: new Float32Array(3),
    VINF_A: new Float32Array(3),
    VINF_B: new Float32Array(3),
    WROT: new Float32Array([0.01, -0.02, 0.03]),
    XYZREF: new Float32Array(3),
    SREF: Math.fround(1.0),
    CREF: Math.fround(1.0),
    BREF: Math.fround(1.0),
    CDREF: Math.fround(0.01),

    PARVAL: new Float32Array((IPTOT + 1) * (NRMAX + 1)),
    CONVAL: new Float32Array((ICMAX + 1) * (NRMAX + 1)),
    ICON: new Int32Array((IVTOT + 1) * (NRMAX + 1)),
    ITRIM: new Int32Array(NRMAX + 1),

    DELCON: new Float32Array(DIM_C),
    DELDES: new Float32Array(DIM_G),

    AICN: new Float32Array((NVMAX + 1) * (NVMAX + 1)),
    IAPIV: new Int32Array(NVMAX + 1),
    WORK: new Float32Array(NVMAX + 1),

    LVNC: new Array(DIM_N).fill(false),
    LVALBE: new Array(DIM_N).fill(false),
    LCONDEF: new Array(DIM_C).fill(false),
    LDESDEF: new Array(DIM_G).fill(false),

    ENC: new Float32Array(4 * DIM_N),
    ENC_D: new Float32Array(4 * DIM_N * DIM_C),
    ENC_G: new Float32Array(4 * DIM_N * DIM_G),

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
    WCSRD_U: new Float32Array(DIM_K * DIM_N * DIM_U),
    WVSRD_U: new Float32Array(DIM_K * DIM_N * DIM_U),
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

    CFTOT: new Float32Array(3),
    CMTOT: new Float32Array(3),
    CFTOT_U: new Float32Array(3 * NUMAX),
    CMTOT_U: new Float32Array(3 * NUMAX),
    CFTOT_D: new Float32Array(0),
    CMTOT_D: new Float32Array(0),
    CFTOT_G: new Float32Array(0),
    CMTOT_G: new Float32Array(0),
    CDTOT: 0.0,
    CYTOT: 0.0,
    CLTOT: 0.0,
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

    CHINGE: new Float32Array(0),
    CHINGE_U: new Float32Array(0),
    CHINGE_D: new Float32Array(0),
    CHINGE_G: new Float32Array(0),

    IJFRST: new Int32Array(0),
    NVSTRP: new Int32Array(0),
    JFRST: new Int32Array(0),
    NJ: new Int32Array(0),
    LSSURF: new Int32Array(0),
    IMAGS: new Int32Array(0),
    LNCOMP: new Int32Array(0),
    LFRST: new Int32Array(0),
    NLNODE: new Int32Array(0),
    LFLOAD: new Array(0),
    LVISCSTRP: new Array(0),

    CHORD: new Float32Array(0),
    WSTRIP: new Float32Array(0),
    CHORD1: new Float32Array(0),
    CHORD2: new Float32Array(0),
    RLE1: new Float32Array(0),
    RLE2: new Float32Array(0),
    RLE: new Float32Array(0),
    ENSY: new Float32Array(0),
    ENSZ: new Float32Array(0),
    ESS: new Float32Array(0),
    AINC: new Float32Array(0),
    XSREF: new Float32Array(0),
    YSREF: new Float32Array(0),
    ZSREF: new Float32Array(0),
    SSURF: new Float32Array(0),
    CAVESURF: new Float32Array(0),

    RV1: new Float32Array(0),
    RV2: new Float32Array(0),
    RV: new Float32Array(0),
    RC: new Float32Array(4 * DIM_N),
    DXV: new Float32Array(0),
    ENV: new Float32Array(0),
    ENV_D: new Float32Array(0),
    ENV_G: new Float32Array(0),

    GAM_U_0_buf: null,
  };

  state.PARVAL[idx2(IPMACH, 1, IPTOT)] = Math.fround(0.2);
  state.PARVAL[idx2(IPCD0, 1, IPTOT)] = Math.fround(0.01);
  state.PARVAL[idx2(IPXCG, 1, IPTOT)] = Math.fround(0.1);
  state.PARVAL[idx2(IPYCG, 1, IPTOT)] = Math.fround(0.2);
  state.PARVAL[idx2(IPZCG, 1, IPTOT)] = Math.fround(0.3);

  state.AICN[idx2(1, 1, NVMAX + 1)] = Math.fround(1.0);
  state.IAPIV[1] = 1;

  state.LVNC[1] = true;
  state.LVALBE[1] = false;
  state.ENC[idx2(3, 1, 4)] = Math.fround(1.0);

  for (let k = 1; k <= 3; k += 1) {
    state.WC_GAM[idx3(k, 1, 1, DIM_K, DIM_N)] = Math.fround(0.1 * k);
    state.WV_GAM[idx3(k, 1, 1, DIM_K, DIM_N)] = Math.fround(0.2 * k);
  }

  return state;
}

function extractOutputs(state) {
  const out = [];
  out.push(state.VINF[0], state.VINF[1], state.VINF[2]);
  out.push(state.WROT[0], state.WROT[1], state.WROT[2]);
  out.push(state.GAM[1]);
  out.push(state.WC[idx2_0(1, 1, 4)], state.WC[idx2_0(2, 1, 4)], state.WC[idx2_0(3, 1, 4)]);
  out.push(state.WV[idx2_0(1, 1, 4)], state.WV[idx2_0(2, 1, 4)], state.WV[idx2_0(3, 1, 4)]);
  out.push(
    state.PARVAL[idx2(state.IPALFA, 1, state.IPTOT)],
    state.PARVAL[idx2(state.IPBETA, 1, state.IPTOT)],
    state.PARVAL[idx2(state.IPCL, 1, state.IPTOT)],
  );
  return out;
}

test('EXEC JS matches Fortran reference', () => {
  const refBin = path.join(refDir, 'exec_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();
  const state = makeState();
  EXEC(state, 0, 0, 1);
  const actual = extractOutputs(state);
  assertCloseArray(actual, ref, 1e-4);
});

test('EXEC wasm matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'exec_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();
  const { EXEC_wasm } = await loadAoperWasm();
  const state = makeState();
  EXEC_wasm(state, 0, 0, 1);
  const actual = extractOutputs(state);
  assertCloseArray(actual, ref, 1e-4);
});

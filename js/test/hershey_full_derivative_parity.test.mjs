import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
} from '../src/exec_pipeline.js';
import {
  EXEC,
  preloadAoperLinSolveWasm,
  preloadAsetupWasm,
  preloadAeroWasm,
  preloadAicWasmBridge,
  preloadAsetpLuWasmBridge,
} from '../src/aoper.js';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const fixtures = [
  path.join(repoRoot, 'js', 'test', 'data', 'hershey_fortran_derivatives.json'),
  path.join(repoRoot, 'js', 'test', 'data', 'plane_fortran_derivatives.json'),
];

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function computeDerivativesFortranStyle(state) {
  const out = { stability: {}, body: {} };
  const dir = state.LNASA_SA ? -1.0 : 1.0;
  const ca = Math.cos(state.ALFA);
  const sa = Math.sin(state.ALFA);
  const w0 = Number(state.WROT[0] ?? 0.0);
  const w1 = Number(state.WROT[1] ?? 0.0);
  const w2 = Number(state.WROT[2] ?? 0.0);

  const RX = (w0 * ca + w2 * sa) * dir;
  const RY = w1;
  const RZ = (w2 * ca - w0 * sa) * dir;
  void RY;

  const WROT_RX = [ca * dir, 0.0, sa * dir];
  const WROT_RZ = [-sa * dir, 0.0, ca * dir];
  const WROT_A = [-RX * sa - RZ * ca, 0.0, -RZ * sa + RX * ca];

  const vinfA = [
    Number(state.VINF_A?.[0] ?? 0.0),
    Number(state.VINF_A?.[1] ?? 0.0),
    Number(state.VINF_A?.[2] ?? 0.0),
  ];
  const vinfB = [
    Number(state.VINF_B?.[0] ?? 0.0),
    Number(state.VINF_B?.[1] ?? 0.0),
    Number(state.VINF_B?.[2] ?? 0.0),
  ];

  const clU = Array.from({ length: 6 }, (_, k) => Number(state.CLTOT_U[k] ?? 0.0));
  const cyU = Array.from({ length: 6 }, (_, k) => Number(state.CYTOT_U[k] ?? 0.0));
  const cdU = Array.from({ length: 6 }, (_, k) => Number(state.CDTOT_U[k] ?? 0.0));
  const cmU = (row, k) => Number(state.CMTOT_U[idx2(row, k, 3)] ?? 0.0);
  const cmD = (row, k) => Number(state.CMTOT_D[idx2(row, k, 3)] ?? 0.0);

  const CRSAX_U = Array.from({ length: 6 }, (_, k) => cmU(0, k) * ca + cmU(2, k) * sa);
  const CMSAX_U = Array.from({ length: 6 }, (_, k) => cmU(1, k));
  const CNSAX_U = Array.from({ length: 6 }, (_, k) => cmU(2, k) * ca - cmU(0, k) * sa);

  const CRSAX_A = -Number(state.CMTOT[0] ?? 0.0) * sa + Number(state.CMTOT[2] ?? 0.0) * ca;
  const CNSAX_A = -Number(state.CMTOT[2] ?? 0.0) * sa - Number(state.CMTOT[0] ?? 0.0) * ca;

  const alpha = (arr6, extra = 0.0) => (
    arr6[0] * vinfA[0] + arr6[1] * vinfA[1] + arr6[2] * vinfA[2]
    + arr6[3] * WROT_A[0] + arr6[4] * WROT_A[1] + arr6[5] * WROT_A[2]
    + extra
  );
  const beta = (arr6) => arr6[0] * vinfB[0] + arr6[1] * vinfB[1] + arr6[2] * vinfB[2];
  const rx = (arr6) => arr6[3] * WROT_RX[0] + arr6[5] * WROT_RX[2];
  const ry = (arr6) => arr6[4];
  const rz = (arr6) => arr6[5] * WROT_RZ[2] + arr6[3] * WROT_RZ[0];

  const CL_AL = alpha(clU, Number(state.CLTOT_A ?? 0.0));
  const CL_BE = beta(clU);
  const CL_RX = rx(clU);
  const CL_RY = ry(clU);
  const CL_RZ = rz(clU);

  const CY_AL = alpha(cyU);
  const CY_BE = beta(cyU);
  const CY_RX = rx(cyU);
  const CY_RY = ry(cyU);
  const CY_RZ = rz(cyU);

  const CD_AL = alpha(cdU, Number(state.CDTOT_A ?? 0.0));
  const CD_BE = beta(cdU);
  const CD_RX = rx(cdU);
  const CD_RY = ry(cdU);
  const CD_RZ = rz(cdU);

  const CR_AL = alpha(CRSAX_U, CRSAX_A);
  const CR_BE = beta(CRSAX_U);
  const CR_RX = rx(CRSAX_U);
  const CR_RY = ry(CRSAX_U);
  const CR_RZ = rz(CRSAX_U);

  const CM_AL = alpha(CMSAX_U);
  const CM_BE = beta(CMSAX_U);
  const CM_RX = rx(CMSAX_U);
  const CM_RY = ry(CMSAX_U);
  const CM_RZ = rz(CMSAX_U);

  const CN_AL = alpha(CNSAX_U, CNSAX_A);
  const CN_BE = beta(CNSAX_U);
  const CN_RX = rx(CNSAX_U);
  const CN_RY = ry(CNSAX_U);
  const CN_RZ = rz(CNSAX_U);

  const bref = Number(state.BREF);
  const cref = Number(state.CREF);
  const pScale = 2.0 / bref;
  const qScale = 2.0 / cref;
  const rScale = 2.0 / bref;

  out.stability.CLa = CL_AL;
  out.stability.CLb = CL_BE;
  out.stability.CYa = CY_AL;
  out.stability.CYb = CY_BE;
  out.stability.CDa = CD_AL;
  out.stability.CDb = CD_BE;
  out.stability.Cla = dir * CR_AL;
  out.stability.Clb = dir * CR_BE;
  out.stability.Cma = CM_AL;
  out.stability.Cmb = CM_BE;
  out.stability.Cna = dir * CN_AL;
  out.stability.Cnb = dir * CN_BE;
  out.stability.CLp = CL_RX * pScale;
  out.stability.CLq = CL_RY * qScale;
  out.stability.CLr = CL_RZ * rScale;
  out.stability.CYp = CY_RX * pScale;
  out.stability.CYq = CY_RY * qScale;
  out.stability.CYr = CY_RZ * rScale;
  out.stability.CDp = CD_RX * pScale;
  out.stability.CDq = CD_RY * qScale;
  out.stability.CDr = CD_RZ * rScale;
  out.stability.Clp = dir * CR_RX * pScale;
  out.stability.Clq = dir * CR_RY * qScale;
  out.stability.Clr = dir * CR_RZ * rScale;
  out.stability.Cmp = CM_RX * pScale;
  out.stability.Cmq = CM_RY * qScale;
  out.stability.Cmr = CM_RZ * rScale;
  out.stability.Cnp = dir * CN_RX * pScale;
  out.stability.Cnq = dir * CN_RY * qScale;
  out.stability.Cnr = dir * CN_RZ * rScale;

  const crsD1 = cmD(0, 0) * ca + cmD(2, 0) * sa;
  const cmsD1 = cmD(1, 0);
  const cnsD1 = cmD(2, 0) * ca - cmD(0, 0) * sa;
  out.stability.CLd1 = Number(state.CLTOT_D[0] ?? 0.0);
  out.stability.CYd1 = Number(state.CYTOT_D[0] ?? 0.0);
  out.stability.CDd1 = Number(state.CDTOT_D[0] ?? 0.0);
  out.stability.Cld1 = dir * crsD1;
  out.stability.Cmd1 = cmsD1;
  out.stability.Cnd1 = dir * cnsD1;

  const cftU = (row, k) => Number(state.CFTOT_U[idx2(row, k, 3)] ?? 0.0);
  const cftD = (row, k) => Number(state.CFTOT_D[idx2(row, k, 3)] ?? 0.0);

  out.body.CXu = -cftU(0, 0);
  out.body.CXv = -dir * cftU(0, 1);
  out.body.CXw = -cftU(0, 2);
  out.body.CYu = -dir * cftU(1, 0);
  out.body.CYv = -cftU(1, 1);
  out.body.CYw = -dir * cftU(1, 2);
  out.body.CZu = -cftU(2, 0);
  out.body.CZv = -dir * cftU(2, 1);
  out.body.CZw = -cftU(2, 2);
  out.body.Clu = -cmU(0, 0);
  out.body.Clv = -dir * cmU(0, 1);
  out.body.Clw = -cmU(0, 2);
  out.body.Cmu = -dir * cmU(1, 0);
  out.body.Cmv = -cmU(1, 1);
  out.body.Cmw = -dir * cmU(1, 2);
  out.body.Cnu = -cmU(2, 0);
  out.body.Cnv = -dir * cmU(2, 1);
  out.body.Cnw = -cmU(2, 2);
  out.body.CXp = cftU(0, 3) * pScale;
  out.body.CXq = dir * cftU(0, 4) * qScale;
  out.body.CXr = cftU(0, 5) * rScale;
  out.body.CYp = dir * cftU(1, 3) * pScale;
  out.body.CYq = cftU(1, 4) * qScale;
  out.body.CYr = dir * cftU(1, 5) * rScale;
  out.body.CZp = cftU(2, 3) * pScale;
  out.body.CZq = dir * cftU(2, 4) * qScale;
  out.body.CZr = cftU(2, 5) * rScale;
  out.body.Clp = cmU(0, 3) * pScale;
  out.body.Clq = dir * cmU(0, 4) * qScale;
  out.body.Clr = cmU(0, 5) * rScale;
  out.body.Cmp = dir * cmU(1, 3) * pScale;
  out.body.Cmq = cmU(1, 4) * qScale;
  out.body.Cmr = dir * cmU(1, 5) * rScale;
  out.body.Cnp = cmU(2, 3) * pScale;
  out.body.Cnq = dir * cmU(2, 4) * qScale;
  out.body.Cnr = cmU(2, 5) * rScale;
  out.body.CXd1 = dir * cftD(0, 0);
  out.body.CYd1 = cftD(1, 0);
  out.body.CZd1 = dir * cftD(2, 0);
  out.body.Cld1 = dir * cmD(0, 0);
  out.body.Cmd1 = cmD(1, 0);
  out.body.Cnd1 = dir * cmD(2, 0);

  return out;
}

function tolerance(reference) {
  const mag = Math.abs(reference);
  if (mag < 1e-8) return 2e-6;
  return Math.max(2e-6, mag * 4e-3);
}

function compareSection(actual, expected, section) {
  const mismatches = [];
  for (const [label, ref] of Object.entries(expected)) {
    const got = Number(actual[label]);
    const tol = tolerance(ref);
    const err = Math.abs(got - ref);
    if (!Number.isFinite(got) || err > tol) {
      mismatches.push(`${section}.${label}: got=${got} ref=${ref} err=${err} tol=${tol}`);
    }
  }
  return mismatches;
}

for (const refPath of fixtures) {
  test(`full derivative matrix matches Fortran reference (${path.basename(refPath)})`, { timeout: 180000 }, async () => {
    const refs = JSON.parse(fs.readFileSync(refPath, 'utf8'));
    const caseName = refs?.meta?.case;
    assert.ok(caseName, `Missing meta.case in ${refPath}`);
    const casePath = path.join(runsDir, caseName);
    assert.ok(fs.existsSync(casePath), `Missing AVL case file: ${casePath}`);

    await preloadAoperLinSolveWasm();
    await preloadAsetupWasm();
    await preloadAeroWasm();
    await preloadAicWasmBridge();
    await preloadAsetpLuWasmBridge();

    const model = await buildSolverModel(fs.readFileSync(casePath, 'utf8'), { baseDir: runsDir });
    const state = buildExecState(model, {
      alpha: 1.0,
      beta: 0.0,
      cl: 0.0,
      vel: 1.0,
      rho: 1.0,
      gee: 1.0,
      mass: 1.0,
      bank: 0.0,
      cd0: 0.0,
    });

    const IR = 1;
    state.PARVAL[idx2(state.IPMACH, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(state.IPVEE, IR, state.IPTOT)] = 1.0;
    state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)] = 1.0;
    state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)] = 1.0;
    state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] = 1.0;
    state.PARVAL[idx2(state.IPPHI, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(state.IPCD0, IR, state.IPTOT)] = 0.0;

    state.ICON[idx2(state.IVALFA, IR, state.IVMAX)] = state.ICALFA;
    state.ICON[idx2(state.IVBETA, IR, state.IVMAX)] = state.ICBETA;
    state.ICON[idx2(state.IVROTX, IR, state.IVMAX)] = state.ICROTX;
    state.ICON[idx2(state.IVROTY, IR, state.IVMAX)] = state.ICROTY;
    state.ICON[idx2(state.IVROTZ, IR, state.IVMAX)] = state.ICROTZ;
    state.CONVAL[idx2(state.ICALFA, IR, state.ICMAX)] = 1.0;
    state.CONVAL[idx2(state.ICBETA, IR, state.ICMAX)] = 0.0;
    state.CONVAL[idx2(state.ICROTX, IR, state.ICMAX)] = 0.0;
    state.CONVAL[idx2(state.ICROTY, IR, state.ICMAX)] = 0.0;
    state.CONVAL[idx2(state.ICROTZ, IR, state.ICMAX)] = 0.0;
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      state.ICON[idx2(state.IVTOT + n, IR, state.IVMAX)] = state.ICTOT + n;
      state.CONVAL[idx2(state.ICTOT + n, IR, state.ICMAX)] = 0.0;
    }

    state.USE_WASM_SOLVE = true;
    state.USE_WASM_GAM = true;
    state.USE_WASM_AERO = true;
    state.USE_WASM_AIC = false;
    state.USE_WASM_LU = true;

    buildGeometry(state, model);
    EXEC(state, 20, 0, 1);

    const actual = computeDerivativesFortranStyle(state);
    const mismatches = [
      ...compareSection(actual.stability, refs.stability, 'stability'),
      ...compareSection(actual.body, refs.body, 'body'),
    ];
    assert.equal(
      mismatches.length,
      0,
      `Derivative parity mismatches (${mismatches.length}) for ${caseName}:\n${mismatches.slice(0, 20).join('\n')}`,
    );
  });
}

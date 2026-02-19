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
const hersheyPath = path.join(runsDir, 'hershey.avl');

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

test('hershey body-axis derivatives keep Fortran signs (mass/rho/g/v=1, alpha=1)', { timeout: 180000 }, async () => {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const model = await buildSolverModel(fs.readFileSync(hersheyPath, 'utf8'), { baseDir: runsDir });
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

  const dir = state.LNASA_SA ? -1.0 : 1.0;
  const bref = Number(state.BREF);
  const cref = Number(state.CREF);
  const cft = (row, col) => Number(state.CFTOT_U[idx2(row, col, 3)] ?? 0.0);
  const cmt = (row, col) => Number(state.CMTOT_U[idx2(row, col, 3)] ?? 0.0);
  const cfd = (row) => Number(state.CFTOT_D[idx2(row, 0, 3)] ?? 0.0);

  const cxq = dir * cft(0, 4) * (2.0 / cref);
  const czq = dir * cft(2, 4) * (2.0 / cref);
  const clr = cmt(0, 5) * (2.0 / bref);
  const cnp = cmt(2, 3) * (2.0 / bref);
  const cxd1 = dir * cfd(0);
  const czd1 = dir * cfd(2);

  assert.ok(cxq > 0.05, `CXq sign/magnitude mismatch: ${cxq}`);
  assert.ok(czq < -5.0, `CZq sign/magnitude mismatch: ${czq}`);
  assert.ok(clr > 0.008, `Clr sign/magnitude mismatch: ${clr}`);
  assert.ok(cnp < -0.01, `Cnp sign/magnitude mismatch: ${cnp}`);
  assert.ok(cxd1 > 0.0, `CXd1 sign mismatch: ${cxd1}`);
  assert.ok(czd1 < -0.02, `CZd1 sign/magnitude mismatch: ${czd1}`);
});

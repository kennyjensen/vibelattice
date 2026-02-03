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

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function applyB737Constraints(state) {
  const IR = 1;
  const { IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ } = state;
  const { ICCL, ICBETA, ICROTX, ICROTY, ICROTZ } = state;

  state.CONVAL[idx2(ICCL, IR, state.ICMAX)] = 0.6;
  state.CONVAL[idx2(ICBETA, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTX, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTY, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTZ, IR, state.ICMAX)] = 0.0;

  state.ICON[idx2(IVALFA, IR, state.IVMAX)] = ICCL;
  state.ICON[idx2(IVBETA, IR, state.IVMAX)] = ICBETA;
  state.ICON[idx2(IVROTX, IR, state.IVMAX)] = ICROTX;
  state.ICON[idx2(IVROTY, IR, state.IVMAX)] = ICROTY;
  state.ICON[idx2(IVROTZ, IR, state.IVMAX)] = ICROTZ;

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    state.CONVAL[idx2(state.ICTOT + n, IR, state.ICMAX)] = 0.0;
    state.ICON[idx2(state.IVTOT + n, IR, state.IVMAX)] = state.ICTOT + n;
  }
}

async function buildB737State() {
  const repoRoot = repoRootDir();
  const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
  const b737Path = path.join(runsDir, 'b737.avl');
  const model = await buildSolverModel(fs.readFileSync(b737Path, 'utf8'), { baseDir: runsDir });
  const state = buildExecState(model, {
    alpha: 2.0,
    beta: 0.0,
    cl: 0.6,
    vel: 16.34,
    rho: 1.225,
    gee: 9.81,
    bank: 0.0,
    cd0: 0.0,
    xcg: 60.0,
    ycg: 0.0,
    zcg: 0.0,
  });
  applyB737Constraints(state);
  buildGeometry(state, model);
  return state;
}

function assertClose(actual, expected, tol, label) {
  const diff = Math.abs(actual - expected);
  assert.ok(diff <= tol, `${label} diff ${diff} > ${tol}`);
}

function assertCloseArray(actual, expected, tol, label) {
  assert.equal(actual.length, expected.length, `${label} length mismatch`);
  for (let i = 0; i < actual.length; i += 1) {
    assertClose(actual[i], expected[i], tol, `${label}[${i}]`);
  }
}

test('EXEC wasm kernels match EXEC JS for b737 outputs', { timeout: 180000 }, async () => {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const stateJs = await buildB737State();
  EXEC(stateJs, 20, 0, 1);

  const stateWasm = await buildB737State();
  stateWasm.USE_WASM_SOLVE = true;
  stateWasm.USE_WASM_GAM = true;
  stateWasm.USE_WASM_AERO = true;
  stateWasm.USE_WASM_AIC = true;
  stateWasm.USE_WASM_LU = true;
  EXEC(stateWasm, 20, 0, 1);

  const tolForce = 1e-5;
  const tolStrip = 1e-4;
  assertClose(stateWasm.CLTOT, stateJs.CLTOT, tolForce, 'CLTOT');
  assertClose(stateWasm.CDTOT, stateJs.CDTOT, tolForce, 'CDTOT');
  assertClose(stateWasm.CYTOT, stateJs.CYTOT, tolForce, 'CYTOT');
  assertCloseArray(stateWasm.CMTOT, stateJs.CMTOT, tolForce, 'CMTOT');
  assertCloseArray(stateWasm.CFTOT, stateJs.CFTOT, tolForce, 'CFTOT');
  assertClose(stateWasm.CDVTOT, stateJs.CDVTOT, tolForce, 'CDVTOT');

  assert.equal(stateWasm.NSTRIP, stateJs.NSTRIP, 'NSTRIP mismatch');

  for (let j = 1; j <= stateJs.NSTRIP; j += 1) {
    assertClose(stateWasm.RLE[idx2(2, j, 4)], stateJs.RLE[idx2(2, j, 4)], tolStrip, `strip ${j} y`);
    assertClose(stateWasm.RLE[idx2(3, j, 4)], stateJs.RLE[idx2(3, j, 4)], tolStrip, `strip ${j} z`);
    assertClose(stateWasm.CNC[j], stateJs.CNC[j], tolStrip, `strip ${j} cnc`);
    assertClose(stateWasm.CLA_LSTRP[j], stateJs.CLA_LSTRP[j], tolStrip, `strip ${j} cla`);
    assertClose(stateWasm.CLT_LSTRP[j], stateJs.CLT_LSTRP[j], tolStrip, `strip ${j} clt`);
    assertClose(stateWasm.DWWAKE[j], stateJs.DWWAKE[j], tolStrip, `strip ${j} dwwake`);
  }
});

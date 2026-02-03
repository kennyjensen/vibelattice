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
  const idx2i = (i, j, dim1) => i + dim1 * j;
  const { IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ } = state;
  const { ICCL, ICBETA, ICROTX, ICROTY, ICROTZ } = state;

  state.CONVAL[idx2i(ICCL, IR, state.ICMAX)] = 0.6;
  state.CONVAL[idx2i(ICBETA, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2i(ICROTX, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2i(ICROTY, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2i(ICROTZ, IR, state.ICMAX)] = 0.0;

  state.ICON[idx2i(IVALFA, IR, state.IVMAX)] = ICCL;
  state.ICON[idx2i(IVBETA, IR, state.IVMAX)] = ICBETA;
  state.ICON[idx2i(IVROTX, IR, state.IVMAX)] = ICROTX;
  state.ICON[idx2i(IVROTY, IR, state.IVMAX)] = ICROTY;
  state.ICON[idx2i(IVROTZ, IR, state.IVMAX)] = ICROTZ;

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    state.CONVAL[idx2i(state.ICTOT + n, IR, state.ICMAX)] = 0.0;
    state.ICON[idx2i(state.IVTOT + n, IR, state.IVMAX)] = state.ICTOT + n;
  }
}

async function runExecWithFlags({ useAic, useAero }) {
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
  state.USE_WASM_SOLVE = true;
  state.USE_WASM_GAM = true;
  state.USE_WASM_AERO = useAero;
  state.USE_WASM_AIC = useAic;
  state.USE_WASM_LU = true;

  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);
  return state;
}

function assertTrefftzFinite(state, label) {
  for (let j = 1; j <= state.NSTRIP; j += 1) {
    assert.ok(Number.isFinite(state.CNC[j]), `${label} CNC[${j}]`);
    assert.ok(Number.isFinite(state.CLA_LSTRP[j]), `${label} CLA_LSTRP[${j}]`);
    assert.ok(Number.isFinite(state.CLT_LSTRP[j]), `${label} CLT_LSTRP[${j}]`);
    assert.ok(Number.isFinite(state.DWWAKE[j]), `${label} DWWAKE[${j}]`);
  }
  const y = state.RLE[idx2(2, 1, 4)];
  assert.ok(Number.isFinite(y), `${label} RLE y finite`);
}

test('EXEC with full WASM kernels produces finite trefftz outputs for b737', { timeout: 180000 }, async () => {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const stateAicOnly = await runExecWithFlags({ useAic: true, useAero: false });
  assertTrefftzFinite(stateAicOnly, 'AIC-only');

  const stateAeroOnly = await runExecWithFlags({ useAic: false, useAero: true });
  assertTrefftzFinite(stateAeroOnly, 'AERO-only');

  const stateFull = await runExecWithFlags({ useAic: true, useAero: true });
  assertTrefftzFinite(stateFull, 'AIC+AERO');
});

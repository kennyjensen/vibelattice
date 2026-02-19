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

// Fortran reference values from `third_party/avl/ref/b737_exec_ref hershey.avl`
// with the b737-style run settings used in this test.
const REF_CLQ = -457.5464261412494;
const REF_CLD1 = 0.03960742272004237;

function assertClose(actual, expected, tol, label) {
  const diff = Math.abs(actual - expected);
  assert.ok(diff <= tol, `${label} diff ${diff} > ${tol}; actual=${actual} expected=${expected}`);
}

async function preloadWasm() {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();
}

function applyB737StyleConstraints(state) {
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

test('EXEC wasm-aero derivatives match Fortran for hershey.avl (CLq, CLd1)', { timeout: 180000 }, async () => {
  await preloadWasm();

  const model = await buildSolverModel(fs.readFileSync(hersheyPath, 'utf8'), { baseDir: runsDir });
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
  const IR = 1;
  const idx2i = (i, j, dim1) => i + dim1 * j;
  state.MACH = 0.78;
  state.AMACH = 0.78;
  state.PARVAL[idx2i(state.IPMACH, IR, state.IPTOT)] = 0.78;
  applyB737StyleConstraints(state);
  state.USE_WASM_SOLVE = true;
  state.USE_WASM_GAM = true;
  state.USE_WASM_AERO = true;
  state.USE_WASM_AIC = false;
  state.USE_WASM_LU = true;

  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);

  const clq = state.CLTOT_U[4];
  const cld1 = state.CLTOT_D[0];

  assertClose(clq, REF_CLQ, 5e-3, 'CLq');
  assertClose(cld1, REF_CLD1, 5e-3, 'CLd1');
});

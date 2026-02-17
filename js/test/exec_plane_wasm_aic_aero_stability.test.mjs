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

const planeParams = {
  alpha: -0.1455,
  beta: 0.0,
  cl: 0.390510,
  vel: 64.5396,
  rho: 0.0005846,
  gee: 32.18,
  bank: 0.0,
  cd0: 0.00835,
  xcg: 0.02463,
  ycg: 0.0,
  zcg: 0.2239,
  cmx: 0.0,
  cmy: 0.0,
  cmz: 0.0,
};

function runExec(model, flags, niter = 20) {
  const state = buildExecState(model, planeParams);
  state.USE_WASM_SOLVE = Boolean(flags.solve);
  state.USE_WASM_GAM = Boolean(flags.gam);
  state.USE_WASM_AERO = Boolean(flags.aero);
  state.USE_WASM_AIC = Boolean(flags.aic);
  state.USE_WASM_LU = Boolean(flags.lu);
  buildGeometry(state, model);
  EXEC(state, niter, 0, 1);
  return state;
}

test('plane.avl: repeated WASM AIC+AERO runs stay finite and match JS outputs', { timeout: 120000 }, async () => {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const repoRoot = repoRootDir();
  const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
  const planePath = path.join(runsDir, 'plane.avl');
  const text = fs.readFileSync(planePath, 'utf8');

  const jsModel = await buildSolverModel(text, {});
  const jsState = runExec(jsModel, {
    solve: false,
    gam: false,
    aero: false,
    aic: false,
    lu: false,
  });

  const wasmFlags = {
    solve: true,
    gam: true,
    aero: true,
    aic: true,
    lu: true,
  };
  const tol = 1e-5;
  for (let i = 0; i < 3; i += 1) {
    const model = await buildSolverModel(text, {});
    const wasmState = runExec(model, wasmFlags);
    assert.equal(wasmState.USE_WASM_AERO, true, `run ${i}: wasm aero unexpectedly fell back`);
    assert.ok(Number.isFinite(wasmState.CLTOT), `run ${i}: CLTOT non-finite`);
    assert.ok(Number.isFinite(wasmState.CDTOT), `run ${i}: CDTOT non-finite`);
    assert.ok(Number.isFinite(wasmState.CDVTOT), `run ${i}: CDVTOT non-finite`);
    assert.ok(Math.abs(wasmState.CLTOT - jsState.CLTOT) <= tol, `run ${i}: CLTOT mismatch`);
    assert.ok(Math.abs(wasmState.CDTOT - jsState.CDTOT) <= tol, `run ${i}: CDTOT mismatch`);
    assert.ok(Math.abs(wasmState.CDVTOT - jsState.CDVTOT) <= tol, `run ${i}: CDVTOT mismatch`);
  }
});

test('plane.avl: WASM AIC+AERO matches JS for fixed-alpha (NITER=0) case', { timeout: 120000 }, async () => {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const repoRoot = repoRootDir();
  const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
  const planePath = path.join(runsDir, 'plane.avl');
  const text = fs.readFileSync(planePath, 'utf8');

  const jsState = runExec(await buildSolverModel(text, {}), {
    solve: false,
    gam: false,
    aero: false,
    aic: false,
    lu: false,
  }, 0);
  const wasmState = runExec(await buildSolverModel(text, {}), {
    solve: true,
    gam: true,
    aero: true,
    aic: true,
    lu: true,
  }, 0);

  assert.equal(wasmState.USE_WASM_AERO, true, 'wasm aero unexpectedly fell back');
  const tol = 1e-5;
  assert.ok(Math.abs(wasmState.CLTOT - jsState.CLTOT) <= tol, 'CLTOT mismatch (NITER=0)');
  assert.ok(Math.abs(wasmState.CDTOT - jsState.CDTOT) <= tol, 'CDTOT mismatch (NITER=0)');
  assert.ok(Math.abs(wasmState.CYTOT - jsState.CYTOT) <= tol, 'CYTOT mismatch (NITER=0)');
  assert.ok(Math.abs(wasmState.CDVTOT - jsState.CDVTOT) <= tol, 'CDVTOT mismatch (NITER=0)');
});

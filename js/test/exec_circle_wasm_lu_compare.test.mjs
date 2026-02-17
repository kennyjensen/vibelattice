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
import { EXEC, preloadAsetpLuWasmBridge } from '../src/aoper.js';

function runCircleState(model, useWasmLu, niter = 0) {
  const state = buildExecState(model, {
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
  });
  state.USE_WASM_LU = Boolean(useWasmLu);
  buildGeometry(state, model);
  EXEC(state, niter, 0, 1);
  return state;
}

test('circle.avl: WASM LU path matches JS path (non-iterative + iterative)', async () => {
  const repoRoot = repoRootDir();
  const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
  const circlePath = path.join(runsDir, 'circle.avl');
  const model = await buildSolverModel(fs.readFileSync(circlePath, 'utf8'), {});

  await preloadAsetpLuWasmBridge();
  const tol = 1e-6;
  for (const niter of [0, 20]) {
    const jsState = runCircleState(model, false, niter);
    const wasmLuState = runCircleState(model, true, niter);

    assert.equal(jsState.NSTRIP, wasmLuState.NSTRIP, `NSTRIP mismatch (NITER=${niter})`);
    assert.ok(Math.abs(jsState.CLTOT - wasmLuState.CLTOT) <= tol, `CLTOT mismatch (NITER=${niter})`);
    assert.ok(Math.abs(jsState.CDTOT - wasmLuState.CDTOT) <= tol, `CDTOT mismatch (NITER=${niter})`);
    assert.ok(Math.abs(jsState.CDVTOT - wasmLuState.CDVTOT) <= tol, `CDVTOT mismatch (NITER=${niter})`);
  }
});

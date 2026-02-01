import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { repoRootDir, buildSolverModel, buildExecState, buildGeometry } from '../src/exec_pipeline.js';
import { EXEC } from '../src/aoper.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

test('EXEC tolerates unconstrained control variables without NaNs', async () => {
  const repoRoot = repoRootDir();
  const modelPath = path.join(repoRoot, 'third_party', 'avl', 'runs', 'b737.avl');
  const text = await fs.readFile(modelPath, 'utf8');
  const model = await buildSolverModel(text, {});
  const state = buildExecState(model, {
    alpha: 0.0,
    beta: 0.0,
    cl: 0.6,
    vel: 16.34,
    rho: 1.225,
    gee: 9.81,
    bank: 0.0,
    cd0: 0.0,
  });
  buildGeometry(state, model);

  const IR = 1;
  state.ICON[idx2(state.IVALFA, IR, state.IVMAX)] = state.ICCL;
  state.ICON[idx2(state.IVBETA, IR, state.IVMAX)] = state.ICBETA;
  state.ICON[idx2(state.IVROTX, IR, state.IVMAX)] = state.ICMOMX;
  state.ICON[idx2(state.IVROTY, IR, state.IVMAX)] = state.ICMOMY;
  state.ICON[idx2(state.IVROTZ, IR, state.IVMAX)] = state.ICMOMZ;

  state.CONVAL[idx2(state.ICCL, IR, state.ICMAX)] = 0.6;
  state.CONVAL[idx2(state.ICBETA, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICMOMX, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICMOMY, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICMOMZ, IR, state.ICMAX)] = 0.0;

  // Mimic app buildExecState: include control variables in NVTOT.
  state.NVTOT = state.IVTOT + state.NCONTROL;
  // Explicitly clear one control variable constraint to simulate a "none" constraint.
  if (state.NCONTROL > 0) {
    state.ICON[idx2(state.IVTOT + 1, IR, state.IVMAX)] = 0;
  }

  EXEC(state, 8, 0, IR);
  assert.ok(Number.isFinite(state.ALFA), 'ALFA should be finite');
  assert.ok(Number.isFinite(state.BETA), 'BETA should be finite');
  assert.ok(state.VINF.every((v) => Number.isFinite(v)), 'VINF should be finite');
  assert.ok(state.WROT.every((v) => Number.isFinite(v)), 'WROT should be finite');
});

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { repoRootDir, buildSolverModel, buildExecState, buildGeometry } from '../src/exec_pipeline.js';
import { TRMSET_CORE } from '../src/atrim.js';
import { EXEC } from '../src/aoper.js';

const repoRoot = repoRootDir();
const planePath = path.join(repoRoot, 'third_party', 'avl', 'runs', 'plane.avl');

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

test('trim smoke: plane.avl runs TRMSET_CORE and EXEC', async () => {
  const text = await fs.readFile(planePath, 'utf8');
  const model = await buildSolverModel(text, {});
  const state = buildExecState(model, {
    alpha: 0.0,
    beta: 0.0,
    cl: 0.6,
    vel: 30.0,
    rho: 1.225,
    gee: 9.81,
    bank: 0.0,
    cd0: 0.0,
  });
  buildGeometry(state, model);

  const IR = 1;
  // Default AVL trim mapping.
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

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    state.ICON[idx2(state.IVTOT + n, IR, state.IVMAX)] = state.ICTOT + n;
    state.CONVAL[idx2(state.ICTOT + n, IR, state.ICMAX)] = 0.0;
  }

  assert.doesNotThrow(() => TRMSET_CORE(state, 1, IR, IR, IR));
  assert.doesNotThrow(() => EXEC(state, 20, 0, IR));
});

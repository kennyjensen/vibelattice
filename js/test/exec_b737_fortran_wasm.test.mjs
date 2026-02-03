import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import {
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
} from '../src/exec_pipeline.js';
import { EXEC, preloadAoperLinSolveWasm, preloadAsetupWasm, preloadAeroWasm, preloadAicWasmBridge, preloadAsetpLuWasmBridge } from '../src/aoper.js';
import { ensureRefBuilt } from './ref_build_lock.mjs';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const b737Path = path.join(runsDir, 'b737.avl');
const refBin = path.join(refDir, 'b737_exec_ref');

function parseRefOutput(stdout) {
  const lines = stdout.trim().split(/\r?\n/);
  const numRe = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g;
  const out = { strips: [] };
  for (const line of lines) {
    if (!line.trim()) continue;
    const parts = line.trim().split(/\s+/);
    if (parts[0] === 'FORCE') {
      const nums = (line.match(numRe) || []).map((v) => Number(v.replace(/d/i, 'e')));
      out.force = {
        CLTOT: nums[0],
        CDTOT: nums[1],
        CYTOT: nums[2],
        CMTOT: [nums[3], nums[4], nums[5]],
        CFTOT: [nums[6], nums[7], nums[8]],
      };
    } else if (parts[0] === 'CDVTOT') {
      const nums = (line.match(numRe) || []).map((v) => Number(v.replace(/d/i, 'e')));
      out.CDVTOT = nums[0];
    } else if (parts[0] === 'NSTRIP') {
      const nums = (line.match(numRe) || []).map((v) => Number(v.replace(/d/i, 'e')));
      out.NSTRIP = nums[0];
    } else if (parts[0] === 'STRIP') {
      const nums = (line.match(numRe) || []).map((v) => Number(v.replace(/d/i, 'e')));
      const j = nums[0];
      out.strips[j] = {
        y: nums[1],
        z: nums[2],
        cnc: nums[3],
        cla: nums[4],
        clt: nums[5],
        dwwake: nums[6],
        off: nums[7] ? 1 : 0,
      };
    }
  }
  return out;
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

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

async function preloadWasm() {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();
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

test('EXEC wasm path matches Fortran for b737.avl', { timeout: 180000 }, async () => {
  await ensureRefBuilt('b737_exec_ref', refDir);
  await preloadWasm();

  const refProc = spawnSync(refBin, ['b737.avl'], { encoding: 'utf8', cwd: runsDir });
  if (refProc.error) throw refProc.error;
  if (refProc.status !== 0) {
    throw new Error(refProc.stderr || refProc.stdout || `ref exited with ${refProc.status}`);
  }
  const ref = parseRefOutput(refProc.stdout);

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
  state.USE_WASM_AERO = false;
  state.USE_WASM_AIC = false;
  state.USE_WASM_LU = true;

  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);

  const tolForce = 5e-3;
  const tolStrip = 5e-2;
  assertClose(state.CLTOT, ref.force.CLTOT, tolForce, 'CLTOT');
  assertClose(state.CDTOT, ref.force.CDTOT, tolForce, 'CDTOT');
  assertClose(state.CYTOT, ref.force.CYTOT, tolForce, 'CYTOT');
  assertCloseArray(state.CMTOT, ref.force.CMTOT, tolForce, 'CMTOT');
  assertCloseArray(state.CFTOT, ref.force.CFTOT, tolForce, 'CFTOT');
  assertClose(state.CDVTOT, ref.CDVTOT, tolForce, 'CDVTOT');

  assert.equal(state.NSTRIP, ref.NSTRIP, 'NSTRIP mismatch');

  for (let j = 1; j <= state.NSTRIP; j += 1) {
    const r = ref.strips[j];
    assert.ok(r, `missing ref strip ${j}`);
    const y = state.RLE[idx2(2, j, 4)];
    const z = state.RLE[idx2(3, j, 4)];
    assertClose(y, r.y, tolStrip, `strip ${j} y`);
    assertClose(z, r.z, tolStrip, `strip ${j} z`);

    if (r.off) {
      assertClose(state.CNC[j], 0.0, tolStrip, `strip ${j} cnc off`);
      assertClose(state.CLA_LSTRP[j], 0.0, tolStrip, `strip ${j} cla off`);
      assertClose(state.CLT_LSTRP[j], 0.0, tolStrip, `strip ${j} clt off`);
      assertClose(state.DWWAKE[j], 0.0, tolStrip, `strip ${j} dwwake off`);
    } else {
      assertClose(state.CNC[j], r.cnc, tolStrip, `strip ${j} cnc`);
      assertClose(state.CLA_LSTRP[j], r.cla, tolStrip, `strip ${j} cla`);
      assertClose(state.CLT_LSTRP[j], r.clt, tolStrip, `strip ${j} clt`);
      assertClose(state.DWWAKE[j], r.dwwake, tolStrip, `strip ${j} dwwake`);
    }
  }
});

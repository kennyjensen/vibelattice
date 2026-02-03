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
import { EXEC } from '../src/aoper.js';
import { ensureRefBuilt } from './ref_build_lock.mjs';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const planePath = path.join(runsDir, 'plane.avl');
const refBin = path.join(refDir, 'plane_exec_ref');

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

test('EXEC matches Fortran for plane.avl forces and trefftz', { timeout: 80000 }, async () => {
  await ensureRefBuilt('plane_exec_ref', refDir);

  const refProc = spawnSync(refBin, [planePath], { encoding: 'utf8' });
  if (refProc.error) throw refProc.error;
  if (refProc.status !== 0) {
    throw new Error(refProc.stderr || refProc.stdout || `ref exited with ${refProc.status}`);
  }
  const ref = parseRefOutput(refProc.stdout);

  const model = await buildSolverModel(fs.readFileSync(planePath, 'utf8'), {});
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
  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);

  const tolForce = 2e-4;
  const tolStrip = 2e-3;
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

test('EXEC matches Fortran for plane.avl with iterations', { timeout: 120000 }, async () => {
  await ensureRefBuilt('plane_exec_ref', refDir);

  const refProc = spawnSync(refBin, [planePath, '20'], { encoding: 'utf8' });
  if (refProc.error) throw refProc.error;
  if (refProc.status !== 0) {
    throw new Error(refProc.stderr || refProc.stdout || `ref exited with ${refProc.status}`);
  }
  const ref = parseRefOutput(refProc.stdout);

  const model = await buildSolverModel(fs.readFileSync(planePath, 'utf8'), {});
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
  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);

  const tolForce = 5e-4;
  const tolStrip = 5e-3;
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

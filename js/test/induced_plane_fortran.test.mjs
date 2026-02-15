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
import {
  buildSolverHorseshoesFromExec,
  computeInducedVelocityFromHorseshoes,
} from '../src/flow_induced.js';
import { ensureRefBuilt } from './ref_build_lock.mjs';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const planePath = path.join(runsDir, 'plane.avl');
const vorvelcRefBin = path.join(refDir, 'vorvelc_ref');

function runVorvelcRef(cases) {
  const inputLines = [String(cases.length)];
  for (const c of cases) {
    inputLines.push([
      c.x, c.y, c.z, c.lbound ? 1 : 0,
      c.x1, c.y1, c.z1,
      c.x2, c.y2, c.z2,
      c.beta, c.rcore,
    ].join(' '));
  }
  const proc = spawnSync(vorvelcRefBin, {
    input: inputLines.join('\n'),
    encoding: 'utf8',
  });
  if (proc.error) throw proc.error;
  if (proc.status !== 0) {
    throw new Error(proc.stderr || proc.stdout || `vorvelc_ref exited with ${proc.status}`);
  }
  const nums = String(proc.stdout || '').trim().split(/\s+/).map(Number).filter(Number.isFinite);
  assert.equal(nums.length, cases.length * 3, 'vorvelc_ref output length mismatch');
  const out = new Array(cases.length);
  for (let i = 0; i < cases.length; i += 1) {
    out[i] = { u: nums[i * 3], v: nums[i * 3 + 1], w: nums[i * 3 + 2] };
  }
  return out;
}

function assertClose(actual, expected, tol, label) {
  const diff = Math.abs(actual - expected);
  assert.ok(diff <= tol, `${label}: diff ${diff} > ${tol}`);
}

test('induced velocity matches AVL Fortran plane reference points', { timeout: 120000 }, async () => {
  await ensureRefBuilt('vorvelc_ref', refDir);
  if (!fs.existsSync(vorvelcRefBin)) {
    assert.fail(`Fortran reference binary not found: ${vorvelcRefBin}`);
  }

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

  const solver = buildSolverHorseshoesFromExec(state);
  assert.ok(Array.isArray(solver.horseshoes) && solver.horseshoes.length > 0, 'solver horseshoes missing');

  const xref = Number(model?.header?.xref) || 0;
  const yref = Number(model?.header?.yref) || 0;
  const zref = Number(model?.header?.zref) || 0;
  const cref = Math.max(0.1, Math.abs(Number(model?.header?.cref) || 1));
  const bref = Math.max(0.2, Math.abs(Number(model?.header?.bref) || 2));
  const points = [
    { x: xref + 0.20 * cref, y: yref + 0.12 * bref, z: zref + 0.15 * cref },
    { x: xref + 0.50 * cref, y: yref - 0.20 * bref, z: zref + 0.20 * cref },
    { x: xref + 0.90 * cref, y: yref + 0.35 * bref, z: zref + 0.10 * cref },
    { x: xref + 1.30 * cref, y: yref - 0.40 * bref, z: zref + 0.05 * cref },
    { x: xref + 0.40 * cref, y: yref + 0.08 * bref, z: zref - 0.10 * cref },
    { x: xref + 1.10 * cref, y: yref - 0.10 * bref, z: zref + 0.35 * cref },
    { x: xref + 0.70 * cref, y: yref + 0.30 * bref, z: zref - 0.05 * cref },
    { x: xref + 1.40 * cref, y: yref - 0.25 * bref, z: zref + 0.22 * cref },
  ];

  const fysym = Number(solver.iysym) || 0;
  const fzsym = Number(solver.izsym) || 0;
  const yoff = 2 * (Number(solver.ysym) || 0);
  const zoff = 2 * (Number(solver.zsym) || 0);
  const beta = Math.abs(Number(solver.beta) || 0) > 1e-8 ? Number(solver.beta) : 1;

  const cases = [];
  const casePoint = [];
  const caseScale = [];
  const addCase = (pt, seg, scale) => {
    cases.push({
      x: pt.x, y: pt.y, z: pt.z,
      lbound: true,
      x1: seg.x1, y1: seg.y1, z1: seg.z1,
      x2: seg.x2, y2: seg.y2, z2: seg.z2,
      beta,
      rcore: Math.max(1e-7, Number(seg.core) || 0),
    });
    casePoint.push(pt.idx);
    caseScale.push(scale);
  };

  points.forEach((p, idx) => { p.idx = idx; });
  for (const p of points) {
    for (const h of solver.horseshoes) {
      const gamma = Number(h.gamma) || 0;
      if (!Number.isFinite(gamma) || Math.abs(gamma) < 1e-12) continue;
      addCase(p, h, gamma);
      if (fysym !== 0) {
        addCase(p, {
          x1: h.x2, y1: yoff - h.y2, z1: h.z2,
          x2: h.x1, y2: yoff - h.y1, z2: h.z1,
          core: h.core,
        }, gamma * fysym);
      }
      if (fzsym !== 0) {
        addCase(p, {
          x1: h.x2, y1: h.y2, z1: zoff - h.z2,
          x2: h.x1, y2: h.y1, z2: zoff - h.z1,
          core: h.core,
        }, gamma * fzsym);
        if (fysym !== 0) {
          addCase(p, {
            x1: h.x1, y1: yoff - h.y1, z1: zoff - h.z1,
            x2: h.x2, y2: yoff - h.y2, z2: zoff - h.z2,
            core: h.core,
          }, gamma * fysym * fzsym);
        }
      }
    }
  }

  const fortranCases = runVorvelcRef(cases);
  const refSum = points.map(() => [0, 0, 0]);
  for (let i = 0; i < cases.length; i += 1) {
    const pidx = casePoint[i];
    const scale = caseScale[i];
    refSum[pidx][0] += fortranCases[i].u * scale;
    refSum[pidx][1] += fortranCases[i].v * scale;
    refSum[pidx][2] += fortranCases[i].w * scale;
  }

  const tol = 5e-5;
  const acc = [0, 0, 0];
  for (const p of points) {
    computeInducedVelocityFromHorseshoes(p.x, p.y, p.z, solver.horseshoes, solver, acc);
    assertClose(acc[0], refSum[p.idx][0], tol, `u@${p.idx}`);
    assertClose(acc[1], refSum[p.idx][1], tol, `v@${p.idx}`);
    assertClose(acc[2], refSum[p.idx][2], tol, `w@${p.idx}`);
  }
});

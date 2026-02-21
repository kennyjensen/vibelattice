import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { Worker } from 'node:worker_threads';
import {
  parseAVL,
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
  applyZSymmetry,
  applyYSymmetry,
  applyYDuplicate,
} from '../src/exec_pipeline.js';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const b737Path = path.join(runsDir, 'b737.avl');

async function readRunFile(name) {
  return fs.readFile(path.join(runsDir, name), 'utf8');
}

test('parseAVL loads b737 surfaces', async () => {
  const text = await readRunFile('b737.avl');
  const model = parseAVL(text);
  assert.ok(model.surfaces.length > 0, 'expected surfaces');
  const wing = model.surfaces.find((s) => /wing/i.test(s.name));
  assert.ok(wing, 'expected wing surface');
  assert.ok(wing.sections.length > 0, 'expected wing sections');
});

test('parseAVL ignores inline ! comments on numeric lines (circle.avl spacing)', async () => {
  const text = await readRunFile('circle.avl');
  const model = parseAVL(text);
  const wing = model.surfaces[0];
  assert.ok(wing, 'expected first surface');
  assert.equal(wing.nSpan, 0, 'inline ! comment should not contribute numeric tokens');
  assert.equal(wing.sections.length, 13, 'expected all circle sections');
});

test('parseAVL ignores inline # comments on numeric lines', () => {
  const text = [
    'Inline Hash Test',
    '0.0',
    '0 0 0',
    '1 1 1',
    '0 0 0',
    'SURFACE',
    'Wing',
    '2 1.0 # 9 0.0',
    'SECTION',
    '0 0 0 1 0 # keep only section fields',
    'SECTION',
    '0 1 0 1 0',
  ].join('\n');
  const model = parseAVL(text);
  const wing = model.surfaces[0];
  assert.ok(wing, 'expected surface');
  assert.equal(wing.nChord, 2);
  assert.equal(wing.sSpace, 1);
  assert.equal(wing.nSpan, 0, 'inline # comment should not contribute numeric tokens');
  assert.equal(wing.sections.length, 2);
  assert.equal(wing.sections[0].ainc, 0);
});

test('applyZSymmetry mirrors sections about zsym plane', async () => {
  const text = [
    'Z Sym Test',
    '0.0',
    '0 1 1',
    '1 1 1',
    '0 0 1.5',
    'SURFACE',
    'Wing',
    '2 1.0 2 1.0',
    'SECTION',
    '0 0 2 1 0',
    'NACA',
    '0012',
    'SECTION',
    '0 1 2 1 0',
    'NACA',
    '0012',
  ].join('\n');
  const model = parseAVL(text);
  const withSym = applyZSymmetry(model);
  assert.equal(withSym.surfaces.length, 2, 'expected mirrored surface');
  const original = withSym.surfaces[0];
  const mirrored = withSym.surfaces[1];
  assert.equal(original.sections[0].zle, 2);
  assert.equal(mirrored.sections[0].zle, 0.0);
  assert.equal(mirrored.sections[1].zle, 0.0);
});

test('applyYSymmetry mirrors sections about xz plane', async () => {
  const text = [
    'Y Sym Test',
    '0.0',
    '1 0 0',
    '1 1 1',
    '0 0 0',
    'SURFACE',
    'Wing',
    '2 1.0 2 1.0',
    'SECTION',
    '0 2 0 1 0',
    'NACA',
    '0012',
    'SECTION',
    '0 4 0 1 0',
    'NACA',
    '0012',
  ].join('\n');
  const model = parseAVL(text);
  const withSym = applyYSymmetry(model);
  assert.equal(withSym.surfaces.length, 2, 'expected mirrored surface');
  const original = withSym.surfaces[0];
  const mirrored = withSym.surfaces[1];
  assert.equal(original.sections[0].yle, 2);
  assert.equal(mirrored.sections[0].yle, -2);
  assert.equal(mirrored.sections[1].yle, -4);
});

test('applyYDuplicate preserves symmetric tip heights with ainc', async () => {
  const text = [
    'Y Dup Test',
    '0.0',
    '0 0 0',
    '1 1 10',
    '0 0 0',
    'SURFACE',
    'Wing',
    '1 1.0 2 1.0',
    'YDUPLICATE',
    '0.0',
    'SECTION',
    '0 0 0 1 4.0 0 0',
    'NACA',
    '0012',
    'SECTION',
    '0 5 0.5 1 4.0 0 0',
    'NACA',
    '0012',
  ].join('\n');
  const model = applyYDuplicate(parseAVL(text));
  assert.equal(model.surfaces.length, 2, 'expected duplicated surface');
  const [a, b] = model.surfaces;
  const rotatePoint = (sec, p) => {
    const axX = 0;
    const axY = 1;
    const axZ = 0;
    const [x, y, z] = p;
    const px = x - sec.xle;
    const py = y - sec.yle;
    const pz = z - sec.zle;
    const ang = sec.ainc * Math.PI / 180;
    const c = Math.cos(ang);
    const s = Math.sin(ang);
    const dot = axX * px + axY * py + axZ * pz;
    const cx = axY * pz - axZ * py;
    const cy = axZ * px - axX * pz;
    const cz = axX * py - axY * px;
    const rx = px * c + cx * s + axX * dot * (1 - c);
    const ry = py * c + cy * s + axY * dot * (1 - c);
    const rz = pz * c + cz * s + axZ * dot * (1 - c);
    return [sec.xle + rx, sec.yle + ry, sec.zle + rz];
  };
  const tipA = a.sections[1];
  const tipB = b.sections[1];
  const teA = rotatePoint(tipA, [tipA.xle + tipA.chord, tipA.yle, tipA.zle]);
  const teB = rotatePoint(tipB, [tipB.xle + tipB.chord, tipB.yle, tipB.zle]);
  assert.ok(Math.abs(teA[2] - teB[2]) < 1e-6, 'tip heights should match');
});

test('buildSolverModel resolves airfoil files', async () => {
  const text = await readRunFile('b737.avl');
  const model = await buildSolverModel(text, { baseDir: runsDir });
  const section = model.surfaces[0]?.sections?.[0];
  assert.ok(section, 'expected a section');
  assert.ok(section.airfoilCamber, 'expected airfoil camber slope');
});

test('buildGeometry populates BODY node arrays for supra', async () => {
  const text = await readRunFile('supra.avl');
  const model = await buildSolverModel(text, { baseDir: runsDir });
  const state = buildExecState(model, { alpha: 0, beta: 0, vel: 1, rho: 1, gee: 1, unitl: 0.0254 });
  buildGeometry(state, model);

  assert.ok(state.NBODY >= 1, 'expected at least one body');
  assert.ok(state.NLNODE >= 2, 'expected body node count');
  assert.ok(state.LFRST[0] >= 1, 'expected first body node index');
  assert.ok(state.NL[0] >= 2, 'expected first body node length');
  assert.ok(Number.isFinite(state.RL[4 * state.LFRST[0] + 1]), 'expected RL x coord');
  assert.ok(Number.isFinite(state.RL[4 * state.LFRST[0] + 2]), 'expected RL y coord');
  assert.ok(Number.isFinite(state.RL[4 * state.LFRST[0] + 3]), 'expected RL z coord');
  assert.ok(Number.isFinite(state.RADL[state.LFRST[0]]), 'expected RADL radius');
});

function runExecInWorker(text, options, timeoutMs = 2000) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(new URL('./exec_worker.mjs', import.meta.url), {
      workerData: { text, options },
    });
    const timer = setTimeout(() => {
      worker.terminate();
      reject(new Error(`EXEC timed out after ${timeoutMs}ms`));
    }, timeoutMs);
    worker.on('message', (msg) => {
      clearTimeout(timer);
      worker.terminate();
      resolve(msg);
    });
    worker.on('error', (err) => {
      clearTimeout(timer);
      worker.terminate();
      reject(err);
    });
  });
}

test('EXEC pipeline runs on a small model', async () => {
  const text = [
    'Unit Test Wing',
    '0.0',
    '0 0 0',
    '1 1 1',
    '0 0 0',
    'SURFACE',
    'Wing',
    '2 1.0 2 1.0',
    'SECTION',
    '0 0 0 1 0',
    'NACA',
    '0012',
    'SECTION',
    '0 1 0 1 0',
    'NACA',
    '0012',
  ].join('\n');
  const result = await runExecInWorker(text, {
    vel: 30,
    cl: 0.4,
    alpha: 2.0,
    beta: 0.0,
  });
  assert.equal(result.ok, true, result.error || 'EXEC failed');
  assert.ok(Number.isFinite(result.CLTOT), 'CLTOT should be finite');
  assert.ok(result.NVOR > 0, 'expected NVOR > 0');
  assert.equal(result.cncNan, 0, 'expected CNC to be finite');
  assert.equal(result.dwNan, 0, 'expected DWWAKE to be finite');
  assert.equal(result.cltNan, 0, 'expected CLT_LSTRP to be finite');
});

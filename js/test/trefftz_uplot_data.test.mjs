import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { repoRootDir, buildSolverModel, buildExecState, buildGeometry } from '../src/exec_pipeline.js';
import { EXEC } from '../src/aoper.js';

function buildTrefftzFromState(state) {
  const idx2 = (i, j, dim1) => i + dim1 * j;
  const tref = { axis: 'Y', cref: state.CREF, strips: [], surfaces: [] };
  if (!state.DWWAKE || !state.RLE || !state.CNC || !state.CLA_LSTRP || !state.CLT_LSTRP) return tref;
  for (let n = 1; n <= state.NSURF; n += 1) {
    const j1 = state.JFRST[n];
    const nj = state.NJ[n];
    if (!j1 || !nj) continue;
    const start = tref.strips.length;
    for (let jj = 0; jj < nj; jj += 1) {
      const j = j1 + jj;
      const y = state.RLE[idx2(2, j, 4)];
      const z = state.RLE[idx2(3, j, 4)];
      const cnc = state.CNC[j];
      const cl = state.CLA_LSTRP[j];
      const clp = state.CLT_LSTRP[j];
      const dw = state.DWWAKE[j];
      tref.strips.push([y, z, cnc, cl, clp, dw, n]);
    }
    tref.surfaces.push({ id: n, start, count: nj });
  }
  return tref;
}

test('Trefftz data uses non-degenerate x range', async () => {
  const repoRoot = repoRootDir();
  const b737Path = path.join(repoRoot, 'third_party', 'avl', 'runs', 'b737.avl');
  const avlText = await fs.readFile(b737Path, 'utf8');

  const model = await buildSolverModel(avlText, { baseDir: path.dirname(b737Path) });
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
  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);

  const tref = buildTrefftzFromState(state);
  const x = tref.strips.map((row) => row[0]);
  const finiteX = x.filter((v) => Number.isFinite(v));
  assert.ok(finiteX.length > 2, 'x should contain finite values');
  const minX = Math.min(...finiteX);
  const maxX = Math.max(...finiteX);
  assert.ok(maxX - minX > 1e-3, 'x range should be non-zero');

  const clVals = tref.strips.map((row) => row[3]).filter((v) => Number.isFinite(v));
  const clpVals = tref.strips.map((row) => row[4]).filter((v) => Number.isFinite(v));
  const cncVals = tref.strips.map((row) => row[2]).filter((v) => Number.isFinite(v));
  const dwVals = tref.strips.map((row) => row[5]).filter((v) => Number.isFinite(v));
  const ranges = [clVals, clpVals, cncVals, dwVals].map((vals) => {
    if (!vals.length) return 0;
    return Math.max(...vals) - Math.min(...vals);
  });
  assert.ok(ranges.some((r) => r > 1e-4), 'at least one series should have non-zero range');
});

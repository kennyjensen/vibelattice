import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { AVL_MAIN, avlSnapshot } from '../src/avl.js';
import { loadAvlWasm } from '../src/avl_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'avl_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  const matches = proc.stdout.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g) || [];
  const vals = matches.map((v) => Number(v.replace(/d/i, 'e')));
  const needed = 14;
  return vals.slice(Math.max(0, vals.length - needed));
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

test('avl.f JS startup snapshot matches Fortran reference', () => {
  const refBin = path.join(refDir, 'avl_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();

  const state = {};
  AVL_MAIN(state, {
    askc: () => ({ COMAND: 'Q   ', COMARG: ' ' }),
    getarg0: () => ' ',
  });

  const snap = avlSnapshot(state);
  assertCloseArray(snap, ref, 1e-4);
});

test('avl.f WASM startup snapshot matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'avl_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = runFortranRef();
  const { avlInitSnapshot } = await loadAvlWasm();
  const snapObj = avlInitSnapshot();
  const snap = [
    snapObj.VERSION,
    snapObj.PI,
    snapObj.DTR,
    snapObj.LUINP,
    snapObj.LURUN,
    snapObj.LUOUT,
    snapObj.LUSTD,
    snapObj.LUSYS,
    snapObj.RMASS0,
    snapObj.NDEFINI,
    snapObj.NMASINI,
    snapObj.NHEAP,
    snapObj.NPLINIT,
    snapObj.NPLCLOSE,
  ];
  assertCloseArray(snap, ref, 1e-4);
});

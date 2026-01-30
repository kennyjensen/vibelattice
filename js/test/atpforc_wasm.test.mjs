import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { loadAtpforcWasm } from '../src/atpforc_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef(binName) {
  const bin = path.join(refDir, binName);
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  return proc.stdout.trim().split(/\s+/).map(Number);
}

function assertCloseArray(actual, expected, tol = 1e-4) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function idx3(r, c) {
  return r + 3 * c;
}

test('TPFORC WASM matches Fortran reference', async (t) => {
  const refBin = path.join(refDir, 'atpforc_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }

  const ref = runFortranRef('atpforc_ref');
  let offset = 0;
  const refScalars = ref.slice(offset, offset + 4); offset += 4;
  const refDwwake = ref.slice(offset, offset + 2); offset += 2;
  const refClffU = ref.slice(offset, offset + 6); offset += 6;
  const refCyffU = ref.slice(offset, offset + 6); offset += 6;
  const refCdffU = ref.slice(offset, offset + 6); offset += 6;
  const refSpanU = ref.slice(offset, offset + 6); offset += 6;

  const NSTRIP = 2;
  const NUMAX = 6;

  const RV1 = new Float32Array(3 * 2);
  const RV2 = new Float32Array(3 * 2);
  const RC = new Float32Array(3 * 2);

  RV1[idx3(0, 0)] = 0.0; RV1[idx3(1, 0)] = 0.0; RV1[idx3(2, 0)] = 0.0;
  RV2[idx3(0, 0)] = 1.0; RV2[idx3(1, 0)] = 0.5; RV2[idx3(2, 0)] = 0.2;
  RV1[idx3(0, 1)] = 0.2; RV1[idx3(1, 1)] = -0.3; RV1[idx3(2, 1)] = 0.1;
  RV2[idx3(0, 1)] = 1.2; RV2[idx3(1, 1)] = 0.2; RV2[idx3(2, 1)] = -0.1;

  RC[idx3(0, 0)] = 0.5; RC[idx3(1, 0)] = 0.1; RC[idx3(2, 0)] = 0.05;
  RC[idx3(0, 1)] = 0.8; RC[idx3(1, 1)] = -0.1; RC[idx3(2, 1)] = 0.2;

  const state = {
    PI: Math.fround(3.14159265),
    AMACH: Math.fround(0.3),
    YSYM: Math.fround(0.2),
    ZSYM: Math.fround(-0.1),
    IYSYM: 0,
    IZSYM: 0,
    VRCOREC: Math.fround(0.01),
    VRCOREW: Math.fround(0.02),
    NSTRIP,
    NUMAX,
    SREF: Math.fround(1.5),
    BREF: Math.fround(2.0),
    IJFRST: Int32Array.from([0, 1]),
    NVSTRP: Int32Array.from([1, 1]),
    GAM: Float32Array.from([0.5, 0.3]),
    GAM_U: (() => {
      const out = new Float32Array(2 * NUMAX);
      for (let i = 0; i < 2; i += 1) {
        for (let n = 0; n < NUMAX; n += 1) {
          out[i * NUMAX + n] = Math.fround(0.05 * (i + 1 + n + 1));
        }
      }
      return out;
    })(),
    RV1,
    RV2,
    RC,
    CHORD: Float32Array.from([1.0, 0.8]),
    LSSURF: Int32Array.from([0, 0]),
    LNCOMP: Int32Array.from([1]),
    LFLOAD: Int32Array.from([1]),
  };

  const wasm = await loadAtpforcWasm();
  const out = wasm.TPFORC(state);
  const scalars = [out.CLFF, out.CYFF, out.CDFF, out.SPANEF];

  assertCloseArray(scalars, refScalars);
  assertCloseArray(Array.from(out.DWWAKE), refDwwake);
  assertCloseArray(Array.from(out.CLFF_U), refClffU);
  assertCloseArray(Array.from(out.CYFF_U), refCyffU);
  assertCloseArray(Array.from(out.CDFF_U), refCdffU, 1e-4);
  assertCloseArray(Array.from(out.SPANEF_U), refSpanU, 1e-3);
});

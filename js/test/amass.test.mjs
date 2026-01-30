import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { MASINI, UNITSET, APPGET, MASPUT, MASGET } from '../src/amass.js';
import { loadAmassWasm } from '../src/amass_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const wasmPath = path.join(repoRoot, 'js', 'dist', 'amass.wasm');

function runFortranRef() {
  const bin = path.join(refDir, 'amass_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  return proc.stdout.trim().split(/\s+/).map(Number);
}

function parseRef(values) {
  let idx = 0;
  const masini = {
    rmass0: values[idx++],
    xyzmass0: values.slice(idx, idx + 3),
    rinerDiag: values.slice(idx + 3, idx + 6),
    lmass: Math.trunc(values[idx + 6]),
  };
  idx += 7;
  const unitset = values.slice(idx, idx + 6); idx += 6;
  const amass = values.slice(idx, idx + 9); idx += 9;
  const ainer = values.slice(idx, idx + 9); idx += 9;
  const parval = values.slice(idx, idx + 12); idx += 12;
  const masget = {
    rmass0: values[idx++],
    xyzmass0: values.slice(idx, idx + 3),
    riner: values.slice(idx + 3, idx + 12),
    gee0: values[idx + 12],
    rho0: values[idx + 13],
    unitl: values[idx + 14],
    unitm: values[idx + 15],
    unitt: values[idx + 16],
    lmass: Math.trunc(values[idx + 17]),
  };
  return { masini, unitset, amass, ainer, parval, masget };
}

function assertCloseArray(actual, expected, tol = 1e-5) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function makeState() {
  return {
    RMASS0: 0.0,
    RINER0: new Float32Array(9),
    AMASS: new Float32Array(9),
    AINER: new Float32Array(9),
    XYZMASS0: new Float32Array(3),
    LMASS: false,
    UNITL: 1.0,
    UNITM: 1.0,
    UNITT: 1.0,
    UNCHL: 'Lunit',
    UNCHM: 'Munit',
    UNCHT: 'Tunit',
    NUL: 5,
    NUM: 5,
    NUT: 5,
    UNCHV: '',
    UNCHD: '',
    UNCHA: '',
    UNCHI: '',
    UNCHS: '',
    UNCHF: '',
    NUV: 0,
    NUD: 0,
    NUA: 0,
    NUI: 0,
    NUS: 0,
    NUF: 0,
    UNITF: 0.0,
    UNITS: 0.0,
    UNITV: 0.0,
    UNITA: 0.0,
    UNITI: 0.0,
    UNITD: 0.0,
    NSTRIP: 2,
    CHORD: new Float32Array(2),
    WSTRIP: new Float32Array(2),
    ENSY: new Float32Array(2),
    ENSZ: new Float32Array(2),
    RLE1: new Float32Array(6),
    RLE2: new Float32Array(6),
    CHORD1: new Float32Array(2),
    CHORD2: new Float32Array(2),
    RLE: new Float32Array(6),
    PARVAL: new Float32Array(31),
    IPALFA: 1,
    IPBETA: 2,
    IPPHI: 8,
    IPTHE: 9,
    IPPSI: 10,
    IPVEE: 12,
    IPRHO: 13,
    IPGEE: 14,
    IPRAD: 15,
    IPXCG: 17,
    IPYCG: 18,
    IPZCG: 19,
    IPMASS: 20,
    IPIXX: 21,
    IPIYY: 22,
    IPIZZ: 23,
    IPIXY: 24,
    IPIYZ: 25,
    IPIZX: 26,
    IPTOT: 30,
    PARUNCH: Array.from({ length: 31 }, () => ''),
    GEE0: 0.0,
    RHO0: 0.0,
  };
}

function fillAppget(state) {
  for (let j = 0; j < state.NSTRIP; j += 1) {
    const jj = j + 1;
    state.CHORD[j] = Math.fround(1.0 + 0.2 * jj);
    state.WSTRIP[j] = Math.fround(0.5 + 0.1 * jj);
    state.ENSY[j] = Math.fround(0.1 * jj);
    state.ENSZ[j] = Math.fround(0.2 * jj);
    state.RLE1[j * 3 + 0] = Math.fround(0.1 * jj);
    state.RLE1[j * 3 + 1] = Math.fround(0.2 * jj);
    state.RLE1[j * 3 + 2] = Math.fround(0.3 * jj);
    state.RLE2[j * 3 + 0] = Math.fround(0.4 * jj);
    state.RLE2[j * 3 + 1] = Math.fround(0.5 * jj);
    state.RLE2[j * 3 + 2] = Math.fround(0.6 * jj);
    state.CHORD1[j] = Math.fround(0.8 + 0.1 * jj);
    state.CHORD2[j] = Math.fround(1.1 + 0.1 * jj);
    state.RLE[j * 3 + 0] = Math.fround(0.05 * jj);
    state.RLE[j * 3 + 1] = Math.fround(0.06 * jj);
    state.RLE[j * 3 + 2] = Math.fround(0.07 * jj);
  }
}

const massFile = [
  '# comment',
  'Lunit = 2.0 m',
  'Munit = 3.0 kg',
  'Tunit = 4.0 s',
  'g = 9.81 m/s^2',
  'rho = 1.2 kg/m^3',
  '* 2 3 4 5 6 7 8 9 10 11',
  '+ 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0',
  '1 1 2 3 0.1 0.2 0.3 0.01 0.02 0.03',
  '2 0.5 1.5 -0.5 0.2 0.1 0.05 0.02 -0.01 0.04',
].join('\n');

test('amass.f JS port matches Fortran reference', () => {
  const refBin = path.join(refDir, 'amass_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const ref = parseRef(runFortranRef());

  const state = makeState();
  MASINI(state);
  assert.ok(Math.abs(state.RMASS0 - ref.masini.rmass0) <= 1e-6);
  assertCloseArray(Array.from(state.XYZMASS0), ref.masini.xyzmass0);
  assertCloseArray([
    state.RINER0[0],
    state.RINER0[4],
    state.RINER0[8],
  ], ref.masini.rinerDiag);
  assert.equal(state.LMASS ? 1 : 0, ref.masini.lmass);

  state.UNITL = 2.0;
  state.UNITM = 3.0;
  state.UNITT = 4.0;
  state.UNCHL = 'm';
  state.UNCHM = 'kg';
  state.UNCHT = 's';
  UNITSET(state);
  assertCloseArray([
    state.UNITF,
    state.UNITS,
    state.UNITV,
    state.UNITA,
    state.UNITI,
    state.UNITD,
  ], ref.unitset);

  fillAppget(state);
  APPGET(state);
  assertCloseArray(Array.from(state.AMASS), ref.amass);
  assertCloseArray(Array.from(state.AINER), ref.ainer);

  state.RMASS0 = Math.fround(5.0);
  state.RINER0[0] = Math.fround(1.1);
  state.RINER0[4] = Math.fround(2.2);
  state.RINER0[8] = Math.fround(3.3);
  state.RINER0[3] = Math.fround(-0.1);
  state.RINER0[7] = Math.fround(-0.2);
  state.RINER0[2] = Math.fround(-0.3);
  state.GEE0 = Math.fround(9.81);
  state.RHO0 = Math.fround(1.225);
  state.XYZMASS0[0] = Math.fround(0.4);
  state.XYZMASS0[1] = Math.fround(-0.5);
  state.XYZMASS0[2] = Math.fround(0.6);
  state.UNITL = Math.fround(2.0);
  MASPUT(state, 0, 0);
  assertCloseArray(Array.from(state.PARVAL.slice(1, 13)), ref.parval);

  const out = MASGET(state, massFile);
  assert.equal(out.ERROR, false);
  assertCloseArray([state.RMASS0], [ref.masget.rmass0]);
  assertCloseArray(Array.from(state.XYZMASS0), ref.masget.xyzmass0);
  assertCloseArray(Array.from(state.RINER0), ref.masget.riner, 1e-3);
  assertCloseArray([state.GEE0, state.RHO0], [ref.masget.gee0, ref.masget.rho0]);
  assertCloseArray([state.UNITL, state.UNITM, state.UNITT], [ref.masget.unitl, ref.masget.unitm, ref.masget.unitt]);
  assert.equal(state.LMASS ? 1 : 0, ref.masget.lmass);
});

test('amass.f WASM port matches Fortran reference (numeric)', async (t) => {
  const refBin = path.join(refDir, 'amass_ref');
  if (!fs.existsSync(refBin)) {
    t.skip(`Fortran reference binary not found: ${refBin}`);
    return;
  }
  if (!fs.existsSync(wasmPath)) {
    t.skip(`WASM build not found: ${wasmPath}`);
    return;
  }
  const ref = parseRef(runFortranRef());

  const state = makeState();
  const wasm = await loadAmassWasm({ wasmPath });

  wasm.MASINI(state);
  assert.ok(Math.abs(state.RMASS0 - ref.masini.rmass0) <= 1e-6);
  assertCloseArray(Array.from(state.XYZMASS0), ref.masini.xyzmass0);
  assertCloseArray([
    state.RINER0[0],
    state.RINER0[4],
    state.RINER0[8],
  ], ref.masini.rinerDiag);
  assert.equal(state.LMASS ? 1 : 0, ref.masini.lmass);

  state.UNITL = 2.0;
  state.UNITM = 3.0;
  state.UNITT = 4.0;
  wasm.UNITSET(state);
  assertCloseArray([
    state.UNITF,
    state.UNITS,
    state.UNITV,
    state.UNITA,
    state.UNITI,
    state.UNITD,
  ], ref.unitset);

  fillAppget(state);
  wasm.APPGET(state);
  assertCloseArray(Array.from(state.AMASS), ref.amass);
  assertCloseArray(Array.from(state.AINER), ref.ainer);

  state.RMASS0 = Math.fround(5.0);
  state.RINER0[0] = Math.fround(1.1);
  state.RINER0[4] = Math.fround(2.2);
  state.RINER0[8] = Math.fround(3.3);
  state.RINER0[3] = Math.fround(-0.1);
  state.RINER0[7] = Math.fround(-0.2);
  state.RINER0[2] = Math.fround(-0.3);
  state.GEE0 = Math.fround(9.81);
  state.RHO0 = Math.fround(1.225);
  state.XYZMASS0[0] = Math.fround(0.4);
  state.XYZMASS0[1] = Math.fround(-0.5);
  state.XYZMASS0[2] = Math.fround(0.6);
  state.UNITL = Math.fround(2.0);
  wasm.MASPUT(state, 0, 0);
  assertCloseArray(Array.from(state.PARVAL.slice(1, 13)), ref.parval);
});

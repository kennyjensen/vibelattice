import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { TRMSET_CORE } from '../src/atrim.js';
import { loadAtrimWasm } from '../src/atrim_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'atrim_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
  const matches = proc.stdout.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g) || [];
  return matches.map((v) => Number(v.replace(/d/i, 'e')));
}

function parseRef(values) {
  const needed = 28;
  const trimmed = values.slice(Math.max(0, values.length - needed));
  let idx = 0;
  const cases = [];
  for (let c = 0; c < 2; c += 1) {
    const parvals = trimmed.slice(idx, idx + 5); idx += 5;
    const convals = trimmed.slice(idx, idx + 4); idx += 4;
    const icons = trimmed.slice(idx, idx + 4).map((v) => Math.trunc(v)); idx += 4;
    const itrim = Math.trunc(trimmed[idx++]);
    cases.push({ parvals, convals, icons, itrim });
  }
  return cases;
}

function assertCloseArray(actual, expected, tol = 1e-4) {
  assert.equal(actual.length, expected.length, 'length mismatch');
  for (let i = 0; i < actual.length; i += 1) {
    const diff = Math.abs(actual[i] - expected[i]);
    assert.ok(diff <= tol, `idx ${i} diff ${diff} > ${tol}`);
  }
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function makeState() {
  const NRUN = 2;
  const IPTOT = 30;
  const IVTOT = 5;
  const ICMAX = 10;
  const IVMAX = IVTOT;
  const parLen = IPTOT * (NRUN + 1) + 1;
  const conLen = ICMAX * (NRUN + 1) + 1;
  const iconLen = IVMAX * (NRUN + 1) + 1;
  return {
    DTR: Math.fround(Math.PI / 180.0),
    CREF: Math.fround(1.5),
    BREF: Math.fround(2.5),
    SREF: Math.fround(3.0),
    UNITL: Math.fround(2.0),
    RHO0: Math.fround(1.2),
    GEE0: Math.fround(9.81),
    RMASS0: Math.fround(100.0),
    NVTOT: IVTOT,
    IPTOT,
    IVTOT,
    IVMAX,
    ICMAX,
    IPPHI: 8,
    IPTHE: 9,
    IPCL: 6,
    IPVEE: 12,
    IPRAD: 15,
    IPRHO: 13,
    IPGEE: 14,
    IPFAC: 16,
    IPMASS: 20,
    IVALFA: 1,
    IVROTX: 3,
    IVROTY: 4,
    IVROTZ: 5,
    ICCL: 6,
    ICROTX: 3,
    ICROTY: 4,
    ICROTZ: 5,
    PARVAL: new Float32Array(parLen),
    CONVAL: new Float32Array(conLen),
    ICON: new Int32Array(iconLen),
    ITRIM: new Int32Array(NRUN + 1),
  };
}

function initState(state) {
  state.CONVAL[idx2(state.ICCL, 1, state.ICMAX)] = Math.fround(0.7);
  state.ICON[idx2(state.IVALFA, 1, state.IVMAX)] = state.ICCL;
  state.PARVAL[idx2(state.IPPHI, 1, state.IPTOT)] = Math.fround(20.0);
  state.PARVAL[idx2(state.IPVEE, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPCL, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPRHO, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPGEE, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPMASS, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPFAC, 1, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPRAD, 1, state.IPTOT)] = Math.fround(0.0);

  state.PARVAL[idx2(state.IPPHI, 2, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPCL, 2, state.IPTOT)] = Math.fround(0.6);
  state.PARVAL[idx2(state.IPVEE, 2, state.IPTOT)] = Math.fround(50.0);
  state.PARVAL[idx2(state.IPRAD, 2, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPFAC, 2, state.IPTOT)] = Math.fround(0.0);
  state.PARVAL[idx2(state.IPRHO, 2, state.IPTOT)] = Math.fround(1.1);
  state.PARVAL[idx2(state.IPGEE, 2, state.IPTOT)] = Math.fround(9.8);
  state.PARVAL[idx2(state.IPMASS, 2, state.IPTOT)] = Math.fround(120.0);
  return state;
}

function extractCase(state, ir) {
  const parvals = [
    state.PARVAL[idx2(state.IPVEE, ir, state.IPTOT)],
    state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)],
    state.PARVAL[idx2(state.IPRAD, ir, state.IPTOT)],
    state.PARVAL[idx2(state.IPFAC, ir, state.IPTOT)],
    state.PARVAL[idx2(state.IPTHE, ir, state.IPTOT)],
  ];
  const convals = [
    state.CONVAL[idx2(state.ICCL, ir, state.ICMAX)],
    state.CONVAL[idx2(state.ICROTX, ir, state.ICMAX)],
    state.CONVAL[idx2(state.ICROTY, ir, state.ICMAX)],
    state.CONVAL[idx2(state.ICROTZ, ir, state.ICMAX)],
  ];
  const icons = [
    state.ICON[idx2(state.IVALFA, ir, state.IVMAX)],
    state.ICON[idx2(state.IVROTX, ir, state.IVMAX)],
    state.ICON[idx2(state.IVROTY, ir, state.IVMAX)],
    state.ICON[idx2(state.IVROTZ, ir, state.IVMAX)],
  ];
  return { parvals, convals, icons, itrim: state.ITRIM[ir] };
}

test('atrim.f JS port matches Fortran reference', () => {
  const refBin = path.join(refDir, 'atrim_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refCases = parseRef(runFortranRef());

  const state = initState(makeState());
  TRMSET_CORE(state, 1, 1, 1, 1);
  TRMSET_CORE(state, 2, 2, 2, 2);

  for (let i = 1; i <= 2; i += 1) {
    const actual = extractCase(state, i);
    const expected = refCases[i - 1];
    assertCloseArray(actual.parvals, expected.parvals);
    assertCloseArray(actual.convals, expected.convals);
    assert.deepEqual(actual.icons, expected.icons);
    assert.equal(actual.itrim, expected.itrim);
  }
});

test('atrim.f WASM port matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'atrim_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refCases = parseRef(runFortranRef());

  const { TRMSET_CORE_wasm } = await loadAtrimWasm();
  let state = initState(makeState());
  state = TRMSET_CORE_wasm(state, 1, 1, 1, 1);
  state = TRMSET_CORE_wasm(state, 2, 2, 2, 2);

  for (let i = 1; i <= 2; i += 1) {
    const actual = extractCase(state, i);
    const expected = refCases[i - 1];
    assertCloseArray(actual.parvals, expected.parvals);
    assertCloseArray(actual.convals, expected.convals);
    assert.deepEqual(actual.icons, expected.icons);
    assert.equal(actual.itrim, expected.itrim);
  }
});

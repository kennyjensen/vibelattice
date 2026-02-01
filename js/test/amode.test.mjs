import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { RUNCHK, SYSMAT, APPMAT } from '../src/amode.js';
import { loadAmodeWasm } from '../src/amode_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'amode_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) throw proc.error;
  if (proc.status !== 0) throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  return proc.stdout;
}

function extractNumbers(text) {
  const matches = text.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g) || [];
  return matches.map((v) => Number(v.replace(/d/i, 'e')));
}

function splitSections(text) {
  const lines = text.split(/\r?\n/);
  const sections = new Map();
  let current = null;
  let buf = [];
  for (const line of lines) {
    if (line.startsWith('BEGIN ')) {
      if (current) sections.set(current, buf.join('\n'));
      current = line.slice(6).trim();
      buf = [];
    } else if (current) {
      buf.push(line);
    }
  }
  if (current) sections.set(current, buf.join('\n'));
  return sections;
}

function extractLogicals(text) {
  return text.split(/\r?\n/).map((line) => line.trim()).filter((v) => v === 'T' || v === 'F');
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
  const state = {
    NUMAX: 6,
    NDMAX: 1,
    JEMAX: 12,
    NCONTROL: 1,
    NVTOT: 5,
    IVTOT: 5,
    IVMAX: 6,
    NRMAX: 2,
    IPTOT: 30,
    UNITL: Math.fround(1.0),
    SREF: Math.fround(1.5),
    CREF: Math.fround(0.75),
    BREF: Math.fround(2.0),
    PI: Math.fround(3.1415927),
    DTR: Math.fround(3.1415927 / 180.0),

    JEU: 1,
    JEW: 2,
    JEQ: 3,
    JETH: 4,
    JEV: 5,
    JEP: 6,
    JER: 7,
    JEPH: 8,
    JEX: 9,
    JEY: 10,
    JEZ: 11,
    JEPS: 12,

    IPGEE: 14,
    IPRHO: 13,
    IPVEE: 12,
    IPPHI: 8,
    IPTHE: 9,
    IPPSI: 10,
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
    IPCLU: 28,
    IPCMU: 30,
    IPCLA: 27,
    IPCMA: 29,

    VINF: new Float32Array([0.8, -0.1, 0.2]),
    WROT: new Float32Array([0.01, -0.02, 0.03]),
    XYZREF: new Float32Array(3),

    CFTOT: new Float32Array([0.4, -0.1, 0.2]),
    CMTOT: new Float32Array([0.05, -0.02, 0.04]),
    CFTOT_U: new Float32Array(3 * 6),
    CMTOT_U: new Float32Array(3 * 6),
    CFTOT_D: new Float32Array(3 * 1),
    CMTOT_D: new Float32Array(3 * 1),

    AMASS: new Float32Array(9),
    AINER: new Float32Array(9),

    PARVAL: new Float32Array((30 + 1) * (2 + 1)),
    ICON: new Int32Array((6 + 1) * (2 + 1)),
  };

  for (let iu = 1; iu <= 6; iu += 1) {
    state.CFTOT_U[(iu - 1) * 3 + 0] = Math.fround(0.01 * iu);
    state.CFTOT_U[(iu - 1) * 3 + 1] = Math.fround(-0.02 * iu);
    state.CFTOT_U[(iu - 1) * 3 + 2] = Math.fround(0.03 * iu);
    state.CMTOT_U[(iu - 1) * 3 + 0] = Math.fround(0.005 * iu);
    state.CMTOT_U[(iu - 1) * 3 + 1] = Math.fround(-0.006 * iu);
    state.CMTOT_U[(iu - 1) * 3 + 2] = Math.fround(0.007 * iu);
  }
  state.CFTOT_D[0] = Math.fround(0.11);
  state.CFTOT_D[1] = Math.fround(-0.12);
  state.CFTOT_D[2] = Math.fround(0.13);
  state.CMTOT_D[0] = Math.fround(0.021);
  state.CMTOT_D[1] = Math.fround(-0.022);
  state.CMTOT_D[2] = Math.fround(0.023);

  state.AMASS[idx2(0, 0, 3)] = Math.fround(0.1);
  state.AMASS[idx2(1, 1, 3)] = Math.fround(0.2);
  state.AMASS[idx2(2, 2, 3)] = Math.fround(0.15);
  state.AMASS[idx2(0, 1, 3)] = Math.fround(0.01);
  state.AMASS[idx2(1, 0, 3)] = Math.fround(0.01);
  state.AMASS[idx2(0, 2, 3)] = Math.fround(-0.02);
  state.AMASS[idx2(2, 0, 3)] = Math.fround(-0.02);
  state.AMASS[idx2(1, 2, 3)] = Math.fround(0.03);
  state.AMASS[idx2(2, 1, 3)] = Math.fround(0.03);

  state.AINER[idx2(0, 0, 3)] = Math.fround(0.02);
  state.AINER[idx2(1, 1, 3)] = Math.fround(0.03);
  state.AINER[idx2(2, 2, 3)] = Math.fround(0.025);
  state.AINER[idx2(0, 1, 3)] = Math.fround(0.004);
  state.AINER[idx2(1, 0, 3)] = Math.fround(0.004);
  state.AINER[idx2(0, 2, 3)] = Math.fround(-0.003);
  state.AINER[idx2(2, 0, 3)] = Math.fround(-0.003);
  state.AINER[idx2(1, 2, 3)] = Math.fround(0.002);
  state.AINER[idx2(2, 1, 3)] = Math.fround(0.002);

  const IR = 1;
  state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)] = Math.fround(9.81);
  state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)] = Math.fround(1.225);
  state.PARVAL[idx2(state.IPVEE, IR, state.IPTOT)] = Math.fround(30.0);
  state.PARVAL[idx2(state.IPPHI, IR, state.IPTOT)] = Math.fround(5.0);
  state.PARVAL[idx2(state.IPTHE, IR, state.IPTOT)] = Math.fround(-2.0);
  state.PARVAL[idx2(state.IPPSI, IR, state.IPTOT)] = Math.fround(1.0);
  state.PARVAL[idx2(state.IPXCG, IR, state.IPTOT)] = Math.fround(0.1);
  state.PARVAL[idx2(state.IPYCG, IR, state.IPTOT)] = Math.fround(-0.2);
  state.PARVAL[idx2(state.IPZCG, IR, state.IPTOT)] = Math.fround(0.3);
  state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] = Math.fround(120.0);
  state.PARVAL[idx2(state.IPIXX, IR, state.IPTOT)] = Math.fround(12.0);
  state.PARVAL[idx2(state.IPIYY, IR, state.IPTOT)] = Math.fround(15.0);
  state.PARVAL[idx2(state.IPIZZ, IR, state.IPTOT)] = Math.fround(20.0);
  state.PARVAL[idx2(state.IPIXY, IR, state.IPTOT)] = Math.fround(0.5);
  state.PARVAL[idx2(state.IPIYZ, IR, state.IPTOT)] = Math.fround(-0.4);
  state.PARVAL[idx2(state.IPIZX, IR, state.IPTOT)] = Math.fround(0.3);
  state.PARVAL[idx2(state.IPCLU, IR, state.IPTOT)] = Math.fround(0.02);
  state.PARVAL[idx2(state.IPCMU, IR, state.IPTOT)] = Math.fround(-0.01);
  state.PARVAL[idx2(state.IPCLA, IR, state.IPTOT)] = Math.fround(0.03);
  state.PARVAL[idx2(state.IPCMA, IR, state.IPTOT)] = Math.fround(-0.015);

  for (let iv = 1; iv <= state.NVTOT; iv += 1) {
    state.ICON[idx2(iv, 1, state.IVMAX)] = iv;
    state.ICON[idx2(iv, 2, state.IVMAX)] = 1;
  }

  return state;
}

function extractMatrixNoRSYS(state, ASYS, BSYS, NSYS) {
  const out = [NSYS];
  for (let i = 1; i <= NSYS; i += 1) {
    for (let j = 1; j <= NSYS; j += 1) {
      out.push(ASYS[idx2(i, j, state.JEMAX + 1)]);
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      out.push(BSYS[idx2(i, n, state.JEMAX + 1)]);
    }
  }
  return out;
}

function extractRSYS(state, RSYS, NSYS) {
  const out = [];
  for (let i = 1; i <= NSYS; i += 1) {
    out.push(RSYS[i]);
  }
  return out;
}

function stripRSYSFromOutput(nums, nsys, ncontrol) {
  const out = [nums[0]];
  const rowLen = nsys + ncontrol + 1;
  for (let i = 0; i < nsys; i += 1) {
    const row = nums.slice(1 + i * rowLen, 1 + (i + 1) * rowLen);
    out.push(...row.slice(0, nsys + ncontrol));
  }
  return out;
}

test('amode.f JS port matches Fortran reference', () => {
  const refBin = path.join(refDir, 'amode_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refOut = runFortranRef();
  const sections = splitSections(refOut);
  const state = makeState();

  const ok1 = RUNCHK(state, 1);
  const ok2 = RUNCHK(state, 2);
  const runchkLog = extractLogicals(sections.get('RUNCHK'));
  assert.equal(ok1, runchkLog[0] === 'T');
  assert.equal(ok2, runchkLog[1] === 'T');

  const ASYS = new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1));
  const BSYS = new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1));
  const RSYS = new Float32Array(state.JEMAX + 1);
  const resSys = SYSMAT(state, 1, ASYS, BSYS, RSYS);
  const sysNums = extractNumbers(sections.get('SYSMAT'));
  const actualSys = extractMatrixNoRSYS(state, ASYS, BSYS, resSys.NSYS);
  const expectedSys = stripRSYSFromOutput(sysNums, resSys.NSYS, state.NCONTROL);
  assertCloseArray(actualSys, expectedSys, 1e-3);
  const sysRsysNums = extractNumbers(sections.get('RSYS_SYSMAT'));
  const actualRsys = extractRSYS(state, RSYS, resSys.NSYS);
  assertCloseArray(actualRsys, sysRsysNums, 1e-3);

  const resApp = APPMAT(state, 1, ASYS, BSYS, RSYS);
  const appNums = extractNumbers(sections.get('APPMAT'));
  const actualApp = extractMatrixNoRSYS(state, ASYS, BSYS, resApp.NSYS);
  const expectedApp = stripRSYSFromOutput(appNums, resApp.NSYS, state.NCONTROL);
  assertCloseArray(actualApp, expectedApp, 1e-3);
  const appRsysNums = extractNumbers(sections.get('RSYS_APPMAT'));
  const actualAppRsys = extractRSYS(state, RSYS, resApp.NSYS);
  assertCloseArray(actualAppRsys, appRsysNums, 1e-3);
});

test('amode.f WASM port matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'amode_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refOut = runFortranRef();
  const sections = splitSections(refOut);
  const { RUNCHK_wasm, SYSMAT_wasm, APPMAT_wasm } = await loadAmodeWasm();
  const state = makeState();

  const ok1 = RUNCHK_wasm(state, 1);
  const ok2 = RUNCHK_wasm(state, 2);
  const runchkLog = extractLogicals(sections.get('RUNCHK'));
  assert.equal(ok1, runchkLog[0] === 'T');
  assert.equal(ok2, runchkLog[1] === 'T');

  const sysRes = SYSMAT_wasm(state, 1);
  const sysNums = extractNumbers(sections.get('SYSMAT'));
  const actualSys = extractMatrixNoRSYS(state, state.__amode.ASYS, state.__amode.BSYS, sysRes.NSYS);
  const expectedSys = stripRSYSFromOutput(sysNums, sysRes.NSYS, state.NCONTROL);
  assertCloseArray(actualSys, expectedSys, 1e-3);
  const sysRsysNums = extractNumbers(sections.get('RSYS_SYSMAT'));
  const actualRsys = extractRSYS(state, state.__amode.RSYS, sysRes.NSYS);
  assertCloseArray(actualRsys, sysRsysNums, 1e-3);

  const appRes = APPMAT_wasm(state, 1);
  const appNums = extractNumbers(sections.get('APPMAT'));
  const actualApp = extractMatrixNoRSYS(state, state.__amode.ASYS, state.__amode.BSYS, appRes.NSYS);
  const expectedApp = stripRSYSFromOutput(appNums, appRes.NSYS, state.NCONTROL);
  assertCloseArray(actualApp, expectedApp, 1e-3);
  const appRsysNums = extractNumbers(sections.get('RSYS_APPMAT'));
  const actualAppRsys = extractRSYS(state, state.__amode.RSYS, appRes.NSYS);
  assertCloseArray(actualAppRsys, appRsysNums, 1e-3);
});

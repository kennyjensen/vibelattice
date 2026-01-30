import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import {
  MRFTOT,
  MRFSURF,
  MRFBODY,
  MRFSTRP,
  MRFELE,
  MRFHINGE,
  MRFCNC,
  MRFVM,
} from '../src/aoutmrf.js';
import { loadAoutmrfWasm } from '../src/aoutmrf_wasm.js';

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');

function runFortranRef() {
  const bin = path.join(refDir, 'aoutmrf_ref');
  const proc = spawnSync(bin, { encoding: 'utf8' });
  if (proc.error) {
    throw proc.error;
  }
  if (proc.status !== 0) {
    throw new Error(proc.stderr || `ref exited with ${proc.status}`);
  }
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
    PI: Math.fround(3.1415927),
    DTR: Math.fround(3.1415927 / 180.0),
    LNASA_SA: false,
    IYSYM: 0,
    IZSYM: 0,
    YSYM: 0.0,
    ZSYM: 0.0,
    NVOR: 1,
    NSURF: 1,
    NSTRIP: 1,
    NBODY: 1,
    NCONTROL: 1,
    NDESIGN: 1,
    IRUN: 1,

    TITLE: 'AOUTMRF REF',
    STITLE: [null, 'SURF1'],
    BTITLE: [null, 'BODY'],
    RTITLE: [null, 'RUN1'],
    DNAME: [null, 'CTRL1'],
    GNAME: [null, 'DES1'],

    ALFA: Math.fround(0.1),
    BETA: Math.fround(0.05),
    AMACH: Math.fround(0.2),
    SREF: Math.fround(1.5),
    CREF: Math.fround(0.75),
    BREF: Math.fround(2.0),
    XYZREF: new Float32Array([0.1, -0.2, 0.3]),

    WROT: new Float32Array([0.01, -0.02, 0.03]),

    CFTOT: new Float32Array([0.4, -0.1, 0.2]),
    CMTOT: new Float32Array([0.05, -0.02, 0.04]),
    CLTOT: Math.fround(0.6),
    CDTOT: Math.fround(0.08),
    CYTOT: Math.fround(-0.03),
    CDVTOT: Math.fround(0.01),
    CLFF: Math.fround(0.58),
    CDFF: Math.fround(0.07),
    CYFF: Math.fround(-0.02),
    SPANEF: Math.fround(0.85),

    DELCON: new Float32Array([0.0, 2.0]),
    DELDES: new Float32Array([0.0, -1.0]),

    SSURF: new Float32Array([0.0, 1.1]),
    CAVESURF: new Float32Array([0.0, 0.8]),
    CLSURF: new Float32Array([0.0, 0.5]),
    CDSURF: new Float32Array([0.0, 0.09]),
    CYSURF: new Float32Array([0.0, -0.02]),
    CMSURF: new Float32Array(3 * 2),
    CDVSURF: new Float32Array([0.0, 0.015]),
    CL_LSRF: new Float32Array([0.0, 0.45]),
    CD_LSRF: new Float32Array([0.0, 0.05]),

    NJ: new Int32Array([0, 1]),
    NK: new Int32Array([0, 1]),
    JFRST: new Int32Array([0, 1]),
    IMAGS: new Int32Array([0, 1]),
    IJFRST: new Int32Array([0, 1]),

    WSTRIP: new Float32Array([0.0, 0.4]),
    CHORD: new Float32Array([0.0, 0.9]),
    AINC: new Float32Array([0.0, 0.02]),
    RLE: new Float32Array(3 * 2),
    RLE1: new Float32Array(3 * 2),
    RLE2: new Float32Array(3 * 2),

    CL_LSTRP: new Float32Array([0.0, 0.4]),
    CD_LSTRP: new Float32Array([0.0, 0.03]),
    CDV_LSTRP: new Float32Array([0.0, 0.005]),
    CMLE_LSTRP: new Float32Array([0.0, -0.01]),
    CMC4_LSTRP: new Float32Array([0.0, -0.02]),
    CLT_LSTRP: new Float32Array([0.0, 0.35]),
    CN_LSTRP: new Float32Array([0.0, 0.5]),
    CA_LSTRP: new Float32Array([0.0, 0.1]),
    CNC: new Float32Array([0.0, 0.6]),
    DWWAKE: new Float32Array([0.0, 0.02]),

    RV1: new Float32Array(3 * 2),
    RV2: new Float32Array(3 * 2),
    DXV: new Float32Array([0.0, 0.15]),
    SLOPEC: new Float32Array([0.0, 0.02]),
    DCP: new Float32Array([0.0, -0.1]),

    ELBDY: new Float32Array([0.0, 1.2]),
    SRFBDY: new Float32Array([0.0, 0.9]),
    VOLBDY: new Float32Array([0.0, 0.3]),
    CLBDY: new Float32Array([0.0, 0.05]),
    CDBDY: new Float32Array([0.0, 0.01]),
    CMBDY: new Float32Array(3 * 2),
    CFBDY: new Float32Array(3 * 2),

    CHINGE: new Float32Array([0.0, -0.03]),

    ENSY: new Float32Array([0.0, 0.0]),
    ENSZ: new Float32Array([0.0, 1.0]),
  };

  state.CMSURF[idx2(0, 1, 3)] = Math.fround(0.01);
  state.CMSURF[idx2(1, 1, 3)] = Math.fround(-0.02);
  state.CMSURF[idx2(2, 1, 3)] = Math.fround(0.03);

  state.RLE[idx2(0, 1, 3)] = Math.fround(0.0);
  state.RLE[idx2(1, 1, 3)] = Math.fround(0.5);
  state.RLE[idx2(2, 1, 3)] = Math.fround(0.1);
  state.RLE1[idx2(0, 1, 3)] = Math.fround(0.0);
  state.RLE1[idx2(1, 1, 3)] = Math.fround(0.45);
  state.RLE1[idx2(2, 1, 3)] = Math.fround(0.1);
  state.RLE2[idx2(0, 1, 3)] = Math.fround(0.1);
  state.RLE2[idx2(1, 1, 3)] = Math.fround(0.55);
  state.RLE2[idx2(2, 1, 3)] = Math.fround(0.1);

  state.RV1[idx2(0, 1, 3)] = Math.fround(0.0);
  state.RV1[idx2(1, 1, 3)] = Math.fround(0.4);
  state.RV1[idx2(2, 1, 3)] = Math.fround(0.1);
  state.RV2[idx2(0, 1, 3)] = Math.fround(0.2);
  state.RV2[idx2(1, 1, 3)] = Math.fround(0.6);
  state.RV2[idx2(2, 1, 3)] = Math.fround(0.1);

  state.CMBDY[idx2(0, 1, 3)] = Math.fround(0.004);
  state.CMBDY[idx2(1, 1, 3)] = Math.fround(-0.005);
  state.CMBDY[idx2(2, 1, 3)] = Math.fround(0.006);
  state.CFBDY[idx2(0, 1, 3)] = Math.fround(0.02);
  state.CFBDY[idx2(1, 1, 3)] = Math.fround(-0.01);
  state.CFBDY[idx2(2, 1, 3)] = Math.fround(0.015);

  return state;
}

function getOutputsJS(state) {
  return {
    MRFTOT: MRFTOT(state, 6, 'TESTFILE'),
    MRFSURF: MRFSURF(state, 6),
    MRFBODY: MRFBODY(state, 6),
    MRFSTRP: MRFSTRP(state, 6),
    MRFELE: MRFELE(state, 6),
    MRFHINGE: MRFHINGE(state, 6),
    MRFCNC: MRFCNC(state, 6),
    MRFVM: MRFVM(state, 6),
  };
}

test('aoutmrf.f JS port matches Fortran reference', () => {
  const refBin = path.join(refDir, 'aoutmrf_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refOut = runFortranRef();
  const refSections = splitSections(refOut);
  const state = makeState();
  const outputs = getOutputsJS(state);

  for (const [key, text] of Object.entries(outputs)) {
    const refText = refSections.get(key);
    assert.ok(refText != null, `missing reference section ${key}`);
    const actualNums = extractNumbers(text);
    const expectedNums = extractNumbers(refText);
    assertCloseArray(actualNums, expectedNums, 1e-4);
  }
});

test('aoutmrf.f WASM port matches Fortran reference', async () => {
  const refBin = path.join(refDir, 'aoutmrf_ref');
  if (!fs.existsSync(refBin)) {
    assert.fail(`Fortran reference binary not found: ${refBin}`);
  }
  const refOut = runFortranRef();
  const refSections = splitSections(refOut);
  const { MRFTOT_wasm, MRFSURF_wasm, MRFBODY_wasm, MRFSTRP_wasm, MRFELE_wasm, MRFHINGE_wasm, MRFCNC_wasm, MRFVM_wasm } = await loadAoutmrfWasm();
  const state = makeState();
  const outputs = {
    MRFTOT: MRFTOT_wasm(state, 6, 'TESTFILE'),
    MRFSURF: MRFSURF_wasm(state, 6),
    MRFBODY: MRFBODY_wasm(state, 6),
    MRFSTRP: MRFSTRP_wasm(state, 6),
    MRFELE: MRFELE_wasm(state, 6),
    MRFHINGE: MRFHINGE_wasm(state, 6),
    MRFCNC: MRFCNC_wasm(state, 6),
    MRFVM: MRFVM_wasm(state, 6),
  };

  for (const [key, text] of Object.entries(outputs)) {
    const refText = refSections.get(key);
    assert.ok(refText != null, `missing reference section ${key}`);
    const actualNums = extractNumbers(text);
    const expectedNums = extractNumbers(refText);
    assertCloseArray(actualNums, expectedNums, 1e-4);
  }
});

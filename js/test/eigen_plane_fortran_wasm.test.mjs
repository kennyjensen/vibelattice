import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import { execFileSync } from 'node:child_process';
import {
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
} from '../src/exec_pipeline.js';
import { EXEC } from '../src/aoper.js';
import { RUNCHK, SYSMAT, EIGSOL } from '../src/amode.js';
import { APPGET } from '../src/amass.js';
import { loadAmodeWasm } from '../src/amode_wasm.js';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const planeAvlPath = path.join(runsDir, 'plane.avl');
const planeRunPath = path.join(runsDir, 'plane.run');
const planeMassPath = path.join(runsDir, 'plane.mass');
const avlBinPath = path.join(repoRoot, 'third_party', 'avl', 'bin', 'avl');

function normalizeRunKey(text) {
  return String(text || '')
    .toLowerCase()
    .replaceAll('.', '')
    .replaceAll('_', '')
    .replaceAll('-', '')
    .replace(/\s+/g, ' ')
    .trim();
}

function parsePlaneRun(text) {
  const lines = String(text || '').split(/\r?\n/);
  const out = {
    inputs: {
      alphaDeg: 0,
      betaDeg: 0,
      cl: 0,
      cd0: 0,
      bankDeg: 0,
      vel: 0,
      rho: 0,
      gee: 0,
      xcg: 0,
      ycg: 0,
      zcg: 0,
    },
    constraints: [],
  };

  const varMap = {
    alpha: 'alpha',
    beta: 'beta',
    'pb/2v': 'p',
    'qc/2v': 'q',
    'rb/2v': 'r',
  };
  const constraintMap = {
    alpha: 'alpha',
    beta: 'beta',
    'pb/2v': 'p',
    'qc/2v': 'q',
    'rb/2v': 'r',
    cl: 'cl',
    cy: 'cy',
    'cl roll mom': 'cmx',
    'cm pitchmom': 'cmy',
    'cn yaw mom': 'cmz',
  };

  const hasMappedVariable = (variableKey) => out.constraints.some((row) => row.variable === variableKey);

  for (const rawLine of lines) {
    const line = String(rawLine || '');

    const arrowMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*->\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*$/);
    if (arrowMatch) {
      const leftRaw = arrowMatch[1].trim();
      const rightRaw = arrowMatch[2].trim();
      const num = Number(arrowMatch[3]);
      if (!Number.isFinite(num)) continue;
      const variable = varMap[normalizeRunKey(leftRaw)] || `ctrl:${leftRaw}`;
      const constraint = constraintMap[normalizeRunKey(rightRaw)] || (rightRaw ? `ctrl:${rightRaw}` : 'none');
      out.constraints.push({ variable, constraint, numeric: num });
      continue;
    }

    const valueMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*(.*?)\s*$/);
    if (!valueMatch) continue;
    const keyNorm = normalizeRunKey(valueMatch[1].trim());
    const value = Number(valueMatch[2]);
    if (!Number.isFinite(value)) continue;

    if (keyNorm === 'alpha') out.inputs.alphaDeg = value;
    else if (keyNorm === 'beta') out.inputs.betaDeg = value;
    else if (keyNorm === 'cl') out.inputs.cl = value;
    else if (keyNorm === 'cdo') out.inputs.cd0 = value;
    else if (keyNorm === 'bank') out.inputs.bankDeg = value;
    else if (keyNorm === 'velocity') out.inputs.vel = value;
    else if (keyNorm === 'density') out.inputs.rho = value;
    else if (keyNorm === 'gravacc') out.inputs.gee = value;
    else if (keyNorm === 'xcg') out.inputs.xcg = value;
    else if (keyNorm === 'ycg') out.inputs.ycg = value;
    else if (keyNorm === 'zcg') out.inputs.zcg = value;

    if (keyNorm === 'alpha' && !hasMappedVariable('alpha')) out.constraints.push({ variable: 'alpha', constraint: 'alpha', numeric: value });
    else if (keyNorm === 'beta' && !hasMappedVariable('beta')) out.constraints.push({ variable: 'beta', constraint: 'beta', numeric: value });
    else if (keyNorm === 'pb/2v' && !hasMappedVariable('p')) out.constraints.push({ variable: 'p', constraint: 'p', numeric: value });
    else if (keyNorm === 'qc/2v' && !hasMappedVariable('q')) out.constraints.push({ variable: 'q', constraint: 'q', numeric: value });
    else if (keyNorm === 'rb/2v' && !hasMappedVariable('r')) out.constraints.push({ variable: 'r', constraint: 'r', numeric: value });
  }

  return out;
}

function parsePlaneMass(text) {
  const lines = text.split(/\r?\n/).map((l) => l.trim()).filter(Boolean);
  const numLine = lines.find((l) => /^[-+0-9.]/.test(l));
  if (!numLine) throw new Error('plane.mass numeric line missing');
  const nums = numLine.split(/\s+/).map(Number).filter(Number.isFinite);
  if (nums.length < 7) throw new Error('plane.mass numeric line incomplete');
  return {
    mass: nums[0],
    xcg: nums[1],
    ycg: nums[2],
    zcg: nums[3],
    ixx: nums[4],
    iyy: nums[5],
    izz: nums[6],
    ixy: nums[7] ?? 0,
    ixz: nums[8] ?? 0,
    iyz: nums[9] ?? 0,
  };
}

function parsePlaneEig(text) {
  const out = [];
  const lines = text.split(/\r?\n/);
  for (const line of lines) {
    const t = line.trim();
    if (!t || t.startsWith('#')) continue;
    const parts = t.split(/\s+/);
    if (parts.length < 3) continue;
    const run = Number(parts[0]);
    const re = Number(parts[1]);
    const im = Number(parts[2]);
    if (!Number.isFinite(run) || !Number.isFinite(re) || !Number.isFinite(im)) continue;
    if (run !== 1) continue;
    out.push({ re, im });
  }
  return out;
}

function sortEig(list) {
  return [...list].sort((a, b) => (a.re - b.re) || (a.im - b.im));
}

function computeFortranPlaneEigen() {
  if (!fs.existsSync(avlBinPath)) {
    assert.fail(`missing Fortran AVL binary: ${avlBinPath}`);
  }

  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'avl-eig-'));
  try {
    const tmpAvl = path.join(tmpDir, 'plane.avl');
    const tmpRun = path.join(tmpDir, 'plane.run');
    const tmpMass = path.join(tmpDir, 'plane.mass');
    const tmpEig = path.join(tmpDir, 'plane.eig');

    fs.copyFileSync(planeAvlPath, tmpAvl);
    fs.copyFileSync(planeRunPath, tmpRun);
    fs.copyFileSync(planeMassPath, tmpMass);

    // Disable graphics so MODE N runs headless in CI/CLI environments.
    const batch = [
      'PLOP',
      'G F',
      '',
      'MODE',
      'N',
      'W',
      'plane.eig',
      '',
      '',
      'QUIT',
      '',
    ].join('\n');

    try {
      execFileSync(avlBinPath, ['plane.avl'], {
        cwd: tmpDir,
        input: batch,
        encoding: 'utf8',
        maxBuffer: 1024 * 1024 * 8,
      });
    } catch (error) {
      const stdout = String(error?.stdout || '');
      const stderr = String(error?.stderr || '');
      assert.fail(`Fortran AVL eigen run failed.\nSTDOUT:\n${stdout}\nSTDERR:\n${stderr}`);
    }

    if (!fs.existsSync(tmpEig)) {
      assert.fail(`Fortran AVL did not generate eigenvalue file: ${tmpEig}`);
    }

    return sortEig(parsePlaneEig(fs.readFileSync(tmpEig, 'utf8')));
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

function applyMassAndRefsToState(state, mass) {
  const IR = 1;
  const idx2 = (i, j, dim1) => i + dim1 * j;
  state.LMASS = true;
  state.UNITL = 1.0;
  state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] = Math.fround(mass.mass);
  state.PARVAL[idx2(state.IPXCG, IR, state.IPTOT)] = Math.fround(mass.xcg);
  state.PARVAL[idx2(state.IPYCG, IR, state.IPTOT)] = Math.fround(mass.ycg);
  state.PARVAL[idx2(state.IPZCG, IR, state.IPTOT)] = Math.fround(mass.zcg);
  state.PARVAL[idx2(state.IPIXX, IR, state.IPTOT)] = Math.fround(mass.ixx);
  state.PARVAL[idx2(state.IPIYY, IR, state.IPTOT)] = Math.fround(mass.iyy);
  state.PARVAL[idx2(state.IPIZZ, IR, state.IPTOT)] = Math.fround(mass.izz);
  state.PARVAL[idx2(state.IPIXY, IR, state.IPTOT)] = Math.fround(-mass.ixy);
  state.PARVAL[idx2(state.IPIYZ, IR, state.IPTOT)] = Math.fround(-mass.iyz);
  state.PARVAL[idx2(state.IPIZX, IR, state.IPTOT)] = Math.fround(-mass.ixz);

  state.JEMAX = 12;
  state.JEU = 1;
  state.JEW = 2;
  state.JEQ = 3;
  state.JETH = 4;
  state.JEV = 5;
  state.JEP = 6;
  state.JER = 7;
  state.JEPH = 8;
  state.JEX = 9;
  state.JEY = 10;
  state.JEZ = 11;
  state.JEPS = 12;
}

function applyRunConstraintsToState(state, model, constraints) {
  const idx2 = (i, j, dim1) => i + dim1 * j;
  const controlNames = model?.controlMap ? Array.from(model.controlMap.keys()) : [];
  const controlIndex = new Map();
  controlNames.forEach((name, idx) => controlIndex.set(name, idx + 1));

  state.ICON.fill(0);
  state.CONVAL.fill(0);

  const constraintMap = {
    alpha: state.ICALFA,
    beta: state.ICBETA,
    p: state.ICROTX,
    q: state.ICROTY,
    r: state.ICROTZ,
    cl: state.ICCL,
    cy: state.ICCY,
    cmx: state.ICMOMX,
    cmy: state.ICMOMY,
    cmz: state.ICMOMZ,
  };
  const variableMap = {
    alpha: state.IVALFA,
    beta: state.IVBETA,
    p: state.IVROTX,
    q: state.IVROTY,
    r: state.IVROTZ,
  };

  for (const row of constraints || []) {
    let ic = constraintMap[row.constraint];
    if (!ic && row.constraint?.startsWith('ctrl:')) {
      const name = row.constraint.slice(5);
      const n = controlIndex.get(name);
      if (n) ic = state.ICTOT + n;
    }
    if (!ic) continue;

    state.CONVAL[idx2(ic, 1, state.ICMAX)] = Math.fround(Number(row.numeric) || 0);

    let iv = variableMap[row.variable];
    if (!iv && row.variable?.startsWith('ctrl:')) {
      const name = row.variable.slice(5);
      const n = controlIndex.get(name);
      if (n) iv = state.IVTOT + n;
    }
    if (!iv) continue;
    state.ICON[idx2(iv, 1, state.IVMAX)] = ic;
  }
}

function computeJsEigen(model, runCase, massVals) {
  const state = buildExecState(model, {
    alpha: runCase.inputs.alphaDeg,
    beta: runCase.inputs.betaDeg,
    cl: runCase.inputs.cl,
    vel: runCase.inputs.vel,
    rho: runCase.inputs.rho,
    gee: runCase.inputs.gee,
    bank: runCase.inputs.bankDeg,
    cd0: runCase.inputs.cd0,
    xcg: massVals.xcg,
    ycg: massVals.ycg,
    zcg: massVals.zcg,
    cmx: 0,
    cmy: 0,
    cmz: 0,
  });
  applyMassAndRefsToState(state, massVals);
  buildGeometry(state, model);
  APPGET(state);
  applyRunConstraintsToState(state, model, runCase.constraints);

  assert.ok(RUNCHK(state, 1), 'run constraints invalid for JS state');
  EXEC(state, 10, 0, 1);

  const ASYS = new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1));
  const BSYS = new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1));
  const RSYS = new Float32Array(state.JEMAX + 1);
  const sys = SYSMAT(state, 1, ASYS, BSYS, RSYS);
  const eig = EIGSOL(state, 1, 1.0e-5, ASYS, sys.NSYS);
  return sortEig(eig.EVAL || []);
}

async function computeWasmEigen(model, runCase, massVals) {
  const state = buildExecState(model, {
    alpha: runCase.inputs.alphaDeg,
    beta: runCase.inputs.betaDeg,
    cl: runCase.inputs.cl,
    vel: runCase.inputs.vel,
    rho: runCase.inputs.rho,
    gee: runCase.inputs.gee,
    bank: runCase.inputs.bankDeg,
    cd0: runCase.inputs.cd0,
    xcg: massVals.xcg,
    ycg: massVals.ycg,
    zcg: massVals.zcg,
    cmx: 0,
    cmy: 0,
    cmz: 0,
  });
  applyMassAndRefsToState(state, massVals);
  buildGeometry(state, model);
  APPGET(state);
  applyRunConstraintsToState(state, model, runCase.constraints);

  assert.ok(RUNCHK(state, 1), 'run constraints invalid for WASM state');
  EXEC(state, 10, 0, 1);

  const wasm = await loadAmodeWasm();
  const sys = wasm.SYSMAT_wasm(state, 1);
  const eig = wasm.EIGSOL_wasm(state, 1, 1.0e-5, state.__amode.ASYS, sys.NSYS);
  return sortEig(eig.EVAL || []);
}

test('plane eigenmodes parity: plane.avl/run/mass vs compiled Fortran AVL (JS and WASM)', { timeout: 120000 }, async () => {
  if (!fs.existsSync(planeAvlPath)) assert.fail(`missing ${planeAvlPath}`);
  if (!fs.existsSync(planeRunPath)) assert.fail(`missing ${planeRunPath}`);
  if (!fs.existsSync(planeMassPath)) assert.fail(`missing ${planeMassPath}`);

  const avlText = fs.readFileSync(planeAvlPath, 'utf8');
  const runCase = parsePlaneRun(fs.readFileSync(planeRunPath, 'utf8'));
  const massVals = parsePlaneMass(fs.readFileSync(planeMassPath, 'utf8'));
  const refEig = computeFortranPlaneEigen();
  assert.ok(refEig.length > 0, 'Fortran AVL produced no eigenvalues');

  const model = await buildSolverModel(avlText, {});
  const jsEig = computeJsEigen(model, runCase, massVals);
  const wasmEig = await computeWasmEigen(model, runCase, massVals);

  assert.equal(jsEig.length, refEig.length, 'JS eigenvalue count mismatch vs plane.eig');
  assert.equal(wasmEig.length, refEig.length, 'WASM eigenvalue count mismatch vs plane.eig');

  const tolRe = 3e-3;
  const tolIm = 1e-3;
  for (let i = 0; i < refEig.length; i += 1) {
    const dr = Math.abs(jsEig[i].re - refEig[i].re);
    const di = Math.abs(jsEig[i].im - refEig[i].im);
    assert.ok(dr <= tolRe, `JS real eig[${i}] diff ${dr} > ${tolRe}`);
    assert.ok(di <= tolIm, `JS imag eig[${i}] diff ${di} > ${tolIm}`);
  }

  for (let i = 0; i < refEig.length; i += 1) {
    const dr = Math.abs(wasmEig[i].re - refEig[i].re);
    const di = Math.abs(wasmEig[i].im - refEig[i].im);
    assert.ok(dr <= tolRe, `WASM real eig[${i}] diff ${dr} > ${tolRe}`);
    assert.ok(di <= tolIm, `WASM imag eig[${i}] diff ${di} > ${tolIm}`);
  }
});

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
const supraAvlPath = path.join(runsDir, 'supra.avl');
const supraRunPath = path.join(runsDir, 'supra.run');
const supraMassPath = path.join(runsDir, 'supra.mass');
const supraEigPath = path.join(runsDir, 'supra.eig');
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

function parseSupraRunCases(text) {
  const lines = String(text || '').split(/\r?\n/);
  const cases = [];
  let current = null;

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

  function startCase(nameRaw = '') {
    if (current) cases.push(current);
    current = {
      name: String(nameRaw || '').trim(),
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
        mass: 0,
        ixx: 0,
        iyy: 0,
        izz: 0,
        ixy: 0,
        iyz: 0,
        izx: 0,
        controlValues: {},
      },
      constraints: [],
    };
  }

  function hasMappedVariable(variableKey) {
    return current?.constraints?.some((row) => row.variable === variableKey);
  }

  for (const rawLine of lines) {
    const line = String(rawLine || '');

    const caseMatch = line.match(/^\s*Run case\s+\d+\s*:\s*(.*?)\s*$/i);
    if (caseMatch) {
      startCase(caseMatch[1]);
      continue;
    }
    if (!current) continue;

    const arrowMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*->\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*$/);
    if (arrowMatch) {
      const leftRaw = arrowMatch[1].trim();
      const rightRaw = arrowMatch[2].trim();
      const num = Number(arrowMatch[3]);
      if (!Number.isFinite(num)) continue;
      const variable = varMap[normalizeRunKey(leftRaw)] || `ctrl:${leftRaw}`;
      const constraint = constraintMap[normalizeRunKey(rightRaw)] || (rightRaw ? `ctrl:${rightRaw}` : 'none');
      current.constraints.push({ variable, constraint, numeric: num });
      continue;
    }

    const valueMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*(.*?)\s*$/);
    if (!valueMatch) continue;
    const keyNorm = normalizeRunKey(valueMatch[1].trim());
    const value = Number(valueMatch[2]);
    if (!Number.isFinite(value)) continue;

    if (keyNorm === 'alpha') current.inputs.alphaDeg = value;
    else if (keyNorm === 'beta') current.inputs.betaDeg = value;
    else if (keyNorm === 'cl') current.inputs.cl = value;
    else if (keyNorm === 'cdo' || keyNorm === 'cd0') current.inputs.cd0 = value;
    else if (keyNorm === 'bank') current.inputs.bankDeg = value;
    else if (keyNorm === 'velocity') current.inputs.vel = value;
    else if (keyNorm === 'density') current.inputs.rho = value;
    else if (keyNorm === 'gravacc') current.inputs.gee = value;
    else if (keyNorm === 'xcg') current.inputs.xcg = value;
    else if (keyNorm === 'ycg') current.inputs.ycg = value;
    else if (keyNorm === 'zcg') current.inputs.zcg = value;
    else if (keyNorm === 'mass') current.inputs.mass = value;
    else if (keyNorm === 'ixx') current.inputs.ixx = value;
    else if (keyNorm === 'iyy') current.inputs.iyy = value;
    else if (keyNorm === 'izz') current.inputs.izz = value;
    else if (keyNorm === 'ixy') current.inputs.ixy = -value;
    else if (keyNorm === 'iyz') current.inputs.iyz = -value;
    else if (keyNorm === 'izx') current.inputs.izx = -value;
    else current.inputs.controlValues[keyNorm] = value;

    if (keyNorm === 'alpha' && !hasMappedVariable('alpha')) current.constraints.push({ variable: 'alpha', constraint: 'alpha', numeric: value });
    else if (keyNorm === 'beta' && !hasMappedVariable('beta')) current.constraints.push({ variable: 'beta', constraint: 'beta', numeric: value });
    else if (keyNorm === 'pb/2v' && !hasMappedVariable('p')) current.constraints.push({ variable: 'p', constraint: 'p', numeric: value });
    else if (keyNorm === 'qc/2v' && !hasMappedVariable('q')) current.constraints.push({ variable: 'q', constraint: 'q', numeric: value });
    else if (keyNorm === 'rb/2v' && !hasMappedVariable('r')) current.constraints.push({ variable: 'r', constraint: 'r', numeric: value });
  }
  if (current) cases.push(current);

  return cases.filter((entry) => entry.constraints.length > 0);
}

function parseFortranEigByRun(text) {
  const byRun = new Map();
  const lines = String(text || '').split(/\r?\n/);
  for (const raw of lines) {
    const t = raw.trim();
    if (!t || t.startsWith('#')) continue;
    const parts = t.split(/\s+/);
    if (parts.length < 3) continue;
    const run = Number(parts[0]);
    const re = Number(parts[1]);
    const im = Number(parts[2]);
    if (!Number.isFinite(run) || !Number.isFinite(re) || !Number.isFinite(im)) continue;
    if (!byRun.has(run)) byRun.set(run, []);
    byRun.get(run).push({ re, im });
  }
  return byRun;
}

function copyDatDependencies(srcDir, dstDir) {
  const entries = fs.readdirSync(srcDir, { withFileTypes: true });
  for (const ent of entries) {
    if (!ent.isFile()) continue;
    if (!ent.name.toLowerCase().endsWith('.dat')) continue;
    fs.copyFileSync(path.join(srcDir, ent.name), path.join(dstDir, ent.name));
  }
}

function parseMassUnitScale(text) {
  const lines = String(text || '').split(/\r?\n/);
  for (const raw of lines) {
    const line = String(raw || '').replace(/!.*/, '').replace(/#.*/, '').trim();
    if (!line) continue;
    const m = line.match(/^Lunit\s*=\s*([^\s]+)/i);
    if (!m) continue;
    const v = Number(m[1]);
    if (Number.isFinite(v) && v > 0) return v;
  }
  return 1.0;
}

function sortEig(list) {
  return [...list].sort((a, b) => (a.re - b.re) || (a.im - b.im));
}

function computeFortranSupraEigenByRun() {
  if (!fs.existsSync(avlBinPath)) {
    assert.fail(`missing Fortran AVL binary: ${avlBinPath}`);
  }

  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'avl-supra-eig-'));
  try {
    const tmpAvl = path.join(tmpDir, 'supra.avl');
    const tmpRun = path.join(tmpDir, 'supra.run');
    const tmpMass = path.join(tmpDir, 'supra.mass');
    const tmpEig = path.join(tmpDir, 'supra.eig');

    fs.copyFileSync(supraAvlPath, tmpAvl);
    fs.copyFileSync(supraRunPath, tmpRun);
    fs.copyFileSync(supraMassPath, tmpMass);
    copyDatDependencies(runsDir, tmpDir);

    const batch = [
      'PLOP',
      'G F',
      '',
      'MODE',
      'N',
      'W',
      'supra.eig',
      '',
      '',
      'QUIT',
      '',
    ].join('\n');

    let runError = null;
    try {
      execFileSync(avlBinPath, ['supra.avl'], {
        cwd: tmpDir,
        input: batch,
        encoding: 'utf8',
        maxBuffer: 1024 * 1024 * 8,
      });
    } catch (error) {
      runError = error;
    }

    if (!fs.existsSync(tmpEig)) {
      if (runError) {
        const stdout = String(runError?.stdout || '');
        const stderr = String(runError?.stderr || '');
        assert.fail(`Fortran AVL supra eigen run failed.\nSTDOUT:\n${stdout}\nSTDERR:\n${stderr}`);
      }
      const hasStatic = fs.existsSync(supraEigPath);
      if (!hasStatic) {
        assert.fail(`Fortran AVL did not generate eigenvalue file: ${tmpEig}`);
      }
      return parseFortranEigByRun(fs.readFileSync(supraEigPath, 'utf8'));
    }

    return parseFortranEigByRun(fs.readFileSync(tmpEig, 'utf8'));
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function applyMassAndRefsToState(state, inputs) {
  const IR = 1;
  state.LMASS = true;

  state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] = Math.fround(Number(inputs.mass) || 0);
  state.PARVAL[idx2(state.IPXCG, IR, state.IPTOT)] = Math.fround(Number(inputs.xcg) || 0);
  state.PARVAL[idx2(state.IPYCG, IR, state.IPTOT)] = Math.fround(Number(inputs.ycg) || 0);
  state.PARVAL[idx2(state.IPZCG, IR, state.IPTOT)] = Math.fround(Number(inputs.zcg) || 0);
  state.PARVAL[idx2(state.IPIXX, IR, state.IPTOT)] = Math.fround(Number(inputs.ixx) || 0);
  state.PARVAL[idx2(state.IPIYY, IR, state.IPTOT)] = Math.fround(Number(inputs.iyy) || 0);
  state.PARVAL[idx2(state.IPIZZ, IR, state.IPTOT)] = Math.fround(Number(inputs.izz) || 0);
  state.PARVAL[idx2(state.IPIXY, IR, state.IPTOT)] = Math.fround(-(Number(inputs.ixy) || 0));
  state.PARVAL[idx2(state.IPIYZ, IR, state.IPTOT)] = Math.fround(-(Number(inputs.iyz) || 0));
  state.PARVAL[idx2(state.IPIZX, IR, state.IPTOT)] = Math.fround(-(Number(inputs.izx) || 0));

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
    if (!ic && String(row.constraint || '').startsWith('ctrl:')) {
      const name = String(row.constraint).slice(5);
      const n = controlIndex.get(name);
      if (n) ic = state.ICTOT + n;
    }
    if (!ic) continue;
    state.CONVAL[idx2(ic, 1, state.ICMAX)] = Math.fround(Number(row.numeric) || 0);

    let iv = variableMap[row.variable];
    if (!iv && String(row.variable || '').startsWith('ctrl:')) {
      const name = String(row.variable).slice(5);
      const n = controlIndex.get(name);
      if (n) iv = state.IVTOT + n;
    }
    if (!iv) continue;
    state.ICON[idx2(iv, 1, state.IVMAX)] = ic;
  }
}

function applyControlInitialGuesses(state, model, inputs) {
  const values = inputs?.controlValues && typeof inputs.controlValues === 'object'
    ? inputs.controlValues
    : {};
  if (!model?.controlMap) return;
  for (const [name, idx] of model.controlMap.entries()) {
    const key = normalizeRunKey(name);
    const val = values[key];
    if (!Number.isFinite(val)) continue;
    state.DELCON[idx] = Math.fround(val);
  }
}

function computeJsEigenForRun(model, runCase, unitl = 1.0) {
  const state = buildExecState(model, {
    alpha: Number(runCase.inputs.alphaDeg) || 0,
    beta: Number(runCase.inputs.betaDeg) || 0,
    cl: Number(runCase.inputs.cl) || 0,
    vel: Number(runCase.inputs.vel) || 0,
    rho: Number(runCase.inputs.rho) || 0,
    gee: Number(runCase.inputs.gee) || 0,
    bank: Number(runCase.inputs.bankDeg) || 0,
    cd0: Number(runCase.inputs.cd0) || 0,
    xcg: Number(runCase.inputs.xcg) || 0,
    ycg: Number(runCase.inputs.ycg) || 0,
    zcg: Number(runCase.inputs.zcg) || 0,
    cmx: 0,
    cmy: 0,
    cmz: 0,
    unitl,
  });
  applyMassAndRefsToState(state, runCase.inputs);
  buildGeometry(state, model);
  APPGET(state);
  applyRunConstraintsToState(state, model, runCase.constraints);
  applyControlInitialGuesses(state, model, runCase.inputs);

  assert.ok(RUNCHK(state, 1), `run constraints invalid for JS state (${runCase.name})`);
  EXEC(state, 20, 0, 1);

  const ASYS = new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1));
  const BSYS = new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1));
  const RSYS = new Float32Array(state.JEMAX + 1);
  const sys = SYSMAT(state, 1, ASYS, BSYS, RSYS);
  const eig = EIGSOL(state, 1, 1.0e-5, ASYS, sys.NSYS);
  return {
    eig: sortEig(eig.EVAL || []),
    nsys: sys.NSYS,
    vee: Number(state.PARVAL[idx2(state.IPVEE, 1, state.IPTOT)] || 0),
    mass: Number(state.PARVAL[idx2(state.IPMASS, 1, state.IPTOT)] || 0),
  };
}

async function computeWasmEigenForRun(model, runCase, unitl = 1.0) {
  const state = buildExecState(model, {
    alpha: Number(runCase.inputs.alphaDeg) || 0,
    beta: Number(runCase.inputs.betaDeg) || 0,
    cl: Number(runCase.inputs.cl) || 0,
    vel: Number(runCase.inputs.vel) || 0,
    rho: Number(runCase.inputs.rho) || 0,
    gee: Number(runCase.inputs.gee) || 0,
    bank: Number(runCase.inputs.bankDeg) || 0,
    cd0: Number(runCase.inputs.cd0) || 0,
    xcg: Number(runCase.inputs.xcg) || 0,
    ycg: Number(runCase.inputs.ycg) || 0,
    zcg: Number(runCase.inputs.zcg) || 0,
    cmx: 0,
    cmy: 0,
    cmz: 0,
    unitl,
  });
  applyMassAndRefsToState(state, runCase.inputs);
  buildGeometry(state, model);
  APPGET(state);
  applyRunConstraintsToState(state, model, runCase.constraints);
  applyControlInitialGuesses(state, model, runCase.inputs);

  assert.ok(RUNCHK(state, 1), `run constraints invalid for WASM state (${runCase.name})`);
  EXEC(state, 20, 0, 1);

  const wasm = await loadAmodeWasm();
  const sys = wasm.SYSMAT_wasm(state, 1);
  const eig = wasm.EIGSOL_wasm(state, 1, 1.0e-5, state.__amode.ASYS, sys.NSYS);
  return sortEig(eig.EVAL || []);
}

test('supra eigenmodes parity: all supra.run cases vs Fortran AVL (JS and WASM)', { timeout: 180000 }, async () => {
  if (!fs.existsSync(supraAvlPath)) assert.fail(`missing ${supraAvlPath}`);
  if (!fs.existsSync(supraRunPath)) assert.fail(`missing ${supraRunPath}`);

  const avlText = fs.readFileSync(supraAvlPath, 'utf8');
  const runCases = parseSupraRunCases(fs.readFileSync(supraRunPath, 'utf8'));
  assert.ok(runCases.length > 0, 'no run cases parsed from supra.run');
  const massUnitl = fs.existsSync(supraMassPath)
    ? parseMassUnitScale(fs.readFileSync(supraMassPath, 'utf8'))
    : 1.0;

  const refByRun = computeFortranSupraEigenByRun();
  assert.ok(refByRun.size > 0, 'Fortran AVL produced no supra eigenvalues');

  const model = await buildSolverModel(avlText, { baseDir: runsDir });

  const tolRe = 8e-3;
  const tolIm = 3e-3;
  const mismatches = [];

  for (let runIdx = 1; runIdx <= runCases.length; runIdx += 1) {
    const runCase = runCases[runIdx - 1];
    const refRaw = refByRun.get(runIdx) || [];
    const refEig = sortEig(refRaw);
    assert.ok(refEig.length > 0, `Fortran AVL: missing eigenvalues for run case ${runIdx}`);

    const jsRes = computeJsEigenForRun(model, runCase, massUnitl);
    const jsEig = jsRes.eig;
    const wasmEig = await computeWasmEigenForRun(model, runCase, massUnitl);

    if (jsEig.length !== refEig.length) {
      mismatches.push(`run ${runIdx}: JS eigenvalue count ${jsEig.length} != ref ${refEig.length} (nsys=${jsRes.nsys}, vee=${jsRes.vee}, mass=${jsRes.mass})`);
      continue;
    }
    if (wasmEig.length !== refEig.length) {
      mismatches.push(`run ${runIdx}: WASM eigenvalue count ${wasmEig.length} != ref ${refEig.length}`);
      continue;
    }

    for (let i = 0; i < refEig.length; i += 1) {
      const dr = Math.abs(jsEig[i].re - refEig[i].re);
      const di = Math.abs(jsEig[i].im - refEig[i].im);
      if (dr > tolRe) mismatches.push(`run ${runIdx} JS eig[${i}] re diff ${dr} > ${tolRe}`);
      if (di > tolIm) mismatches.push(`run ${runIdx} JS eig[${i}] im diff ${di} > ${tolIm}`);
    }

    for (let i = 0; i < refEig.length; i += 1) {
      const dr = Math.abs(wasmEig[i].re - refEig[i].re);
      const di = Math.abs(wasmEig[i].im - refEig[i].im);
      if (dr > tolRe) mismatches.push(`run ${runIdx} WASM eig[${i}] re diff ${dr} > ${tolRe}`);
      if (di > tolIm) mismatches.push(`run ${runIdx} WASM eig[${i}] im diff ${di} > ${tolIm}`);
    }
  }

  assert.equal(
    mismatches.length,
    0,
    `supra eigen parity mismatches:\\n${mismatches.join('\\n')}`,
  );
});

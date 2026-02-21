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
import { RUNCHK, SYSMAT } from '../src/amode.js';
import { APPGET } from '../src/amass.js';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const avlBinPath = path.join(repoRoot, 'third_party', 'avl', 'bin', 'avl');
const supraAvlPath = path.join(runsDir, 'supra.avl');
const supraRunPath = path.join(runsDir, 'supra.run');
const supraMassPath = path.join(runsDir, 'supra.mass');
const supraSysRefPath = path.join(repoRoot, 'js', 'test', 'data', 'supra_run1_syssho.txt');

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

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

function parseFortranSysshoMatrix(text, nsys, ncontrol) {
  const lines = String(text || '').split(/\r?\n/);
  const rows = [];
  for (const raw of lines) {
    const nums = raw.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g);
    if (!nums || nums.length < nsys + ncontrol) continue;
    rows.push(nums.slice(0, nsys + ncontrol).map((v) => Number(v.replace(/d/i, 'e'))));
  }
  return rows;
}

function copyDatDependencies(srcDir, dstDir) {
  const entries = fs.readdirSync(srcDir, { withFileTypes: true });
  for (const ent of entries) {
    if (!ent.isFile()) continue;
    if (!ent.name.toLowerCase().endsWith('.dat')) continue;
    fs.copyFileSync(path.join(srcDir, ent.name), path.join(dstDir, ent.name));
  }
}

function computeFortranSupraRun1Syssho({ runOperFirst = false } = {}) {
  if (!fs.existsSync(avlBinPath)) {
    assert.fail(`missing Fortran AVL binary: ${avlBinPath}`);
  }
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'avl-supra-sys-'));
  try {
    fs.copyFileSync(supraAvlPath, path.join(tmpDir, 'supra.avl'));
    fs.copyFileSync(supraRunPath, path.join(tmpDir, 'supra.run'));
    fs.copyFileSync(supraMassPath, path.join(tmpDir, 'supra.mass'));
    copyDatDependencies(runsDir, tmpDir);

    const batch = runOperFirst
      ? [
        'PLOP',
        'G F',
        '',
        'OPER',
        '1',
        'X',
        '',
        'MODE',
        '1',
        'S',
        'supra_sys.txt',
        '',
        '',
        'QUIT',
        '',
      ].join('\n')
      : [
        'PLOP',
        'G F',
        '',
        'MODE',
        '1',
        'S',
        'supra_sys.txt',
        '',
        '',
        'QUIT',
        '',
      ].join('\n');

    const sysPath = path.join(tmpDir, 'supra_sys.txt');
    let runError = null;
    let stdout = '';
    try {
      stdout = String(execFileSync(avlBinPath, ['supra.avl'], {
        cwd: tmpDir,
        input: batch,
        encoding: 'utf8',
        maxBuffer: 1024 * 1024 * 8,
      }) || '');
    } catch (error) {
      runError = error;
      stdout = String(error?.stdout || '');
    }

    if (runError && fs.existsSync(supraSysRefPath)) {
      return { matrixText: fs.readFileSync(supraSysRefPath, 'utf8'), stdout, fromFallback: true };
    }

    if (!fs.existsSync(sysPath)) {
      if (fs.existsSync(supraSysRefPath)) {
        return { matrixText: fs.readFileSync(supraSysRefPath, 'utf8'), stdout, fromFallback: true };
      }
      if (runError) throw runError;
      assert.fail('Fortran AVL did not emit supra_sys.txt');
    }
    return { matrixText: fs.readFileSync(sysPath, 'utf8'), stdout, fromFallback: false };
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

function parseFortranRunCase1Rows(text) {
  const lines = String(text || '').split(/\r?\n/);
  const rows = [];
  for (const line of lines) {
    if (!/^\s*>?\s*1\s+/.test(line)) continue;
    const nums = line.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g);
    if (!nums || nums.length < 8) continue;
    const vals = nums.map((v) => Number(v.replace(/d/i, 'e'))).filter((v) => Number.isFinite(v));
    if (vals.length < 8) continue;
    // row format: run, alpha, beta, CL, CDo, bank, velocity, density, ...
    rows.push({
      alphaDeg: vals[1],
      betaDeg: vals[2],
      cl: vals[3],
      cd0: vals[4],
      bankDeg: vals[5],
      vel: vals[6],
      rho: vals[7],
    });
  }
  return rows;
}

function parseFortranRunCase1ControlTotals(text) {
  const lines = String(text || '').split(/\r?\n/);
  let inCase1 = false;
  let inIter = false;
  let found = false;
  const sum = { flap: 0, aileron: 0, elevator: 0, rudder: 0 };

  for (const line of lines) {
    if (/Run case\s+1\.\.\./.test(line)) {
      inCase1 = true;
      inIter = false;
      continue;
    }
    if (!inCase1) continue;
    if (/^\s*u\s+w\s+q\s+the/.test(line)) break;
    if (/iter d\(alpha\)/.test(line)) {
      inIter = true;
      continue;
    }
    if (!inIter) continue;
    const nums = line.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g);
    if (!nums || nums.length < 10) continue;
    const vals = nums.map((v) => Number(v.replace(/d/i, 'e')));
    if (!vals.every((v) => Number.isFinite(v))) continue;
    found = true;
    sum.flap += vals[6];
    sum.aileron += vals[7];
    sum.elevator += vals[8];
    sum.rudder += vals[9];
  }
  return found ? sum : null;
}

test('supra SYSMAT parity: run case 1 vs Fortran SYSSHO matrix dump', { timeout: 120000 }, async () => {
  const avlText = fs.readFileSync(supraAvlPath, 'utf8');
  const runCases = parseSupraRunCases(fs.readFileSync(supraRunPath, 'utf8'));
  const runCase = runCases[0];
  assert.ok(runCase, 'supra.run: run case 1 missing');
  const massUnitl = parseMassUnitScale(fs.readFileSync(supraMassPath, 'utf8'));

  const model = await buildSolverModel(avlText, { baseDir: runsDir });
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
    unitl: massUnitl,
  });
  applyMassAndRefsToState(state, runCase.inputs);
  buildGeometry(state, model);
  APPGET(state);
  applyRunConstraintsToState(state, model, runCase.constraints);
  applyControlInitialGuesses(state, model, runCase.inputs);
  assert.ok(RUNCHK(state, 1), 'JS state has invalid run constraints');
  EXEC(state, 20, 0, 1);

  const ASYS = new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1));
  const BSYS = new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1));
  const RSYS = new Float32Array(state.JEMAX + 1);
  const sys = SYSMAT(state, 1, ASYS, BSYS, RSYS);
  assert.equal(sys.NSYS, 12, 'expected 12-state dynamic system');

  const fortranMode = computeFortranSupraRun1Syssho({ runOperFirst: true });
  const fortranRows = parseFortranSysshoMatrix(fortranMode.matrixText, sys.NSYS, state.NCONTROL);
  assert.equal(fortranRows.length, sys.NSYS, `Fortran SYSSHO row count mismatch: ${fortranRows.length}`);
  const fortranRunRows = parseFortranRunCase1Rows(fortranMode.stdout);
  const fortranPost = fortranRunRows[fortranRunRows.length - 1] || null;

  const usgn = new Float32Array(sys.NSYS + 1).fill(1.0);
  usgn[state.JEU] = -1.0;
  usgn[state.JEW] = -1.0;
  usgn[state.JEP] = -1.0;
  usgn[state.JER] = -1.0;
  usgn[state.JEX] = -1.0;
  usgn[state.JEZ] = -1.0;

  const tol = 5e-3;
  const mismatches = [];

  if (fortranPost) {
    const alphaDiff = Math.abs((state.ALFA / state.DTR) - fortranPost.alphaDeg);
    const clDiff = Math.abs(Number(state.CLTOT) - Number(fortranPost.cl));
    const velDiff = Math.abs(Number(state.PARVAL[idx2(state.IPVEE, 1, state.IPTOT)] || 0) - fortranPost.vel);
    if (alphaDiff > 2e-3) mismatches.push(`state alpha diff ${alphaDiff}`);
    if (clDiff > 2e-3) mismatches.push(`state CL diff ${clDiff}`);
    if (velDiff > 2e-3) mismatches.push(`state velocity diff ${velDiff}`);
  }
  for (let i = 1; i <= sys.NSYS; i += 1) {
    for (let j = 1; j <= sys.NSYS; j += 1) {
      const jsShow = Number(ASYS[idx2(i, j, state.JEMAX + 1)]) * usgn[i] * usgn[j];
      const f = fortranRows[i - 1][j - 1];
      const d = Math.abs(jsShow - f);
      if (d > tol) mismatches.push(`A(${i},${j}) diff ${d}`);
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      const jsShow = Number(BSYS[idx2(i, n, state.JEMAX + 1)]) * usgn[i];
      const f = fortranRows[i - 1][sys.NSYS + n - 1];
      const d = Math.abs(jsShow - f);
      if (d > tol) mismatches.push(`B(${i},${n}) diff ${d}`);
    }
  }

  assert.equal(mismatches.length, 0, `supra SYSMAT mismatches:\n${mismatches.join('\n')}`);
});

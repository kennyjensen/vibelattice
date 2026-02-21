import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import {
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
} from '../src/exec_pipeline.js';
import {
  EXEC,
  preloadAoperLinSolveWasm,
  preloadAsetupWasm,
  preloadAeroWasm,
  preloadAicWasmBridge,
  preloadAsetpLuWasmBridge,
} from '../src/aoper.js';
import { APPGET } from '../src/amass.js';
import { ensureRefBuilt } from './ref_build_lock.mjs';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const refBin = path.join(refDir, 'supra_exec_ref');
const supraAvlPath = path.join(runsDir, 'supra.avl');
const supraRunPath = path.join(runsDir, 'supra.run');
const supraMassPath = path.join(runsDir, 'supra.mass');

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

function normalizeControlNameToken(text) {
  return normalizeRunKey(text).replace(/^d\d+\s+/, '').trim();
}

function variableTokenToKey(raw) {
  const norm = normalizeRunKey(raw);
  if (norm === 'alpha') return 'alpha';
  if (norm === 'beta') return 'beta';
  if (norm === 'pb/2v') return 'p';
  if (norm === 'qc/2v') return 'q';
  if (norm === 'rb/2v') return 'r';
  return `ctrl:${normalizeControlNameToken(raw)}`;
}

function constraintTokenToKey(raw) {
  const norm = normalizeRunKey(raw);
  if (norm === 'alpha') return 'alpha';
  if (norm === 'beta') return 'beta';
  if (norm === 'pb/2v') return 'p';
  if (norm === 'qc/2v') return 'q';
  if (norm === 'rb/2v') return 'r';
  if (norm === 'cl') return 'cl';
  if (norm === 'cy') return 'cy';
  if (norm === 'cl roll mom') return 'cmx';
  if (norm === 'cm pitchmom') return 'cmy';
  if (norm === 'cn yaw mom') return 'cmz';
  return `ctrl:${normalizeControlNameToken(raw)}`;
}

function parseSupraRunCases(text) {
  const lines = String(text || '').split(/\r?\n/);
  const cases = [];
  let current = null;

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
      const variable = variableTokenToKey(leftRaw);
      const constraint = constraintTokenToKey(rightRaw);
      current.constraints.push({ variable, constraint, numeric: num });

      // For directly constrained variables, preserve the commanded value
      // from the arrow row (not the stale solved-value block below).
      if (variable === 'alpha') current.inputs.alphaDeg = num;
      else if (variable === 'beta') current.inputs.betaDeg = num;
      else if (variable === 'p') current.inputs.pb2v = num;
      else if (variable === 'q') current.inputs.qc2v = num;
      else if (variable === 'r') current.inputs.rb2v = num;
      else if (variable.startsWith('ctrl:')) {
        current.inputs.controlValues[variable.slice(5)] = num;
      }
      continue;
    }

    const valueMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*(.*?)\s*$/);
    if (!valueMatch) continue;
    const keyNorm = normalizeRunKey(valueMatch[1].trim());
    const value = Number(valueMatch[2]);
    if (!Number.isFinite(value)) continue;

    if (keyNorm === 'alpha' && !hasMappedVariable('alpha')) current.inputs.alphaDeg = value;
    else if (keyNorm === 'beta' && !hasMappedVariable('beta')) current.inputs.betaDeg = value;
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
    else if (keyNorm === 'ixy') current.inputs.ixy = value;
    else if (keyNorm === 'iyz') current.inputs.iyz = value;
    else if (keyNorm === 'izx') current.inputs.izx = value;
    else if (!hasMappedVariable(`ctrl:${keyNorm}`)) current.inputs.controlValues[keyNorm] = value;

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
  const controlIndex = new Map();
  if (model?.controlMap) {
    for (const [name, idx] of model.controlMap.entries()) {
      const n = idx;
      controlIndex.set(name, n);
      controlIndex.set(normalizeRunKey(name), n);
    }
  }

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
      const name = normalizeRunKey(String(row.constraint).slice(5));
      const n = controlIndex.get(name);
      if (n) ic = state.ICTOT + n;
    }
    if (!ic) continue;
    state.CONVAL[idx2(ic, 1, state.ICMAX)] = Math.fround(Number(row.numeric) || 0);

    let iv = variableMap[row.variable];
    if (!iv && String(row.variable || '').startsWith('ctrl:')) {
      const name = normalizeRunKey(String(row.variable).slice(5));
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

function parseRefOutput(stdout) {
  const out = {
    scalar: {},
    vec3: {},
    urows: [],
    drows: [],
    surfRows: [],
    stripRows: [],
    ncontrol: 0,
    nsurf: 0,
    nstrip: 0,
  };
  const lines = String(stdout || '').split(/\r?\n/);
  const toNums = (line) => {
    const re = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g;
    return (line.match(re) || []).map((v) => Number(v.replace(/[dD]/g, 'e')));
  };
  const tokenNum = (token) => Number(String(token || '').replace(/[dD]/g, 'e'));

  for (const raw of lines) {
    const line = raw.trim();
    if (!line) continue;
    const parts = line.split(/\s+/);
    const tag = parts[0];
    if (tag === 'SCALAR' && parts.length >= 3) {
      const nums = toNums(line);
      out.scalar[parts[1]] = nums[0];
    } else if (tag === 'VEC3' && parts.length >= 3) {
      out.vec3[parts[1]] = [tokenNum(parts[2]), tokenNum(parts[3]), tokenNum(parts[4])];
    } else if (tag === 'NSURF') {
      out.nsurf = toNums(line)[0] | 0;
    } else if (tag === 'NCONTROL') {
      out.ncontrol = toNums(line)[0] | 0;
    } else if (tag === 'NSTRIP') {
      out.nstrip = toNums(line)[0] | 0;
    } else if (tag === 'UROW') {
      const nums = toNums(line);
      const k = nums[0] | 0;
      out.urows[k] = {
        cl: nums[1], cd: nums[2], cy: nums[3],
        cfx: nums[4], cfy: nums[5], cfz: nums[6],
        cmx: nums[7], cmy: nums[8], cmz: nums[9],
      };
    } else if (tag === 'DROW') {
      const nums = toNums(line);
      const k = nums[0] | 0;
      out.drows[k] = {
        cl: nums[1], cd: nums[2], cy: nums[3],
        cfx: nums[4], cfy: nums[5], cfz: nums[6],
        cmx: nums[7], cmy: nums[8], cmz: nums[9],
        chinge: nums[10], delcon: nums[11],
      };
    } else if (tag === 'SURFROW') {
      const nums = toNums(line);
      const is = nums[0] | 0;
      out.surfRows[is] = {
        cl: nums[1], cd: nums[2], cy: nums[3], cdv: nums[4],
        cfx: nums[5], cfy: nums[6], cfz: nums[7],
        cmx: nums[8], cmy: nums[9], cmz: nums[10],
      };
    } else if (tag === 'STRIPROW') {
      const nums = toNums(line);
      const j = nums[0] | 0;
      out.stripRows[j] = {
        y: nums[1], z: nums[2], cl: nums[3], cd: nums[4], cy: nums[5],
        cnc: nums[6], cla: nums[7], clt: nums[8], dw: nums[9],
        off: nums[10] | 0,
      };
    }
  }
  return out;
}

function assertNear(actual, expected, tol, label) {
  const diff = Math.abs(actual - expected);
  assert.ok(diff <= tol, `${label} diff ${diff} > ${tol} (got=${actual}, exp=${expected})`);
}

function computeStateXnp(state) {
  const dir = state.LNASA_SA ? -1.0 : 1.0;
  const ca = Math.cos(Number(state.ALFA || 0.0));
  const sa = Math.sin(Number(state.ALFA || 0.0));
  const w0 = Number(state.WROT?.[0] || 0.0);
  const w2 = Number(state.WROT?.[2] || 0.0);
  const rx = (w0 * ca + w2 * sa) * dir;
  const rz = (w2 * ca - w0 * sa) * dir;
  const wrotA = [-rx * sa - rz * ca, 0.0, -rz * sa + rx * ca];
  const cla = Number(state.CLTOT_U?.[0] || 0) * Number(state.VINF_A?.[0] || 0)
    + Number(state.CLTOT_U?.[1] || 0) * Number(state.VINF_A?.[1] || 0)
    + Number(state.CLTOT_U?.[2] || 0) * Number(state.VINF_A?.[2] || 0)
    + Number(state.CLTOT_U?.[3] || 0) * wrotA[0]
    + Number(state.CLTOT_U?.[4] || 0) * wrotA[1]
    + Number(state.CLTOT_U?.[5] || 0) * wrotA[2]
    + Number(state.CLTOT_A || 0);
  const cma = Number(state.CMTOT_U?.[idx2(1, 0, 3)] || 0) * Number(state.VINF_A?.[0] || 0)
    + Number(state.CMTOT_U?.[idx2(1, 1, 3)] || 0) * Number(state.VINF_A?.[1] || 0)
    + Number(state.CMTOT_U?.[idx2(1, 2, 3)] || 0) * Number(state.VINF_A?.[2] || 0)
    + Number(state.CMTOT_U?.[idx2(1, 3, 3)] || 0) * wrotA[0]
    + Number(state.CMTOT_U?.[idx2(1, 4, 3)] || 0) * wrotA[1]
    + Number(state.CMTOT_U?.[idx2(1, 5, 3)] || 0) * wrotA[2];
  if (!Number.isFinite(cla) || Math.abs(cla) < 1e-12 || !Number.isFinite(cma)) return Number.NaN;
  return Number(state.XYZREF?.[0] || 0) - Number(state.CREF || 0) * (cma / cla);
}

function applySupraCaseState(state, model, runCase) {
  const ir = 1;
  const inp = runCase.inputs;
  state.ALFA = Number(inp.alphaDeg || 0) * state.DTR;
  state.BETA = Number(inp.betaDeg || 0) * state.DTR;
  state.MACH = 0.0;
  state.AMACH = 0.0;

  state.PARVAL[idx2(state.IPMACH, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPVEE, ir, state.IPTOT)] = Number(inp.vel || 0);
  state.PARVAL[idx2(state.IPRHO, ir, state.IPTOT)] = Number(inp.rho || 0);
  state.PARVAL[idx2(state.IPGEE, ir, state.IPTOT)] = Number(inp.gee || 0);
  state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)] = Number(inp.cl || 0);
  state.PARVAL[idx2(state.IPPHI, ir, state.IPTOT)] = Number(inp.bankDeg || 0);
  state.PARVAL[idx2(state.IPCD0, ir, state.IPTOT)] = Number(inp.cd0 || 0);

  applyMassAndRefsToState(state, inp);
  applyRunConstraintsToState(state, model, runCase.constraints);
  applyControlInitialGuesses(state, model, inp);
}

function runSingleCase(model, caseDef, unitl, wasmEnabled) {
  const state = buildExecState(model, {
    alpha: 0,
    beta: 0,
    cl: 0,
    vel: 1,
    rho: 1,
    gee: 1,
    bank: 0,
    cd0: 0,
    xcg: 0,
    ycg: 0,
    zcg: 0,
    cmx: 0,
    cmy: 0,
    cmz: 0,
    unitl,
    massLoaded: true,
  });
  applySupraCaseState(state, model, caseDef);
  state.LNASA_SA = false;
  buildGeometry(state, model);
  APPGET(state);

  state.USE_WASM_SOLVE = Boolean(wasmEnabled);
  state.USE_WASM_GAM = Boolean(wasmEnabled);
  state.USE_WASM_AERO = Boolean(wasmEnabled);
  state.USE_WASM_AIC = Boolean(wasmEnabled);
  state.USE_WASM_LU = Boolean(wasmEnabled);
  EXEC(state, 20, 0, 1);
  return state;
}

function compareAllOutputs(state, ref, modeLabel) {
  const tolForce = 4e-3;
  const tolDeriv = 1.5e-2;
  const tolSurf = 3e-3;
  const tolStrip = 4e-3;
  const tolSmall = 1e-5;
  const tolXnp = 2e-2;

  assert.equal(state.NSURF, ref.nsurf, `${modeLabel}: NSURF`);
  assert.equal(state.NCONTROL, ref.ncontrol, `${modeLabel}: NCONTROL`);
  assert.equal(state.NSTRIP, ref.nstrip, `${modeLabel}: NSTRIP`);

  const scalarMap = [
    ['ALFA', state.ALFA, tolDeriv],
    ['BETA', state.BETA, tolDeriv],
    ['MACH', state.MACH, tolSmall],
    ['CLTOT', state.CLTOT, tolForce],
    ['CDTOT', state.CDTOT, tolForce],
    ['CYTOT', state.CYTOT, tolForce],
    ['CDVTOT', state.CDVTOT, tolForce],
    ['SPANEF', state.SPANEF, 3e-3],
    ['SREF', state.SREF, tolSmall],
    ['CREF', state.CREF, tolSmall],
    ['BREF', state.BREF, tolSmall],
    ['XNP', computeStateXnp(state), tolXnp],
  ];
  scalarMap.forEach(([key, value, tol]) => {
    assertNear(Number(value), Number(ref.scalar[key]), tol, `${modeLabel}: scalar ${key}`);
  });

  const vecMap = [
    ['WROT', state.WROT],
    ['CFTOT', state.CFTOT],
    ['CMTOT', state.CMTOT],
  ];
  vecMap.forEach(([key, arr]) => {
    const exp = ref.vec3[key];
    assert.ok(exp, `${modeLabel}: missing ref vec ${key}`);
    for (let i = 0; i < 3; i += 1) {
      const tol = key === 'WROT' ? tolDeriv : tolForce;
      assertNear(Number(arr[i]), Number(exp[i]), tol, `${modeLabel}: ${key}[${i}]`);
    }
  });

  for (let k = 1; k <= 6; k += 1) {
    const u = ref.urows[k];
    assert.ok(u, `${modeLabel}: missing ref UROW ${k}`);
    const j = k - 1;
    assertNear(Number(state.CLTOT_U[j]), u.cl, tolDeriv, `${modeLabel}: UROW ${k} CL`);
    assertNear(Number(state.CDTOT_U[j]), u.cd, tolDeriv, `${modeLabel}: UROW ${k} CD`);
    assertNear(Number(state.CYTOT_U[j]), u.cy, tolDeriv, `${modeLabel}: UROW ${k} CY`);
    assertNear(Number(state.CFTOT_U[idx2(0, j, 3)]), u.cfx, tolDeriv, `${modeLabel}: UROW ${k} CFX`);
    assertNear(Number(state.CFTOT_U[idx2(1, j, 3)]), u.cfy, tolDeriv, `${modeLabel}: UROW ${k} CFY`);
    assertNear(Number(state.CFTOT_U[idx2(2, j, 3)]), u.cfz, tolDeriv, `${modeLabel}: UROW ${k} CFZ`);
    assertNear(Number(state.CMTOT_U[idx2(0, j, 3)]), u.cmx, tolDeriv, `${modeLabel}: UROW ${k} CMX`);
    assertNear(Number(state.CMTOT_U[idx2(1, j, 3)]), u.cmy, tolDeriv, `${modeLabel}: UROW ${k} CMY`);
    assertNear(Number(state.CMTOT_U[idx2(2, j, 3)]), u.cmz, tolDeriv, `${modeLabel}: UROW ${k} CMZ`);
  }

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    const d = ref.drows[n];
    assert.ok(d, `${modeLabel}: missing ref DROW ${n}`);
    const j = n - 1;
    assertNear(Number(state.CLTOT_D[j]), d.cl, tolDeriv, `${modeLabel}: DROW ${n} CL`);
    assertNear(Number(state.CDTOT_D[j]), d.cd, tolDeriv, `${modeLabel}: DROW ${n} CD`);
    assertNear(Number(state.CYTOT_D[j]), d.cy, tolDeriv, `${modeLabel}: DROW ${n} CY`);
    assertNear(Number(state.CFTOT_D[idx2(0, j, 3)]), d.cfx, tolDeriv, `${modeLabel}: DROW ${n} CFX`);
    assertNear(Number(state.CFTOT_D[idx2(1, j, 3)]), d.cfy, tolDeriv, `${modeLabel}: DROW ${n} CFY`);
    assertNear(Number(state.CFTOT_D[idx2(2, j, 3)]), d.cfz, tolDeriv, `${modeLabel}: DROW ${n} CFZ`);
    assertNear(Number(state.CMTOT_D[idx2(0, j, 3)]), d.cmx, tolDeriv, `${modeLabel}: DROW ${n} CMX`);
    assertNear(Number(state.CMTOT_D[idx2(1, j, 3)]), d.cmy, tolDeriv, `${modeLabel}: DROW ${n} CMY`);
    assertNear(Number(state.CMTOT_D[idx2(2, j, 3)]), d.cmz, tolDeriv, `${modeLabel}: DROW ${n} CMZ`);
    assertNear(Number(state.CHINGE[n - 1]), d.chinge, tolDeriv, `${modeLabel}: DROW ${n} CHINGE`);
    assertNear(Number(state.DELCON[n]), d.delcon, tolDeriv, `${modeLabel}: DROW ${n} DELCON`);
  }

  for (let is = 1; is <= state.NSURF; is += 1) {
    const s = ref.surfRows[is];
    assert.ok(s, `${modeLabel}: missing ref SURFROW ${is}`);
    assertNear(Number(state.CLSURF[is]), s.cl, tolSurf, `${modeLabel}: SURF ${is} CL`);
    assertNear(Number(state.CDSURF[is]), s.cd, tolSurf, `${modeLabel}: SURF ${is} CD`);
    assertNear(Number(state.CYSURF[is]), s.cy, tolSurf, `${modeLabel}: SURF ${is} CY`);
    assertNear(Number(state.CDVSURF[is]), s.cdv, tolSurf, `${modeLabel}: SURF ${is} CDV`);
    assertNear(Number(state.CFSURF[idx2(0, is, 3)]), s.cfx, tolSurf, `${modeLabel}: SURF ${is} CFX`);
    assertNear(Number(state.CFSURF[idx2(1, is, 3)]), s.cfy, tolSurf, `${modeLabel}: SURF ${is} CFY`);
    assertNear(Number(state.CFSURF[idx2(2, is, 3)]), s.cfz, tolSurf, `${modeLabel}: SURF ${is} CFZ`);
    assertNear(Number(state.CMSURF[idx2(0, is, 3)]), s.cmx, tolSurf, `${modeLabel}: SURF ${is} CMX`);
    assertNear(Number(state.CMSURF[idx2(1, is, 3)]), s.cmy, tolSurf, `${modeLabel}: SURF ${is} CMY`);
    assertNear(Number(state.CMSURF[idx2(2, is, 3)]), s.cmz, tolSurf, `${modeLabel}: SURF ${is} CMZ`);
  }

  for (let j = 1; j <= state.NSTRIP; j += 1) {
    const s = ref.stripRows[j];
    assert.ok(s, `${modeLabel}: missing ref STRIPROW ${j}`);
    assertNear(Number(state.RLE[idx2(2, j, 4)]), s.y, tolStrip, `${modeLabel}: STRIP ${j} y`);
    assertNear(Number(state.RLE[idx2(3, j, 4)]), s.z, tolStrip, `${modeLabel}: STRIP ${j} z`);
    assertNear(Number(state.CLSTRP[j]), s.cl, tolStrip, `${modeLabel}: STRIP ${j} CL`);
    assertNear(Number(state.CDSTRP[j]), s.cd, tolStrip, `${modeLabel}: STRIP ${j} CD`);
    assertNear(Number(state.CYSTRP[j]), s.cy, tolStrip, `${modeLabel}: STRIP ${j} CY`);
    assertNear(Number(state.CNC[j]), s.cnc, tolStrip, `${modeLabel}: STRIP ${j} CNC`);
    assertNear(Number(state.CLA_LSTRP[j]), s.cla, tolStrip, `${modeLabel}: STRIP ${j} CLA`);
    assertNear(Number(state.CLT_LSTRP[j]), s.clt, tolStrip, `${modeLabel}: STRIP ${j} CLT`);
    assertNear(Number(state.DWWAKE[j]), s.dw, tolStrip, `${modeLabel}: STRIP ${j} DWWAKE`);
    const off = state.LSTRIPOFF[j] ? 1 : 0;
    assert.equal(off, s.off, `${modeLabel}: STRIP ${j} OFF`);
  }
}

async function preloadWasm() {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();
}

function getConstraint(runCase, key, fallback = 0) {
  const row = (runCase.constraints || []).find((r) => r.constraint === key);
  return Number.isFinite(Number(row?.numeric)) ? Number(row.numeric) : fallback;
}

function buildControlIndexMap(model) {
  const map = new Map();
  if (!model?.controlMap) return map;
  for (const [name, idx] of model.controlMap.entries()) {
    map.set(normalizeRunKey(name), idx);
  }
  return map;
}

function constraintKeyToCode(constraint, controlIndex) {
  if (constraint === 'alpha') return 1;
  if (constraint === 'beta') return 2;
  if (constraint === 'p') return 3;
  if (constraint === 'q') return 4;
  if (constraint === 'r') return 5;
  if (constraint === 'cl') return 6;
  if (constraint === 'cy') return 7;
  if (constraint === 'cmx') return 8;
  if (constraint === 'cmy') return 9;
  if (constraint === 'cmz') return 10;
  if (String(constraint || '').startsWith('ctrl:')) {
    const key = normalizeRunKey(String(constraint).slice(5));
    const n = controlIndex.get(key);
    if (n) return 100 + n;
  }
  return 0;
}

function getVariableConstraintCode(caseDef, variable, controlIndex, fallback) {
  const row = (caseDef.constraints || []).find((r) => r.variable === variable);
  const c = constraintKeyToCode(row?.constraint, controlIndex);
  return c || fallback;
}

test('supra run-cases full output parity against Fortran (JS and WASM EXEC paths)', { timeout: 300000 }, async () => {
  await ensureRefBuilt('supra_exec_ref', refDir);
  await preloadWasm();

  const runCases = parseSupraRunCases(fs.readFileSync(supraRunPath, 'utf8'));
  const massUnitl = parseMassUnitScale(fs.readFileSync(supraMassPath, 'utf8'));
  assert.ok(runCases.length > 0, 'no run cases parsed from supra.run');

  const model = await buildSolverModel(fs.readFileSync(supraAvlPath, 'utf8'), { baseDir: runsDir });
  const controlIndex = buildControlIndexMap(model);

  for (let i = 0; i < runCases.length; i += 1) {
    const caseDef = runCases[i];
    const inp = caseDef.inputs;
    const args = [
      String(inp.alphaDeg || 0),
      String(inp.betaDeg || 0),
      String(inp.cl || 0),
      String(inp.cd0 || 0),
      String(inp.bankDeg || 0),
      String(inp.vel || 0),
      String(inp.rho || 0),
      String(inp.gee || 0),
      String(inp.xcg || 0),
      String(inp.ycg || 0),
      String(inp.zcg || 0),
      String(inp.mass || 0),
      String(inp.ixx || 0),
      String(inp.iyy || 0),
      String(inp.izz || 0),
      String(inp.ixy || 0),
      String(inp.iyz || 0),
      String(inp.izx || 0),
      String(getConstraint(caseDef, 'p', 0)),
      String(getConstraint(caseDef, 'q', 0)),
      String(getConstraint(caseDef, 'r', 0)),
      String(getConstraint(caseDef, 'ctrl:flap', 0)),
      String(getConstraint(caseDef, 'cmx', 0)),
      String(getConstraint(caseDef, 'cmy', 0)),
      String(getConstraint(caseDef, 'cmz', 0)),
      String(getVariableConstraintCode(caseDef, 'alpha', controlIndex, 1)),
      String(getVariableConstraintCode(caseDef, 'beta', controlIndex, 2)),
      String(getVariableConstraintCode(caseDef, 'p', controlIndex, 3)),
      String(getVariableConstraintCode(caseDef, 'q', controlIndex, 4)),
      String(getVariableConstraintCode(caseDef, 'r', controlIndex, 5)),
      String(getVariableConstraintCode(caseDef, 'ctrl:flap', controlIndex, 101)),
      String(getVariableConstraintCode(caseDef, 'ctrl:aileron', controlIndex, 8)),
      String(getVariableConstraintCode(caseDef, 'ctrl:elevator', controlIndex, 9)),
      String(getVariableConstraintCode(caseDef, 'ctrl:rudder', controlIndex, 10)),
    ];

    const refProc = spawnSync(refBin, [supraAvlPath, '20', ...args], {
      encoding: 'utf8',
      cwd: runsDir,
    });
    if (refProc.error) throw refProc.error;
    if (refProc.status !== 0) {
      throw new Error(refProc.stderr || refProc.stdout || `supra_exec_ref exited with ${refProc.status}`);
    }
    const ref = parseRefOutput(refProc.stdout);
    const jsState = runSingleCase(model, caseDef, massUnitl, false);
    compareAllOutputs(jsState, ref, `run ${i + 1} JS`);

    const wasmState = runSingleCase(model, caseDef, massUnitl, true);
    compareAllOutputs(wasmState, ref, `run ${i + 1} WASM`);
  }
});

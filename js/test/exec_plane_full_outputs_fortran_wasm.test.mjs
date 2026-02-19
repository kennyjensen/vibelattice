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
import { ensureRefBuilt } from './ref_build_lock.mjs';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
const refDir = path.join(repoRoot, 'third_party', 'avl', 'ref');
const planePath = path.join(runsDir, 'plane.avl');
const runPath = path.join(runsDir, 'plane.run');
const massPath = path.join(runsDir, 'plane.mass');
const refBin = path.join(refDir, 'plane_exec_ref');

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function toNums(line) {
  const re = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g;
  return (line.match(re) || []).map((v) => Number(v.replace(/[dD]/g, 'e')));
}

function tokenNum(token) {
  return Number(String(token || '').replace(/[dD]/g, 'e'));
}

function parsePlaneRun(text) {
  const out = {
    constraints: [],
    values: {},
  };
  const lines = String(text || '').split(/\r?\n/);
  for (const raw of lines) {
    const line = String(raw || '');
    const arrow = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*->\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*$/);
    if (arrow) {
      out.constraints.push({
        variable: arrow[1].trim(),
        target: arrow[2].trim(),
        value: Number(arrow[3]),
      });
      continue;
    }
    const eq = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\b/);
    if (!eq) continue;
    const key = eq[1].trim().toLowerCase().replace(/\s+/g, ' ');
    out.values[key] = Number(eq[2]);
  }
  return out;
}

function parseMassFile(text) {
  const lines = String(text || '').split(/\r?\n/);
  const scale = new Array(10).fill(1);
  const add = new Array(10).fill(0);
  let row = null;
  const parseNums = (line) => String(line || '').trim().split(/\s+/).map((t) => Number(t)).filter((n) => Number.isFinite(n));
  for (const raw of lines) {
    const line = String(raw || '').replace(/!.*/, '').replace(/#.*/, '').trim();
    if (!line) continue;
    if (line.startsWith('*')) {
      const vals = parseNums(line.slice(1));
      vals.forEach((v, i) => { if (i < 10) scale[i] = v; });
      continue;
    }
    if (line.startsWith('+')) {
      const vals = parseNums(line.slice(1));
      vals.forEach((v, i) => { if (i < 10) add[i] = v; });
      continue;
    }
    const vals = parseNums(line);
    if (vals.length < 7) continue;
    const rawVals = new Array(10).fill(0);
    vals.slice(0, 10).forEach((v, i) => { rawVals[i] = v; });
    row = rawVals.map((v, i) => add[i] + scale[i] * v);
    break;
  }
  if (!row) throw new Error('failed to parse plane.mass row');
  return {
    mass: row[0],
    xcg: row[1],
    ycg: row[2],
    zcg: row[3],
    ixx: row[4],
    iyy: row[5],
    izz: row[6],
    ixy: row[7],
    ixz: row[8],
    iyz: row[9],
  };
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

function applyRunCaseConstraints(state, model, run) {
  const baseVar = {
    alpha: state.IVALFA,
    beta: state.IVBETA,
    'pb/2v': state.IVROTX,
    'qc/2v': state.IVROTY,
    'rb/2v': state.IVROTZ,
  };
  const baseTarget = {
    alpha: state.ICALFA,
    beta: state.ICBETA,
    'pb/2v': state.ICROTX,
    'qc/2v': state.ICROTY,
    'rb/2v': state.ICROTZ,
    cl: state.ICCL,
    cy: state.ICCY,
    'cl roll mom': state.ICMOMX,
    'cm pitchmom': state.ICMOMY,
    'cn yaw mom': state.ICMOMZ,
  };
  const controlMap = model.controlMap ? new Map(Array.from(model.controlMap.keys()).map((k, i) => [k.toLowerCase(), i + 1])) : new Map();
  const norm = (s) => String(s || '').trim().toLowerCase().replace(/\s+/g, ' ');

  for (const row of run.constraints) {
    const v = norm(row.variable);
    const t = norm(row.target);
    let iv = baseVar[v];
    if (!iv && controlMap.has(v)) iv = state.IVTOT + controlMap.get(v);
    let ic = baseTarget[t];
    if (!ic && controlMap.has(t)) ic = state.ICTOT + controlMap.get(t);
    if (!iv || !ic) continue;
    state.ICON[idx2(iv, 1, state.IVMAX)] = ic;
    state.CONVAL[idx2(ic, 1, state.ICMAX)] = row.value;
  }
}

function applyPlaneCaseState(state, model, run, mass) {
  const v = run.values;
  const get = (key, fallback = 0) => {
    const num = Number(v[key]);
    return Number.isFinite(num) ? num : fallback;
  };
  const ir = 1;
  state.ALFA = get('alpha') * state.DTR;
  state.BETA = get('beta') * state.DTR;
  state.MACH = get('mach');
  state.AMACH = state.MACH;
  state.PARVAL[idx2(state.IPMACH, ir, state.IPTOT)] = state.MACH;
  state.PARVAL[idx2(state.IPVEE, ir, state.IPTOT)] = get('velocity');
  state.PARVAL[idx2(state.IPRHO, ir, state.IPTOT)] = get('density');
  state.PARVAL[idx2(state.IPGEE, ir, state.IPTOT)] = get('grav.acc.', 1);
  state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)] = get('cl');
  state.PARVAL[idx2(state.IPPHI, ir, state.IPTOT)] = get('bank');
  state.PARVAL[idx2(state.IPCD0, ir, state.IPTOT)] = get('cdo');
  state.PARVAL[idx2(state.IPXCG, ir, state.IPTOT)] = Number.isFinite(mass?.xcg) ? mass.xcg : get('x_cg');
  state.PARVAL[idx2(state.IPYCG, ir, state.IPTOT)] = Number.isFinite(mass?.ycg) ? mass.ycg : get('y_cg');
  state.PARVAL[idx2(state.IPZCG, ir, state.IPTOT)] = Number.isFinite(mass?.zcg) ? mass.zcg : get('z_cg');
  state.PARVAL[idx2(state.IPMASS, ir, state.IPTOT)] = Number.isFinite(mass?.mass) ? mass.mass : get('mass', 1);
  state.PARVAL[idx2(state.IPIXX, ir, state.IPTOT)] = Number.isFinite(mass?.ixx) ? mass.ixx : get('ixx');
  state.PARVAL[idx2(state.IPIYY, ir, state.IPTOT)] = Number.isFinite(mass?.iyy) ? mass.iyy : get('iyy');
  state.PARVAL[idx2(state.IPIZZ, ir, state.IPTOT)] = Number.isFinite(mass?.izz) ? mass.izz : get('izz');
  state.PARVAL[idx2(state.IPIXY, ir, state.IPTOT)] = Number.isFinite(mass?.ixy) ? -mass.ixy : -get('ixy');
  state.PARVAL[idx2(state.IPIYZ, ir, state.IPTOT)] = Number.isFinite(mass?.iyz) ? -mass.iyz : -get('iyz');
  state.PARVAL[idx2(state.IPIZX, ir, state.IPTOT)] = Number.isFinite(mass?.ixz) ? -mass.ixz : -get('izx');
  state.LMASS = true;
  applyRunCaseConstraints(state, model, run);
}

function runCase(model, run, mass, wasmEnabled) {
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
    massLoaded: true,
  });
  applyPlaneCaseState(state, model, run, mass);
  state.LNASA_SA = false;
  buildGeometry(state, model);
  state.USE_WASM_SOLVE = Boolean(wasmEnabled);
  state.USE_WASM_GAM = Boolean(wasmEnabled);
  state.USE_WASM_AERO = Boolean(wasmEnabled);
  state.USE_WASM_AIC = Boolean(wasmEnabled);
  state.USE_WASM_LU = Boolean(wasmEnabled);
  EXEC(state, 20, 0, 1);
  return state;
}

function compareAllOutputs(state, ref, modeLabel) {
  const tolForce = 3e-4;
  const tolDeriv = 8e-3;
  const tolSurf = 5e-4;
  const tolStrip = 2e-3;
  const tolSmall = 5e-6;

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
    ['SPANEF', state.SPANEF, 2e-3],
    ['SREF', state.SREF, tolSmall],
    ['CREF', state.CREF, tolSmall],
    ['BREF', state.BREF, tolSmall],
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

test('plane.avl/.run/.mass full output parity against Fortran (JS and WASM EXEC paths)', { timeout: 120000 }, async () => {
  await ensureRefBuilt('plane_exec_ref', refDir);
  await preloadWasm();

  const runText = fs.readFileSync(runPath, 'utf8');
  const massText = fs.readFileSync(massPath, 'utf8');
  const run = parsePlaneRun(runText);
  const mass = parseMassFile(massText);
  const model = await buildSolverModel(fs.readFileSync(planePath, 'utf8'), {});

  const refProc = spawnSync(refBin, [planePath], { encoding: 'utf8' });
  if (refProc.error) throw refProc.error;
  if (refProc.status !== 0) {
    throw new Error(refProc.stderr || refProc.stdout || `ref exited with ${refProc.status}`);
  }
  const ref = parseRefOutput(refProc.stdout);

  const jsState = runCase(model, run, mass, false);
  compareAllOutputs(jsState, ref, 'JS');

  const wasmState = runCase(model, run, mass, true);
  compareAllOutputs(wasmState, ref, 'WASM');
});

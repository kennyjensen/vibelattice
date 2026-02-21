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
const refBin = path.join(refDir, 'b737_exec_ref');

const CASES = [
  { file: 'b737.avl', tol: { force: 5e-3, moment: 5e-3, stripGeom: 5e-2, stripAero: 5e-2, xnp: 1e-2 } },
  { file: 'allegro.avl', tol: { force: 4e-2, moment: 7e-2, stripGeom: 1e-4, stripAero: 1.5, xnp: 4e-1 } },
  { file: 'circle.avl', tol: { force: 5e-4, moment: 1e-3, stripGeom: 1e-4, stripAero: 2e-3, xnp: 5e-3 } },
  { file: 'hershey.avl', tol: { force: 5e-4, moment: 1e-3, stripGeom: 1e-4, stripAero: 2e-3, xnp: 5e-3 } },
  { file: 'ellip.avl', tol: { force: 1e-3, moment: 2e-3, stripGeom: 1e-4, stripAero: 2e-3, xnp: 1e-2 } },
];

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function toNums(line) {
  const re = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g;
  return (line.match(re) || []).map((v) => Number(v.replace(/[dD]/g, 'e')));
}

function parseRefOutput(stdout) {
  const out = {
    force: null,
    cdvtot: Number.NaN,
    xnp: Number.NaN,
    cla: Number.NaN,
    cma: Number.NaN,
    nvor: 0,
    nstrip: 0,
    strips: [],
  };
  const lines = String(stdout || '').split(/\r?\n/);
  for (const raw of lines) {
    const line = raw.trim();
    if (!line) continue;
    const parts = line.split(/\s+/);
    const tag = parts[0];
    if (tag === 'FORCE') {
      const nums = toNums(line);
      if (nums.length >= 9) {
        out.force = {
          cl: nums[0], cd: nums[1], cy: nums[2],
          cmx: nums[3], cmy: nums[4], cmz: nums[5],
          cfx: nums[6], cfy: nums[7], cfz: nums[8],
        };
      }
    } else if (tag === 'CDVTOT') {
      const nums = toNums(line);
      if (nums.length) out.cdvtot = nums[0];
    } else if (tag === 'XNP') {
      const nums = toNums(line);
      if (nums.length) out.xnp = nums[0];
      if (nums.length > 1) out.cla = nums[1];
      if (nums.length > 2) out.cma = nums[2];
    } else if (tag === 'NVOR') {
      const nums = toNums(line);
      if (nums.length) out.nvor = nums[0] | 0;
    } else if (tag === 'NSTRIP') {
      const nums = toNums(line);
      if (nums.length) out.nstrip = nums[0] | 0;
    } else if (tag === 'STRIP') {
      const nums = toNums(line);
      if (nums.length >= 8) {
        const j = nums[0] | 0;
        out.strips[j] = {
          y: nums[1],
          z: nums[2],
          cnc: nums[3],
          cla: nums[4],
          clt: nums[5],
          dwwake: nums[6],
          off: nums[7] ? 1 : 0,
        };
      }
    }
  }
  return out;
}

function assertNear(actual, expected, tol, label) {
  const diff = Math.abs(actual - expected);
  assert.ok(diff <= tol, `${label} diff ${diff} > ${tol} (got=${actual}, exp=${expected})`);
}

function applyB737ReferenceCaseState(state) {
  const ir = 1;
  state.LNASA_SA = false;
  state.LSA_RATES = false;
  state.LVISC = false;
  state.LBFORCE = false;

  state.MACH = 0.78;
  state.AMACH = 0.78;
  state.ALFA = 2.0 * state.DTR;
  state.BETA = 0.0;

  state.PARVAL[idx2(state.IPMACH, ir, state.IPTOT)] = 0.78;
  state.PARVAL[idx2(state.IPVEE, ir, state.IPTOT)] = 16.34;
  state.PARVAL[idx2(state.IPRHO, ir, state.IPTOT)] = 1.225;
  state.PARVAL[idx2(state.IPGEE, ir, state.IPTOT)] = 9.81;
  state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)] = 0.6;
  state.PARVAL[idx2(state.IPPHI, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPXCG, ir, state.IPTOT)] = 60.0;
  state.PARVAL[idx2(state.IPYCG, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPZCG, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPCD0, ir, state.IPTOT)] = 0.0;

  state.ICON[idx2(state.IVALFA, ir, state.IVMAX)] = state.ICCL;
  state.ICON[idx2(state.IVBETA, ir, state.IVMAX)] = state.ICBETA;
  state.ICON[idx2(state.IVROTX, ir, state.IVMAX)] = state.ICROTX;
  state.ICON[idx2(state.IVROTY, ir, state.IVMAX)] = state.ICROTY;
  state.ICON[idx2(state.IVROTZ, ir, state.IVMAX)] = state.ICROTZ;

  state.CONVAL[idx2(state.ICCL, ir, state.ICMAX)] = 0.6;
  state.CONVAL[idx2(state.ICBETA, ir, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICROTX, ir, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICROTY, ir, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(state.ICROTZ, ir, state.ICMAX)] = 0.0;

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    const iv = state.IVTOT + n;
    const ic = state.ICTOT + n;
    state.ICON[idx2(iv, ir, state.IVMAX)] = ic;
    state.CONVAL[idx2(ic, ir, state.ICMAX)] = 0.0;
  }
}

function runCase(model, mode) {
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
  applyB737ReferenceCaseState(state);
  buildGeometry(state, model);
  if (mode === 'wasm') {
    // Match current app worker path: wasm solve/gam enabled, other kernels JS.
    state.USE_WASM_SOLVE = true;
    state.USE_WASM_GAM = true;
    state.USE_WASM_AERO = false;
    state.USE_WASM_AIC = false;
    state.USE_WASM_LU = false;
  } else {
    state.USE_WASM_SOLVE = false;
    state.USE_WASM_GAM = false;
    state.USE_WASM_AERO = false;
    state.USE_WASM_AIC = false;
    state.USE_WASM_LU = false;
  }
  EXEC(state, 20, 0, 1);
  return state;
}

function computeNeutralPoint(state) {
  const dir = state.LNASA_SA ? -1.0 : 1.0;
  const alfa = Number(state.ALFA || 0.0);
  const ca = Math.cos(alfa);
  const sa = Math.sin(alfa);
  const wrot0 = Number(state.WROT?.[0] || 0.0);
  const wrot2 = Number(state.WROT?.[2] || 0.0);
  const rx = (wrot0 * ca + wrot2 * sa) * dir;
  const rz = (wrot2 * ca - wrot0 * sa) * dir;
  const wrotA0 = -rx * sa - rz * ca;
  const wrotA1 = 0.0;
  const wrotA2 = -rz * sa + rx * ca;
  const vinfA0 = Number(state.VINF_A?.[0] || 0.0);
  const vinfA1 = Number(state.VINF_A?.[1] || 0.0);
  const vinfA2 = Number(state.VINF_A?.[2] || 0.0);

  const clU = state.CLTOT_U || [];
  const cla = Number(clU[0] || 0.0) * vinfA0
    + Number(clU[3] || 0.0) * wrotA0
    + Number(clU[1] || 0.0) * vinfA1
    + Number(clU[4] || 0.0) * wrotA1
    + Number(clU[2] || 0.0) * vinfA2
    + Number(clU[5] || 0.0) * wrotA2
    + Number(state.CLTOT_A || 0.0);

  const cmU = state.CMTOT_U || [];
  const cma = Number(cmU[idx2(1, 0, 3)] || 0.0) * vinfA0
    + Number(cmU[idx2(1, 3, 3)] || 0.0) * wrotA0
    + Number(cmU[idx2(1, 1, 3)] || 0.0) * vinfA1
    + Number(cmU[idx2(1, 4, 3)] || 0.0) * wrotA1
    + Number(cmU[idx2(1, 2, 3)] || 0.0) * vinfA2
    + Number(cmU[idx2(1, 5, 3)] || 0.0) * wrotA2;

  let xnp = Number.NaN;
  if (Number.isFinite(cla) && Math.abs(cla) > 1e-12 && Number.isFinite(cma)) {
    const xref = Number(state.XYZREF?.[0] || 0.0);
    const cref = Number(state.CREF || 0.0);
    xnp = xref - cref * cma / cla;
  }
  return { xnp, cla, cma };
}

function compareAgainstRef(state, ref, tol, label) {
  assert.ok(ref.force, `${label}: missing FORCE in Fortran output`);

  assertNear(Number(state.CLTOT), ref.force.cl, tol.force, `${label}: CLTOT`);
  assertNear(Number(state.CDTOT), ref.force.cd, tol.force, `${label}: CDTOT`);
  assertNear(Number(state.CYTOT), ref.force.cy, tol.force, `${label}: CYTOT`);
  assertNear(Number(state.CDVTOT), Number(ref.cdvtot), tol.force, `${label}: CDVTOT`);

  assertNear(Number(state.CMTOT[0]), ref.force.cmx, tol.moment, `${label}: CMTOT[0]`);
  assertNear(Number(state.CMTOT[1]), ref.force.cmy, tol.moment, `${label}: CMTOT[1]`);
  assertNear(Number(state.CMTOT[2]), ref.force.cmz, tol.moment, `${label}: CMTOT[2]`);

  assertNear(Number(state.CFTOT[0]), ref.force.cfx, tol.force, `${label}: CFTOT[0]`);
  assertNear(Number(state.CFTOT[1]), ref.force.cfy, tol.force, `${label}: CFTOT[1]`);
  assertNear(Number(state.CFTOT[2]), ref.force.cfz, tol.force, `${label}: CFTOT[2]`);

  assert.ok(Number.isFinite(ref.xnp), `${label}: missing XNP in Fortran output`);
  const neutral = computeNeutralPoint(state);
  assertNear(Number(neutral.xnp), Number(ref.xnp), tol.xnp, `${label}: XNP`);

  assert.equal(Number(state.NVOR), Number(ref.nvor), `${label}: NVOR`);
  assert.equal(Number(state.NSTRIP), Number(ref.nstrip), `${label}: NSTRIP`);

  for (let j = 1; j <= state.NSTRIP; j += 1) {
    const strip = ref.strips[j];
    assert.ok(strip, `${label}: missing STRIP ${j}`);
    assertNear(Number(state.RLE[idx2(2, j, 4)]), strip.y, tol.stripGeom, `${label}: STRIP ${j} y`);
    assertNear(Number(state.RLE[idx2(3, j, 4)]), strip.z, tol.stripGeom, `${label}: STRIP ${j} z`);
    assertNear(Number(state.CNC[j]), strip.cnc, tol.stripAero, `${label}: STRIP ${j} CNC`);
    assertNear(Number(state.CLA_LSTRP[j]), strip.cla, tol.stripAero, `${label}: STRIP ${j} CLA`);
    assertNear(Number(state.CLT_LSTRP[j]), strip.clt, tol.stripAero, `${label}: STRIP ${j} CLT`);
    assertNear(Number(state.DWWAKE[j]), strip.dwwake, tol.stripAero, `${label}: STRIP ${j} DWWAKE`);
    assert.equal(state.LSTRIPOFF[j] ? 1 : 0, strip.off, `${label}: STRIP ${j} OFF`);
  }
}

let readyPromise = null;
async function ensureReady() {
  if (!readyPromise) {
    readyPromise = (async () => {
      await ensureRefBuilt('b737_exec_ref', refDir);
      await preloadAoperLinSolveWasm();
      await preloadAsetupWasm();
      await preloadAeroWasm();
      await preloadAicWasmBridge();
      await preloadAsetpLuWasmBridge();
    })();
  }
  await readyPromise;
}

for (const c of CASES) {
  test(`output parity: ${c.file} against Fortran (JS + WASM-worker-mode)`, { timeout: 180000 }, async () => {
    await ensureReady();

    const avlPath = path.join(runsDir, c.file);
    const avlText = fs.readFileSync(avlPath, 'utf8');
    const model = await buildSolverModel(avlText, { baseDir: runsDir });

    // Run from the AVL runs directory so AFILE dependencies resolve like native AVL usage.
    const refProc = spawnSync(refBin, [c.file], { encoding: 'utf8', cwd: runsDir });
    if (refProc.error) throw refProc.error;
    if (refProc.status !== 0) {
      throw new Error(refProc.stderr || refProc.stdout || `ref exited with ${refProc.status}`);
    }
    const refLog = `${String(refProc.stdout || '')}\n${String(refProc.stderr || '')}`;
    assert.ok(!/Airfoil file not found/i.test(refLog), `${c.file}: Fortran reference missing AFILE dependency`);
    const ref = parseRefOutput(refProc.stdout);

    const jsState = runCase(model, 'js');
    compareAgainstRef(jsState, ref, c.tol, `${c.file} JS`);

    const wasmState = runCase(model, 'wasm');
    compareAgainstRef(wasmState, ref, c.tol, `${c.file} WASM`);
  });
}

import test from 'node:test';
import assert from 'node:assert/strict';
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
  };
  const lines = String(stdout || '').split(/\r?\n/);
  for (const raw of lines) {
    const line = raw.trim();
    if (!line) continue;
    const tag = line.split(/\s+/)[0];
    if (tag === 'FORCE') {
      const nums = toNums(line);
      if (nums.length >= 9) {
        out.force = {
          cl: nums[0],
          cd: nums[1],
        };
      }
    } else if (tag === 'CDVTOT') {
      const nums = toNums(line);
      if (nums.length) out.cdvtot = nums[0];
    }
  }
  return out;
}

function applyReferenceCaseState(state) {
  const ir = 1;
  state.LNASA_SA = false;
  state.LSA_RATES = false;
  state.LBFORCE = false;

  state.MACH = 0.0;
  state.AMACH = 0.0;
  state.ALFA = 2.0 * state.DTR;
  state.BETA = 0.0;

  state.PARVAL[idx2(state.IPMACH, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPVEE, ir, state.IPTOT)] = 1.0;
  state.PARVAL[idx2(state.IPRHO, ir, state.IPTOT)] = 1.0;
  state.PARVAL[idx2(state.IPGEE, ir, state.IPTOT)] = 1.0;
  state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)] = 0.6;
  state.PARVAL[idx2(state.IPPHI, ir, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(state.IPXCG, ir, state.IPTOT)] = 0.177;
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
    xcg: 0.177,
    ycg: 0,
    zcg: 0,
    massLoaded: false,
  });
  applyReferenceCaseState(state);
  buildGeometry(state, model);
  if (mode === 'wasm') {
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

test('testcdcl_surface Fortran parity includes viscous drag for JS and WASM EXEC', async () => {
  await ensureRefBuilt('b737_exec_ref', refDir);
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();

  const refProc = spawnSync(refBin, ['testcdcl_surface.avl'], { encoding: 'utf8', cwd: runsDir });
  if (refProc.error) throw refProc.error;
  assert.equal(refProc.status, 0, refProc.stderr || refProc.stdout || 'b737_exec_ref failed');
  const ref = parseRefOutput(refProc.stdout);
  assert.ok(ref.force, 'missing FORCE output from Fortran reference');

  const avlPath = path.join(runsDir, 'testcdcl_surface.avl');
  const model = await buildSolverModel(await import('node:fs/promises').then((fs) => fs.readFile(avlPath, 'utf8')), { baseDir: runsDir });

  for (const mode of ['js', 'wasm']) {
    const state = runCase(model, mode);
    assert.equal(state.LVISC, true, `${mode}: LVISC should be enabled when CDCL is present`);
    assert.ok(Math.abs(Number(state.CDVTOT) - Number(ref.cdvtot)) <= 8e-5, `${mode}: CDVTOT mismatch`);
    assert.ok(Math.abs(Number(state.CDTOT) - Number(ref.force.cd)) <= 1e-4, `${mode}: CDTOT mismatch`);
    assert.ok(Math.abs(Number(state.CLTOT) - Number(ref.force.cl)) <= 5e-5, `${mode}: CLTOT mismatch`);
  }
});

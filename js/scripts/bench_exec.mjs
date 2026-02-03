import fs from 'node:fs';
import path from 'node:path';
import { performance } from 'node:perf_hooks';
import {
  buildSolverModel,
  buildExecState,
  buildGeometry,
  repoRootDir,
} from '../src/exec_pipeline.js';
import { EXEC, preloadAoperLinSolveWasm, preloadAsetupWasm, preloadAeroWasm, preloadAicWasmBridge, preloadAsetpLuWasmBridge } from '../src/aoper.js';
import { loadAoperWasm } from '../src/aoper_wasm.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function applyB737Constraints(state) {
  const IR = 1;
  const { IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ } = state;
  const { ICCL, ICBETA, ICROTX, ICROTY, ICROTZ } = state;

  state.CONVAL[idx2(ICCL, IR, state.ICMAX)] = 0.6;
  state.CONVAL[idx2(ICBETA, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTX, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTY, IR, state.ICMAX)] = 0.0;
  state.CONVAL[idx2(ICROTZ, IR, state.ICMAX)] = 0.0;

  state.ICON[idx2(IVALFA, IR, state.IVMAX)] = ICCL;
  state.ICON[idx2(IVBETA, IR, state.IVMAX)] = ICBETA;
  state.ICON[idx2(IVROTX, IR, state.IVMAX)] = ICROTX;
  state.ICON[idx2(IVROTY, IR, state.IVMAX)] = ICROTY;
  state.ICON[idx2(IVROTZ, IR, state.IVMAX)] = ICROTZ;

  for (let n = 1; n <= state.NCONTROL; n += 1) {
    state.CONVAL[idx2(state.ICTOT + n, IR, state.ICMAX)] = 0.0;
    state.ICON[idx2(state.IVTOT + n, IR, state.IVMAX)] = state.ICTOT + n;
  }
}

function buildPlaneState(model) {
  const state = buildExecState(model, {
    alpha: -0.1455,
    beta: 0.0,
    cl: 0.390510,
    vel: 64.5396,
    rho: 0.0005846,
    gee: 32.18,
    bank: 0.0,
    cd0: 0.00835,
    xcg: 0.02463,
    ycg: 0.0,
    zcg: 0.2239,
    cmx: 0.0,
    cmy: 0.0,
    cmz: 0.0,
  });
  buildGeometry(state, model);
  return state;
}

function buildB737State(model) {
  const state = buildExecState(model, {
    alpha: 2.0,
    beta: 0.0,
    cl: 0.6,
    vel: 16.34,
    rho: 1.225,
    gee: 9.81,
    bank: 0.0,
    cd0: 0.0,
    xcg: 60.0,
    ycg: 0.0,
    zcg: 0.0,
  });
  applyB737Constraints(state);
  buildGeometry(state, model);
  return state;
}

function summarize(samples) {
  const sorted = [...samples].sort((a, b) => a - b);
  const sum = samples.reduce((a, b) => a + b, 0);
  const avg = sum / samples.length;
  const p50 = sorted[Math.floor(sorted.length * 0.5)];
  const p90 = sorted[Math.floor(sorted.length * 0.9)];
  return { avg, p50, p90 };
}

async function preloadWasmKernels() {
  await preloadAoperLinSolveWasm();
  await preloadAsetupWasm();
  await preloadAeroWasm();
  await preloadAicWasmBridge();
  await preloadAsetpLuWasmBridge();
}

async function benchExec(label, buildState, execFn, niter, runs, warmup) {
  const samples = [];
  for (let i = 0; i < warmup + runs; i += 1) {
    const state = buildState();
    const t0 = performance.now();
    execFn(state, niter, 0, 1);
    const t1 = performance.now();
    if (i >= warmup) samples.push(t1 - t0);
  }
  const stats = summarize(samples);
  console.log(`${label}: avg=${stats.avg.toFixed(2)}ms p50=${stats.p50.toFixed(2)}ms p90=${stats.p90.toFixed(2)}ms (n=${runs})`);
}

function parseArgs() {
  const args = process.argv.slice(2);
  const out = { runs: 6, warmup: 2, niter: 0 };
  for (let i = 0; i < args.length; i += 1) {
    const arg = args[i];
    if (arg === '--runs') out.runs = Number(args[i + 1]);
    if (arg === '--warmup') out.warmup = Number(args[i + 1]);
    if (arg === '--niter') out.niter = Number(args[i + 1]);
  }
  if (!Number.isFinite(out.runs) || out.runs < 1) out.runs = 6;
  if (!Number.isFinite(out.warmup) || out.warmup < 0) out.warmup = 2;
  if (!Number.isFinite(out.niter) || out.niter < 0) out.niter = 0;
  return out;
}

async function main() {
  const { runs, warmup, niter } = parseArgs();
  const repoRoot = repoRootDir();
  const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');
  const planePath = path.join(runsDir, 'plane.avl');
  const b737Path = path.join(runsDir, 'b737.avl');

  const planeModel = await buildSolverModel(fs.readFileSync(planePath, 'utf8'), {});
  const b737Model = await buildSolverModel(fs.readFileSync(b737Path, 'utf8'), { baseDir: runsDir });

  const { EXEC_wasm } = await loadAoperWasm();
  await preloadWasmKernels();

  console.log(`EXEC benchmark (niter=${niter})`);
  await benchExec('plane JS', () => buildPlaneState(planeModel), EXEC, niter, runs, warmup);
  await benchExec('plane WASM (stub)', () => buildPlaneState(planeModel), EXEC_wasm, niter, runs, warmup);
  await benchExec('plane WASM kernels', () => {
    const state = buildPlaneState(planeModel);
    state.USE_WASM_SOLVE = true;
    state.USE_WASM_GAM = true;
    state.USE_WASM_AERO = true;
    state.USE_WASM_AIC = true;
    state.USE_WASM_LU = true;
    return state;
  }, EXEC, niter, runs, warmup);
  await benchExec('b737 JS', () => buildB737State(b737Model), EXEC, niter, runs, warmup);
  await benchExec('b737 WASM (stub)', () => buildB737State(b737Model), EXEC_wasm, niter, runs, warmup);
  await benchExec('b737 WASM kernels', () => {
    const state = buildB737State(b737Model);
    state.USE_WASM_SOLVE = true;
    state.USE_WASM_GAM = true;
    state.USE_WASM_AERO = true;
    state.USE_WASM_AIC = true;
    state.USE_WASM_LU = true;
    return state;
  }, EXEC, niter, runs, warmup);
}

main().catch((err) => {
  console.error(err);
  process.exitCode = 1;
});

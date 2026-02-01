import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { Worker } from 'node:worker_threads';
import vm from 'node:vm';
import { repoRootDir } from '../src/exec_pipeline.js';
import { MAKESURF, ENCALC, SDUPL } from '../src/amake.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function runWorker(state, workerPath) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(workerPath, { type: 'module' });
    worker.on('message', (msg) => {
      if (msg.type === 'error') {
        worker.terminate();
        reject(new Error(msg.message));
        return;
      }
      if (msg.type === 'result') {
        worker.terminate();
        resolve(msg);
      }
    });
    worker.on('error', (err) => {
      worker.terminate();
      reject(err);
    });
    worker.postMessage({ state });
  });
}

function makeEl(value = '') {
  return {
    value,
    addEventListener() {},
    classList: { toggle() {} },
    style: {},
    getContext() { return { fillRect() {}, clearRect() {}, beginPath() {}, moveTo() {}, lineTo() {}, stroke() {}, fillText() {} }; },
  };
}

test('EXEC worker should not produce NaNs after structured clone', async () => {
  const repoRoot = repoRootDir();
  const modelPath = path.join(repoRoot, 'third_party', 'avl', 'runs', 'b737.avl');
  const workerPath = path.join(repoRoot, 'js', 'test', 'worker_exec_shim.mjs');
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const text = await fs.readFile(modelPath, 'utf8');
  const appText = await fs.readFile(appPath, 'utf8');

  const stripped = appText
    .replace(/^import .*$/gm, '')
    .replace(/import\.meta/g, '{}')
    .replace(/bootApp\(\)\.catch[\s\S]*?;\s*$/m, '');

  const elMap = {
    fileInput: makeEl(),
    saveBtn: makeEl(),
    fileMeta: makeEl(),
    fileText: makeEl(text),
    bank: makeEl('0'),
    cl: makeEl('0.6'),
    vel: makeEl('16.34'),
    mass: makeEl('120'),
    rho: makeEl('1.225'),
    gee: makeEl('9.81'),
    flightMode: makeEl('level'),
    levelInputs: makeEl(),
    loopInputs: makeEl(),
    clLoop: makeEl('0.6'),
    velLoop: makeEl('16.34'),
    radLoop: makeEl('0'),
    facLoop: makeEl('0'),
    trimBtn: makeEl(),
    execToggle: makeEl(),
    viewer: makeEl(),
    trefftz: makeEl(),
    outAlpha: makeEl(),
    outBeta: makeEl(),
    outBank: makeEl(),
    outCL: makeEl(),
    outCD: makeEl(),
    outCY: makeEl(),
    outV: makeEl(),
    outRad: makeEl(),
    outFac: makeEl(),
    outThe: makeEl(),
    outCM: makeEl(),
    outRates: makeEl(),
    outDef: makeEl(),
    outStability: makeEl(),
    outBodyDeriv: makeEl(),
    outForcesTotal: makeEl(),
    outForcesSurface: makeEl(),
    outForcesStrip: makeEl(),
    outForcesElement: makeEl(),
    outForcesBody: makeEl(),
    outHinge: makeEl(),
    debugLog: makeEl(),
    clearDebug: makeEl(),
    viewerPan: makeEl(),
    viewerZoomIn: makeEl(),
    viewerZoomOut: makeEl(),
    viewerHome: makeEl(),
    viewerView: makeEl(),
    viewerGrid: makeEl(),
    viewerCoord: makeEl(),
    constraintTable: makeEl(),
  };

  const context = {
    console,
    Math,
    Number,
    Float32Array,
    Int32Array,
    Uint8Array,
    Map,
    Set,
    Array,
    requestAnimationFrame() {},
    document: {
      getElementById: (id) => elMap[id] ?? makeEl(),
    },
    window: {
      addEventListener() {},
      __debugLog() {},
    },
    Worker: undefined,
    MAKESURF,
    ENCALC,
    SDUPL,
  };
  vm.createContext(context);
  vm.runInContext(stripped, context);

  context.readConstraintRows = () => [
    { variable: 'alpha', constraint: 'cl', numeric: 0.6 },
    { variable: 'beta', constraint: 'beta', numeric: 0.0 },
    { variable: 'p', constraint: 'cmx', numeric: 0.0 },
    { variable: 'q', constraint: 'cmy', numeric: 0.0 },
    { variable: 'r', constraint: 'cmz', numeric: 0.0 },
  ];

  const model = context.buildSolverModel(text);
  const state = context.buildExecState(model);
  context.applyConstraintRowsToState(state, model.controlMap);
  context.buildGeometry(state, model);

  const result = await runWorker(state, workerPath);
  assert.ok(Number.isFinite(result.ALFA), 'ALFA should be finite');
  assert.ok(Number.isFinite(result.BETA), 'BETA should be finite');
});

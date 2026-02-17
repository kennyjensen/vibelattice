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

function runWorker(state, workerPath, message = {}) {
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
    worker.postMessage({ state, ...message });
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
    .replace(/^\s*import[\s\S]*?;\s*$/gm, '')
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

  const result = await runWorker(state, workerPath, { useWasm: false });
  assert.ok(Number.isFinite(result.ALFA), 'ALFA should be finite');
  assert.ok(Number.isFinite(result.BETA), 'BETA should be finite');
  assert.equal(result.USE_WASM_SOLVE, false, 'WASM solve should be off when useWasm=false');
  assert.equal(result.USE_WASM_GAM, false, 'WASM GAM should be off when useWasm=false');
  assert.equal(result.USE_WASM_AERO, false, 'WASM aero should be off when useWasm=false');
  assert.equal(result.USE_WASM_AIC, false, 'WASM AIC should be off when useWasm=false');
  assert.equal(result.USE_WASM_LU, false, 'WASM LU should be off when useWasm=false');
});

test('EXEC worker should map useWasm toggle to all wasm kernel flags', async () => {
  const repoRoot = repoRootDir();
  const workerPath = path.join(repoRoot, 'js', 'dist', 'exec-worker.js');
  const text = await fs.readFile(workerPath, 'utf8');
  assert.match(text, /state\.USE_WASM_SOLVE\s*=\s*Boolean\(useWasm\)\s*;/, 'USE_WASM_SOLVE should follow useWasm');
  assert.match(text, /state\.USE_WASM_GAM\s*=\s*Boolean\(useWasm\)\s*;/, 'USE_WASM_GAM should follow useWasm');
  assert.match(text, /state\.USE_WASM_AERO\s*=\s*Boolean\(useWasm\)\s*;/, 'USE_WASM_AERO should follow useWasm');
  assert.match(text, /state\.USE_WASM_AIC\s*=\s*Boolean\(useWasm\)\s*;/, 'USE_WASM_AIC should follow useWasm');
  assert.match(text, /state\.USE_WASM_LU\s*=\s*Boolean\(useWasm\)\s*;/, 'USE_WASM_LU should follow useWasm');
  assert.match(text, /if\s*\(useWasm\)\s*\{[\s\S]*?await\s+loadExecWasm\(\)[\s\S]*?execFn\s*\(\s*20\s*,\s*0\s*,\s*1\s*,\s*0\s*\)/, 'useWasm path should execute through EXEC wasm export');
  assert.match(
    text,
    /function\s+getAmodeNativeCtx\s*\([\s\S]*?typeof\s+wasm\.AMODE_runchk\s*!==\s*['"]function['"][\s\S]*?typeof\s+wasm\.AMODE_sysmat\s*!==\s*['"]function['"][\s\S]*?typeof\s+wasm\.AMODE_eigsol\s*!==\s*['"]function['"]/,
    'worker should require native AMODE exports',
  );
  assert.match(
    text,
    /function\s+runAmodeNativeEigen\s*\([\s\S]*?wasm\.AMODE_runchk\(ir\s*\|\s*0\)[\s\S]*?wasm\.AMODE_sysmat\(ir\s*\|\s*0,\s*ctx\.asysPtr,\s*ctx\.bsysPtr,\s*ctx\.rsysPtr\)[\s\S]*?wasm\.AMODE_eigsol\(ir\s*\|\s*0,\s*Math\.fround\(etol\s*\|\|\s*0\),\s*ctx\.asysPtr,\s*nsys\)/,
    'worker should solve eigenmodes through AMODE native runchk/sysmat/eigsol',
  );
  assert.match(
    text,
    /if\s*\(useWasm\)\s*\{[\s\S]*?const\s+wasm\s*=\s*await\s+loadAmodeWasm\(\)[\s\S]*?runAmodeNativeEigen\(wasm,\s*state,\s*IR,\s*ETOL\)/,
    'useWasm eigenmode path should execute through AMODE native bridge',
  );
  assert.doesNotMatch(
    text,
    /RUNCHK_NATIVE|SYSMAT_PRECHECK|APPMAT_PRECHECK|EIGSOL_PRECHECK/,
    'worker should not depend on legacy amode trampoline exports',
  );
});

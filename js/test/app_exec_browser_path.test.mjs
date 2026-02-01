import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import vm from 'node:vm';
import { Worker } from 'node:worker_threads';
import { repoRootDir } from '../src/exec_pipeline.js';
import { MAKESURF, ENCALC, SDUPL } from '../src/amake.js';

function makeEl(value = '') {
  return {
    value,
    addEventListener() {},
    classList: { toggle() {} },
    style: {},
    getContext() { return { fillRect() {}, clearRect() {}, beginPath() {}, moveTo() {}, lineTo() {}, stroke() {}, fillText() {} }; },
  };
}

class ShimWorker {
  constructor(_url, _opts) {
    const repoRoot = repoRootDir();
    const shimPath = path.join(repoRoot, 'js', 'test', 'worker_exec_shim.mjs');
    this.worker = new Worker(shimPath, { type: 'module' });
    if (Array.isArray(globalThis.__testWorkers)) {
      globalThis.__testWorkers.push(this.worker);
    }
    this.onmessage = null;
    this.onerror = null;
    this.worker.on('message', (msg) => {
      if (this.onmessage) this.onmessage({ data: msg });
    });
    this.worker.on('error', (err) => {
      if (this.onerror) this.onerror({ message: err?.message ?? String(err) });
    });
  }
  postMessage(msg) {
    this.worker.postMessage(msg);
  }
  terminate() {
    this.worker.terminate();
  }
}

test('app browser EXEC path should not yield NaN for b737', { timeout: 90000 }, async () => {
  const createdWorkers = [];
  globalThis.__testWorkers = createdWorkers;
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const b737Path = path.join(repoRoot, 'third_party', 'avl', 'runs', 'b737.avl');
  const appText = await fs.readFile(appPath, 'utf8');
  const avlText = await fs.readFile(b737Path, 'utf8');

  const stripped = appText
    .replace(/^import .*$/gm, '')
    .replace(/import\.meta/g, '{}')
    .replace(/new URL\(['"]\.\/exec-worker\.js['"], \{\}\)/g, "'./exec-worker.js'")
    .replace(/bootApp\(\)\.catch[\s\S]*?;\s*$/m, '');

  const elMap = {
    fileInput: makeEl(),
    saveBtn: makeEl(),
    fileMeta: makeEl(),
    fileText: makeEl(avlText),
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
    execToggle: makeEl('true'),
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

  let resolveResult;
  const resultPromise = new Promise((resolve) => { resolveResult = resolve; });

  const debugLogs = [];
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
    URL: class URL {
      constructor(href) {
        this.href = href;
      }
      toString() {
        return this.href;
      }
    },
    performance: { now: () => 0 },
    requestAnimationFrame() {},
    setTimeout: () => 1,
    clearTimeout() {},
    document: {
      getElementById: (id) => elMap[id] ?? makeEl(),
    },
    window: {
      addEventListener() {},
      __debugLog(message) {
        debugLogs.push(message);
      },
    },
    Worker: ShimWorker,
    MAKESURF,
    ENCALC,
    SDUPL,
  };
  vm.createContext(context);
  vm.runInContext(stripped, context);

  context.readConstraintRows = () => [
    { variable: 'alpha', constraint: 'cl', numeric: 0.6 },
    { variable: 'beta', constraint: 'beta', numeric: 0.0 },
    { variable: 'p', constraint: 'p', numeric: 0.0 },
    { variable: 'q', constraint: 'q', numeric: 0.0 },
    { variable: 'r', constraint: 'r', numeric: 0.0 },
    { variable: 'ctrl:aileron', constraint: 'cmx', numeric: 0.0 },
    { variable: 'ctrl:elevator', constraint: 'cmy', numeric: 0.0 },
    { variable: 'ctrl:rudder', constraint: 'cmz', numeric: 0.0 },
  ];

  const prevApply = context.applyExecResults;
  context.applyExecResults = (msg) => {
    if (prevApply) prevApply(msg);
    resolveResult(msg);
  };

  let result;
  try {
    context.runExecFromText(avlText);
    result = await Promise.race([
      resultPromise,
      new Promise((_, reject) => setTimeout(() => {
        reject(new Error(`EXEC timed out.\n${debugLogs.join('\n')}`));
      }, 60000)),
    ]);
  } finally {
    createdWorkers.forEach((worker) => worker.terminate());
    delete globalThis.__testWorkers;
  }

  const debugDump = debugLogs.length ? `\n${debugLogs.join('\n')}` : '';
  assert.ok(Number.isFinite(result.ALFA), `ALFA should be finite${debugDump}`);
  assert.ok(Number.isFinite(result.BETA), `BETA should be finite${debugDump}`);
});

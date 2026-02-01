import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import vm from 'node:vm';
import { repoRootDir } from '../src/exec_pipeline.js';

function makeEl(value = '') {
  return {
    value,
    addEventListener() {},
    classList: { toggle() {} },
    style: {},
    querySelector() { return null; },
    querySelectorAll() { return []; },
    getContext() {
      return {
        fillRect() {},
        clearRect() {},
        beginPath() {},
        moveTo() {},
        lineTo() {},
        stroke() {},
        fillText() {},
      };
    },
  };
}

test('boot loads plane.avl by default and triggers trim', { timeout: 30000 }, async () => {
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const planePath = path.join(repoRoot, 'third_party', 'avl', 'runs', 'plane.avl');
  const appText = await fs.readFile(appPath, 'utf8');
  const planeText = await fs.readFile(planePath, 'utf8');

  const stripped = appText
    .replace(/^import .*$/gm, '')
    .replace(/import\.meta/g, '{}')
    .replace(/new URL\(['"]\.\/exec-worker\.js['"], \{\}\)/g, "'./exec-worker.js'")
    .replace(/bootApp\(\)\.catch[\s\S]*?;\s*$/m, '');

  const elMap = {
    appRoot: makeEl(),
    navSettings: makeEl(),
    navPlots: makeEl(),
    navOutputs: makeEl(),
    fileInput: makeEl(),
    saveBtn: makeEl(),
    fileMeta: makeEl(),
    fileText: makeEl(''),
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
    constraintTable: { ...makeEl(), innerHTML: '', appendChild() {}, children: [] },
  };

  const debugLogs = [];
  let execCalled = false;

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
      constructor(href) { this.href = href; }
      toString() { return this.href; }
    },
    performance: { now: () => 0 },
    requestAnimationFrame() {},
    setTimeout(fn) { fn(); return 1; },
    clearTimeout() {},
    document: {
      getElementById: (id) => elMap[id] ?? makeEl(),
      querySelectorAll() { return []; },
      createElement() {
        return {
          className: '',
          dataset: {},
          innerHTML: '',
          value: '',
          classList: { toggle() {} },
          addEventListener() {},
          appendChild() {},
          querySelector() { return null; },
          querySelectorAll() { return []; },
        };
      },
    },
    window: {
      addEventListener() {},
      location: { href: 'http://localhost/' },
      __debugLog(message) { debugLogs.push(message); },
    },
    fetch: async () => ({ ok: true, text: async () => planeText }),
    TRMSET_CORE() {},
  };

  vm.createContext(context);
  vm.runInContext(stripped, context);

  const makeRow = (variable, constraint, numeric) => ({
    row: { classList: { toggle() {} } },
    select: { classList: { toggle() {} } },
    value: { classList: { toggle() {} } },
    variable,
    constraint,
    numeric,
  });
  context.readConstraintRows = () => [
    makeRow('alpha', 'alpha', 0.0),
    makeRow('beta', 'beta', 0.0),
    makeRow('p', 'p', 0.0),
    makeRow('q', 'q', 0.0),
    makeRow('r', 'r', 0.0),
  ];

  context.ensureThree = async () => false;
  context.runExecFromText = () => { execCalled = true; };

  await context.bootApp();

  assert.equal(elMap.fileText.value.trim(), planeText.trim());
  assert.ok(execCalled, 'trim should trigger EXEC by default');
  assert.ok(debugLogs.some((line) => line.includes('Loaded default file: plane.avl')));
  assert.ok(debugLogs.some((line) => line.includes('Trim requested.')));
});

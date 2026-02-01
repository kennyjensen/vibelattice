import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import vm from 'node:vm';
import { EXEC } from '../src/aoper.js';
import { MAKESURF, ENCALC, SDUPL } from '../src/amake.js';
import { repoRootDir } from '../src/exec_pipeline.js';

function makeEl(value = '') {
  return {
    value,
    addEventListener() {},
    classList: { toggle() {} },
    style: {},
    getContext() { return { fillRect() {}, clearRect() {}, beginPath() {}, moveTo() {}, lineTo() {}, stroke() {}, fillText() {} }; },
  };
}

test('app build pipeline should not yield NaN for b737 EXEC', async () => {
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const b737Path = path.join(repoRoot, 'third_party', 'avl', 'runs', 'b737.avl');
  const appText = await fs.readFile(appPath, 'utf8');
  const avlText = await fs.readFile(b737Path, 'utf8');

  // Strip ESM imports and boot call so we can eval safely in Node.
  const stripped = appText
    .replace(/^import .*$/gm, '')
    .replace(/import\.meta/g, '{}')
    .replace(/bootApp\\(\\)\\.catch[\\s\\S]*?;\\s*$/m, '');

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
    EXEC,
    MAKESURF,
    ENCALC,
    SDUPL,
  };
  vm.createContext(context);
  vm.runInContext(stripped, context);

  // Override constraint rows to match the app default (alpha -> CL, beta -> beta, rates -> moments).
  context.readConstraintRows = () => [
    { variable: 'alpha', constraint: 'cl', numeric: 0.6 },
    { variable: 'beta', constraint: 'beta', numeric: 0.0 },
    { variable: 'p', constraint: 'cmx', numeric: 0.0 },
    { variable: 'q', constraint: 'cmy', numeric: 0.0 },
    { variable: 'r', constraint: 'cmz', numeric: 0.0 },
  ];

  const model = context.buildSolverModel(avlText);
  const state = context.buildExecState(model);
  context.applyConstraintRowsToState(state, model.controlMap);
  context.buildGeometry(state, model);

  context.EXEC(state, 8, 0, 1);

  assert.ok(Number.isFinite(state.ALFA), 'ALFA should be finite');
  assert.ok(Number.isFinite(state.BETA), 'BETA should be finite');
  assert.ok(state.VINF.every((v) => Number.isFinite(v)), 'VINF should be finite');
  assert.ok(state.WROT.every((v) => Number.isFinite(v)), 'WROT should be finite');
});

import { parentPort } from 'node:worker_threads';
import fs from 'node:fs';
import vm from 'node:vm';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { EXEC } from '../dist/aoper.js';
import { TRMSET_CORE } from '../dist/atrim.js';
import { SYSMAT, EIGSOL } from '../src/amode.js';
import { APPGET } from '../src/amass.js';

if (parentPort) {
  globalThis.postMessage = (msg) => {
    parentPort.postMessage(msg);
  };

  parentPort.on('message', (msg) => {
    if (typeof globalThis.onmessage === 'function') {
      globalThis.onmessage({ data: msg });
    }
  });
}

if (parentPort) {
  const __dirname = path.dirname(fileURLToPath(import.meta.url));
  const workerPath = path.join(__dirname, '..', 'dist', 'exec-worker.js');
  globalThis.EXEC = EXEC;
  globalThis.TRMSET_CORE = TRMSET_CORE;
  globalThis.SYSMAT = SYSMAT;
  globalThis.EIGSOL = EIGSOL;
  globalThis.APPGET = APPGET;
  const raw = fs.readFileSync(workerPath, 'utf8');
  const withoutImports = raw.replace(/^import .*$/gm, '');
  const withoutMeta = withoutImports
    .replace(/import\.meta\.url/g, JSON.stringify(`file://${workerPath}`))
    .replace(/import\.meta/g, '{}');
  const patched = `const EXEC = globalThis.EXEC;\nconst TRMSET_CORE = globalThis.TRMSET_CORE;\nconst SYSMAT = globalThis.SYSMAT;\nconst EIGSOL = globalThis.EIGSOL;\nconst APPGET = globalThis.APPGET;\n${withoutMeta.replace(/\\bonmessage\\s*=\\s*/g, 'globalThis.onmessage = ')}`;
  vm.runInThisContext(patched, { filename: workerPath });
}

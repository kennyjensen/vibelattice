import { parentPort } from 'node:worker_threads';
import fs from 'node:fs';
import vm from 'node:vm';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { EXEC } from '../dist/aoper.js';

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
  const raw = fs.readFileSync(workerPath, 'utf8');
  const withoutImports = raw.replace(/^import .*$/gm, '');
  const withoutMeta = withoutImports
    .replace(/import\.meta\.url/g, JSON.stringify(`file://${workerPath}`))
    .replace(/import\.meta/g, '{}');
  const patched = `const EXEC = globalThis.EXEC;\n${withoutMeta.replace(/\\bonmessage\\s*=\\s*/g, 'globalThis.onmessage = ')}`;
  vm.runInThisContext(patched, { filename: workerPath });
}

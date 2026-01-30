import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { Worker } from 'node:worker_threads';
import { repoRootDir } from '../src/exec_pipeline.js';

const repoRoot = repoRootDir();
const runsDir = path.join(repoRoot, 'third_party', 'avl', 'runs');

function runExecInWorker(text, options, timeoutMs = 70000) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(new URL('./exec_worker.mjs', import.meta.url), {
      workerData: { text, options },
    });
    const timer = setTimeout(() => {
      worker.terminate();
      reject(new Error(`EXEC timed out after ${timeoutMs}ms`));
    }, timeoutMs);
    worker.on('message', (msg) => {
      clearTimeout(timer);
      worker.terminate();
      resolve(msg);
    });
    worker.on('error', (err) => {
      clearTimeout(timer);
      worker.terminate();
      reject(err);
    });
  });
}

test('FULL EXEC runs on b737', { timeout: 80000 }, async () => {
  const text = await fs.readFile(path.join(runsDir, 'b737.avl'), 'utf8');
  const result = await runExecInWorker(text, {
    baseDir: runsDir,
    vel: 50,
    cl: 0.5,
    alpha: 2.0,
    beta: 0.0,
  }, 70000);
  assert.equal(result.ok, true, result.error || 'EXEC failed');
  assert.ok(result.NVOR > 0, 'expected NVOR > 0');
  assert.ok(Number.isFinite(result.CLTOT), 'CLTOT should be finite');
  assert.equal(result.cncNan, 0, 'expected CNC to be finite');
  assert.equal(result.dwNan, 0, 'expected DWWAKE to be finite');
  assert.equal(result.cltNan, 0, 'expected CLT_LSTRP to be finite');
});

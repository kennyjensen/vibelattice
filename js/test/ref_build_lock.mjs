import fs from 'node:fs/promises';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

async function acquireLock(lockPath, timeoutMs = 300000) {
  const start = Date.now();
  while (true) {
    try {
      return await fs.open(lockPath, 'wx');
    } catch (err) {
      if (err?.code !== 'EEXIST') throw err;
      if (Date.now() - start > timeoutMs) {
        throw new Error(`timeout waiting for ${lockPath}`);
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  }
}

export async function ensureRefBuilt(target, refDir) {
  const lockPath = path.join(refDir, '.ref_build.lock');
  const lockHandle = await acquireLock(lockPath);
  try {
    const proc = spawnSync('make', ['-B', target], { cwd: refDir, encoding: 'utf8' });
    if (proc.error) throw proc.error;
    if (proc.status !== 0) {
      throw new Error(proc.stderr || proc.stdout || `make failed with ${proc.status}`);
    }
  } finally {
    await lockHandle.close();
    await fs.unlink(lockPath).catch(() => {});
  }
}

import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { repoRootDir } from '../src/exec_pipeline.js';

test('app EXEC state wires PARVAL(IPMASS) from UI mass input', async () => {
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const text = await fs.readFile(appPath, 'utf8');
  const needle = 'PARVAL[idx2(IPMASS, IR, IPTOT)]';
  assert.ok(text.includes(needle), 'app.js should assign PARVAL(IPMASS,IR) before EXEC');
});

test('app EXEC state guards header refs against NaN', async () => {
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const text = await fs.readFile(appPath, 'utf8');
  assert.ok(text.includes('Number.isFinite(model.header.sref)'), 'app.js should guard SREF');
  assert.ok(text.includes('Number.isFinite(model.header.cref)'), 'app.js should guard CREF');
  assert.ok(text.includes('Number.isFinite(model.header.bref)'), 'app.js should guard BREF');
});

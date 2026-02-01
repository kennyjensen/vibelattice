import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { repoRootDir } from '../src/exec_pipeline.js';

const repoRoot = repoRootDir();
const distDir = path.join(repoRoot, 'js', 'dist');
const indexPath = path.join(distDir, 'index.html');
const appPath = path.join(distDir, 'app.js');

function extractIds(html) {
  const ids = new Set();
  const re = /id\s*=\s*"([^"]+)"/g;
  let match;
  while ((match = re.exec(html))) {
    ids.add(match[1]);
  }
  return ids;
}

function extractGetElementIds(js) {
  const ids = new Set();
  const re = /getElementById\(\s*['"]([^'"]+)['"]\s*\)/g;
  let match;
  while ((match = re.exec(js))) {
    ids.add(match[1]);
  }
  return ids;
}

test('app startup HTML has all required element ids', () => {
  const html = fs.readFileSync(indexPath, 'utf8');
  const js = fs.readFileSync(appPath, 'utf8');
  const htmlIds = extractIds(html);
  const jsIds = extractGetElementIds(js);

  const missing = [];
  for (const id of jsIds) {
    if (!htmlIds.has(id)) missing.push(id);
  }

  assert.equal(missing.length, 0, `Missing ids in index.html: ${missing.join(', ')}`);
  assert.ok(htmlIds.has('constraintTable'), 'constraintTable should exist');
});

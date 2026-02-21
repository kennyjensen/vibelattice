import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseAVL } from '../shared/avl_parser.js';

test('parseAVL captures BODY and BFILE entries for supra.avl', async () => {
  const here = path.dirname(fileURLToPath(import.meta.url));
  const repo = path.resolve(here, '..', '..');
  const avlPath = path.join(repo, 'third_party', 'avl', 'runs', 'supra.avl');
  const text = await fs.readFile(avlPath, 'utf8');
  const model = parseAVL(text);

  assert.ok(Array.isArray(model.bodies));
  assert.ok(model.bodies.length > 0);
  assert.equal(model.bodies[0].name, 'Fuse pod');
  assert.equal(model.bodies[0].bodyFile, 'fuseSupra.dat');
  assert.ok(Array.isArray(model.bodyFiles));
  assert.ok(model.bodyFiles.includes('fuseSupra.dat'));
});

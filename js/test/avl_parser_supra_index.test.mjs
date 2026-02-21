import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { parseAVL } from '../shared/avl_parser.js';

test('parseAVL reads INDEX property so supra inner/outer wing share component', async () => {
  const here = path.dirname(fileURLToPath(import.meta.url));
  const file = path.resolve(here, '../../third_party/avl/runs/supra.avl');
  const text = await fs.readFile(file, 'utf8');
  const model = parseAVL(text);

  const inner = model.surfaces.find((s) => s?.name === 'Inner Wing');
  const outer = model.surfaces.find((s) => s?.name === 'Outer Wing');
  assert.ok(inner, 'Inner Wing should exist');
  assert.ok(outer, 'Outer Wing should exist');
  assert.equal(inner.component, 1);
  assert.equal(outer.component, 1);
});

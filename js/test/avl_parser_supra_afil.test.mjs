import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseAVL } from '../shared/avl_parser.js';

test('parseAVL handles AFIL x/c bounds and reads filename from next line (supra.avl)', async () => {
  const here = path.dirname(fileURLToPath(import.meta.url));
  const repo = path.resolve(here, '..', '..');
  const avlPath = path.join(repo, 'third_party', 'avl', 'runs', 'supra.avl');
  const text = await fs.readFile(avlPath, 'utf8');
  const model = parseAVL(text);

  assert.ok(Array.isArray(model.airfoilFiles));
  assert.equal(model.airfoilFiles.includes('0.0 1.0'), false);
  assert.equal(model.airfoilFiles.some((name) => /^AFIL\b/i.test(String(name))), false);
  assert.equal(model.airfoilFiles.includes('ag40d.dat'), true);
  assert.equal(model.airfoilFiles.includes('ag41d.dat'), true);
  assert.equal(model.airfoilFiles.includes('ag42d.dat'), true);
  assert.equal(model.airfoilFiles.includes('ag43d.dat'), true);

  const airfoilRefs = [];
  model.surfaces.forEach((surf) => {
    (surf.sections || []).forEach((section) => {
      if (section?.airfoilFile) airfoilRefs.push(String(section.airfoilFile));
    });
  });
  assert.equal(airfoilRefs.some((name) => /^AFIL\b/i.test(name)), false);
});

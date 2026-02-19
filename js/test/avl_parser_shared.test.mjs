import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseAVL } from '../shared/avl_parser.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, '..', '..');
const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
const execPipelinePath = path.join(repoRoot, 'js', 'src', 'exec_pipeline.js');

test('shared AVL parser handles inline comments and AFIL quoting', () => {
  const text = [
    'Test Aircraft',
    '0.11 ! mach inline comment',
    '0 0 0',
    '10 2 12',
    '0 0 0',
    'SURFACE',
    'Wing',
    '3 1 0 1',
    'SECTION',
    '0 0 0 1.2 2.0 7 1  # section span count',
    'AFILE',
    '"airfoils/wing.dat" ! file comment',
    'SECTION',
    '1 2 0 0.8 1.0 1 1',
  ].join('\n');

  const model = parseAVL(text);
  assert.equal(model.header.mach, 0.11);
  assert.equal(model.surfaces.length, 1);
  assert.equal(model.surfaces[0].sections.length, 2);
  assert.equal(model.surfaces[0].sections[0].nSpan, 7);
  assert.equal(model.surfaces[0].sections[0].airfoilFile, 'airfoils/wing.dat');
  assert.deepEqual(model.airfoilFiles, ['airfoils/wing.dat']);
});

test('app and exec_pipeline both use shared AVL parser module', () => {
  const appSrc = fs.readFileSync(appPath, 'utf8');
  const execSrc = fs.readFileSync(execPipelinePath, 'utf8');

  assert.match(
    appSrc,
    /import\s+\{\s*parseAVL\s*\}\s+from\s+['"]\.\.\/shared\/avl_parser\.js['"];/,
    'dist app should import parseAVL from shared module',
  );
  assert.doesNotMatch(
    appSrc,
    /function\s+parseAVL\s*\(/,
    'dist app should not define a local parseAVL implementation',
  );

  assert.match(
    execSrc,
    /import\s+\{\s*parseAVL\b[\s\S]*\}\s+from\s+['"]\.\.\/shared\/avl_parser\.js['"];/,
    'exec pipeline should import parseAVL from shared module',
  );
  assert.doesNotMatch(
    execSrc,
    /export\s+function\s+parseAVL\s*\(/,
    'exec pipeline should not define a local parseAVL implementation',
  );
});


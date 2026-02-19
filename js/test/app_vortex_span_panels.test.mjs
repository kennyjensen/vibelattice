import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { repoRootDir } from '../src/exec_pipeline.js';

test('viewer span panel resolver prioritizes sectionA nSpan (AVL semantics)', async () => {
  const repoRoot = repoRootDir();
  const appPath = path.join(repoRoot, 'js', 'dist', 'app.js');
  const text = await fs.readFile(appPath, 'utf8');

  assert.match(
    text,
    /function\s+resolveSectionSpanPanels\s*\(\s*surface\s*,\s*sectionA\s*,\s*sectionB\s*\)\s*\{[\s\S]*?const\s+candidates\s*=\s*\[[\s\S]*?Number\(sectionA\?\.nSpan\)\s*,[\s\S]*?Number\(sectionB\?\.nSpan\)\s*,[\s\S]*?Number\(surface\?\.nSpan\)\s*,[\s\S]*?\];/,
    'resolveSectionSpanPanels should use sectionA nSpan before sectionB/surface fallback',
  );
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('multi-file load follows AVL->airfoil->mass->run and staged update chain', async ({ page }) => {
  const root = path.resolve('.');
  const server = http.createServer(async (req, res) => {
    const reqPath = (req.url || '/').split('?')[0];
    const clean = decodeURIComponent(reqPath === '/' ? '/index.html' : reqPath);
    const filePath = path.join(root, clean);
    try {
      const data = await fs.readFile(filePath);
      if (filePath.endsWith('.html')) res.setHeader('Content-Type', 'text/html; charset=utf-8');
      else if (filePath.endsWith('.js')) res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
      else if (filePath.endsWith('.css')) res.setHeader('Content-Type', 'text/css; charset=utf-8');
      else if (filePath.endsWith('.json')) res.setHeader('Content-Type', 'application/json; charset=utf-8');
      else if (filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.avl') || filePath.endsWith('.dat')) {
        res.setHeader('Content-Type', 'text/plain; charset=utf-8');
      }
      res.statusCode = 200;
      res.end(data);
    } catch {
      res.statusCode = 404;
      res.end('Not found');
    }
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const address = server.address();
  const port = typeof address === 'object' && address ? address.port : 0;

  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.clearAutoUpdateTrace));
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 20000 });

    await page.evaluate(() => window.__trefftzTestHook.clearAutoUpdateTrace());

    const files = [
      path.join(root, 'third_party', 'avl', 'runs', 'plane.run'),
      path.join(root, 'third_party', 'avl', 'runs', 'plane.mass'),
      path.join(root, 'third_party', 'avl', 'runs', 'n2412.dat'),
      path.join(root, 'third_party', 'avl', 'runs', 'plane.avl'),
    ];
    await page.setInputFiles('#fileInput', files);

    await page.waitForFunction(() => {
      const t = window.__trefftzTestHook?.getAutoUpdateTrace?.() || [];
      const hasFiles = t.includes('file:avl')
        && t.includes('file:airfoil')
        && t.includes('file:mass')
        && t.includes('file:run');
      const execIdx = t.lastIndexOf('stage:exec');
      const eigenIdx = t.lastIndexOf('stage:eigen');
      const hasTerminalEigen = execIdx >= 0 && eigenIdx > execIdx;
      const hasExec = Boolean(window.__trefftzTestHook?.getLastExecResult?.());
      return hasFiles && hasTerminalEigen && hasExec;
    }, null, { timeout: 45000 });

    const trace = await page.evaluate(() => window.__trefftzTestHook.getAutoUpdateTrace());

    const fileAvl = trace.indexOf('file:avl');
    const fileAfoil = trace.indexOf('file:airfoil');
    const fileMass = trace.indexOf('file:mass');
    const fileRun = trace.indexOf('file:run');
    expect(fileAvl).toBeGreaterThanOrEqual(0);
    expect(fileAfoil).toBeGreaterThan(fileAvl);
    expect(fileMass).toBeGreaterThan(fileAfoil);
    expect(fileRun).toBeGreaterThan(fileMass);

    const stageFlight = trace.indexOf('stage:flight');
    const stageConstraints = trace.indexOf('stage:constraints');
    const stageExec = trace.indexOf('stage:exec');
    const stageEigen = trace.lastIndexOf('stage:eigen');
    expect(stageFlight).toBeGreaterThanOrEqual(0);
    expect(stageConstraints).toBeGreaterThan(stageFlight);
    expect(stageExec).toBeGreaterThan(stageConstraints);
    expect(stageEigen).toBeGreaterThan(stageExec);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

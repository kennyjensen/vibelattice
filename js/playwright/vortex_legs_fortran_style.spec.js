import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('vortex wake legs use Fortran-style +X direction and 2*BREF length', async ({ page }) => {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState));

    await page.click('#viewerVortices');
    await expect.poll(async () => page.evaluate(() => {
      const state = window.__trefftzTestHook?.getViewerOverlayState?.();
      return Number(state?.vortexLegSegments || 0);
    }), { timeout: 15000 }).toBeGreaterThan(10);

    const state = await page.evaluate(() => window.__trefftzTestHook?.getViewerOverlayState?.() || null);
    expect(state).toBeTruthy();
    expect(state.vortexLegDxMean).toBeGreaterThan(25);
    expect(state.vortexLegDxMean).toBeLessThan(35);
    expect(state.vortexLegDyAbsMean).toBeLessThan(1e-5);
    expect(state.vortexLegDzAbsMean).toBeLessThan(1e-5);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

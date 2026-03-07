import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('viewer grid stays centered at the AVL origin', async ({ page }) => {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 20000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState));

    const initial = await page.evaluate(() => window.__trefftzTestHook.getViewerOverlayState());
    expect(initial.gridWorld).toBeTruthy();
    expect(initial.axisWorld).toBeTruthy();
    expect(Math.abs(initial.gridWorld.x - initial.axisWorld.x)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(initial.gridWorld.y - initial.axisWorld.y)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(initial.gridWorld.z - initial.axisWorld.z)).toBeLessThanOrEqual(1e-6);

    await page.click('#viewerGrid');
    const yz = await page.evaluate(() => window.__trefftzTestHook.getViewerOverlayState());
    expect(Math.abs(yz.gridWorld.x - yz.axisWorld.x)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(yz.gridWorld.y - yz.axisWorld.y)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(yz.gridWorld.z - yz.axisWorld.z)).toBeLessThanOrEqual(1e-6);

    await page.click('#viewerGrid');
    const xz = await page.evaluate(() => window.__trefftzTestHook.getViewerOverlayState());
    expect(Math.abs(xz.gridWorld.x - xz.axisWorld.x)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(xz.gridWorld.y - xz.axisWorld.y)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(xz.gridWorld.z - xz.axisWorld.z)).toBeLessThanOrEqual(1e-6);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

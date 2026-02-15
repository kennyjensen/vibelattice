import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('view button cycles top, forward, side presets with matching camera axes', async ({ page }) => {
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
    const entrypoints = ['/index.html', '/js/dist/index.html'];
    for (const entry of entrypoints) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 20000 });
      await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerViewState));

      await page.click('#viewerView');
      let state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('top');
      expect(Math.abs(state.cameraPosition.z)).toBeGreaterThan(Math.abs(state.cameraPosition.x));
      expect(Math.abs(state.cameraPosition.z)).toBeGreaterThan(Math.abs(state.cameraPosition.y));
      await expect(page.locator('#viewerView')).toHaveAttribute('title', /Top \(down\)/);

      await page.click('#viewerView');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('forward');
      expect(Math.abs(state.cameraPosition.x)).toBeGreaterThan(Math.abs(state.cameraPosition.y));
      expect(Math.abs(state.cameraPosition.x)).toBeGreaterThan(Math.abs(state.cameraPosition.z));
      await expect(page.locator('#viewerView')).toHaveAttribute('title', /Forward \(aft\)/);

      await page.click('#viewerView');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('side');
      expect(Math.abs(state.cameraPosition.y)).toBeGreaterThan(Math.abs(state.cameraPosition.x));
      expect(Math.abs(state.cameraPosition.y)).toBeGreaterThan(Math.abs(state.cameraPosition.z));
      await expect(page.locator('#viewerView')).toHaveAttribute('title', /Side \(Y axis\)/);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

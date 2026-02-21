import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

test('body orientation and visibility follow surface render mode controls', async ({ page }) => {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
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
    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect.poll(async () => page.evaluate(() => {
      const state = window.__trefftzTestHook?.getBodyVisualState?.();
      return Number(state?.bodyMeshCount || 0);
    }), { timeout: 30000 }).toBeGreaterThan(0);

    const orientation = await page.evaluate(() => window.__trefftzTestHook?.getBodyVisualState?.());
    expect(typeof orientation?.minX).toBe('number');
    expect(typeof orientation?.maxX).toBe('number');
    expect(typeof orientation?.surfaceMaxX).toBe('number');
    expect(orientation.maxX).toBeGreaterThan(orientation.surfaceMaxX);

    const wireOnly = await page.evaluate(() => window.__trefftzTestHook?.getBodyVisualState?.());
    expect(wireOnly.bodyWireVisible).toBeGreaterThan(0);
    expect(wireOnly.bodyMeshVisible).toBe(0);
    expect(wireOnly.bodyWireSegments).toBeGreaterThan(300);
    expect(wireOnly.bodyWireSegments).toBeLessThan(900);
    expect(wireOnly.bodyWireSegments % 24).toBe(0);
    const stationXs = Array.isArray(wireOnly.bodyWireStationXs) ? wireOnly.bodyWireStationXs : [];
    expect(stationXs.length).toBeGreaterThan(10);
    const deltas = [];
    for (let i = 1; i < stationXs.length; i += 1) deltas.push(stationXs[i] - stationXs[i - 1]);
    const rounded = deltas
      .filter((d) => Number.isFinite(d) && d > 1e-8)
      .map((d) => Number(d.toFixed(5)));
    const uniqueCount = new Set(rounded).size;
    expect(uniqueCount).toBeGreaterThan(3);

    await page.click('#viewerSurface'); // -> both
    await expect.poll(async () => page.evaluate(() => {
      const s = window.__trefftzTestHook?.getBodyVisualState?.();
      return `${s?.bodyMeshVisible || 0}:${s?.bodyWireVisible || 0}`;
    }), { timeout: 10000 }).toMatch(/^[1-9]\d*:[1-9]\d*$/);

    await page.click('#viewerSurface'); // -> surface
    await expect.poll(async () => page.evaluate(() => {
      const s = window.__trefftzTestHook?.getBodyVisualState?.();
      return `${s?.bodyMeshVisible || 0}:${s?.bodyWireVisible || 0}`;
    }), { timeout: 10000 }).toMatch(/^[1-9]\d*:0$/);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

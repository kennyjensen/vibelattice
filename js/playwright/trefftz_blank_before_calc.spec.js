import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('trefftz panel stays blank when no calculation data is present', async ({ page }) => {
  test.setTimeout(60000);
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
    await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

    await page.evaluate(() => {
      window.__trefftzTestHook.setTrefftzData(null);
    });
    await page.waitForFunction(() => window.__trefftzTestHook.zeroLine === null);

    await expect(page.locator('.trefftz-legend')).toBeHidden();
    await expect(page.locator('.trefftz-axis-labels')).toBeHidden();
    await expect(page.locator('.trefftz-ticks')).toHaveCount(0);
    await expect(page.locator('.trefftz-axis-lines')).toHaveCount(0);
    await expect(page.locator('.trefftz-grid-lines')).toHaveCount(0);

    const dbg = await page.evaluate(() => ({
      gridLen: Array.isArray(window.__trefftzTestHook.gridY) ? window.__trefftzTestHook.gridY.length : -1,
      zeroLine: window.__trefftzTestHook.zeroLine,
      mapAxis: window.__trefftzTestHook.mapAxis,
    }));
    expect(dbg.gridLen).toBe(0);
    expect(dbg.zeroLine).toBeNull();
    expect(dbg.mapAxis).toBeNull();
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('trefftz right axis allocates space below zero and aligns ai=0 with cl=0', async ({ page }) => {
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

  const trefftzData = {
    strips: [
      [-7.0, 0.0, 0.12, 0.24, 0.20, 0.030, 1],
      [0.0, 0.0, 0.15, 0.26, 0.22, 0.040, 1],
      [7.0, 0.0, 0.11, 0.23, 0.19, 0.025, 1],
    ],
    surfaces: [{ start: 0, count: 3 }],
    cref: 1.0,
  };

  try {
    for (const entry of ['/index.html', '/js/dist/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

      await page.evaluate((data) => {
        window.__trefftzTestHook.setTrefftzData(data);
      }, trefftzData);
      await page.waitForSelector('.trefftz-tick.right');

      const axis = await page.evaluate(() => window.__trefftzTestHook?.trefftzRightAxis || null);
      expect(axis).toBeTruthy();
      expect(Number(axis.wminRaw)).toBeLessThan(-1e-4);
      expect(Number(axis.wminAdj)).toBeLessThan(-1e-4);
      expect(Number.isFinite(Number(axis.zeroWLine))).toBeTruthy();

      const zeroLine = await page.evaluate(() => Number(window.__trefftzTestHook?.zeroLine));
      expect(Number.isFinite(zeroLine)).toBeTruthy();
      expect(Math.abs(Number(axis.zeroWLine) - zeroLine)).toBeLessThan(0.51);

      const rightTicks = await page.locator('.trefftz-tick.right').allTextContents();
      const tickNums = rightTicks
        .map((s) => Number(String(s || '').trim()))
        .filter((n) => Number.isFinite(n));
      expect(tickNums.length).toBeGreaterThan(0);
      expect(Math.min(...tickNums)).toBeLessThan(-1e-4);
      expect(Math.max(...tickNums)).toBeGreaterThanOrEqual(0);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

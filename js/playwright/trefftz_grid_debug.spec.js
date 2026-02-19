import { test, expect } from '@playwright/test';
import path from 'node:path';
import http from 'node:http';
import fs from 'node:fs';

test('trefftz grid debug output', async ({ page }) => {
  const root = process.cwd();
  const server = http.createServer((req, res) => {
    const urlPath = decodeURIComponent(req.url || '/');
    const safePath = urlPath.split('?')[0].replace(/^\/+/, '');
    const filePath = path.join(root, safePath || 'index.html');
    if (!filePath.startsWith(root)) {
      res.writeHead(403);
      res.end('Forbidden');
      return;
    }
    try {
      const data = fs.readFileSync(filePath);
      const ext = path.extname(filePath);
      const type = ext === '.html' ? 'text/html'
        : ext === '.js' ? 'text/javascript'
        : ext === '.css' ? 'text/css'
        : ext === '.wasm' ? 'application/wasm'
        : 'application/octet-stream';
      res.writeHead(200, { 'Content-Type': type });
      res.end(data);
    } catch {
      res.writeHead(404);
      res.end('Not found');
    }
  });
  await new Promise((resolve) => server.listen(0, resolve));
  const { port } = server.address();

  await page.setViewportSize({ width: 1200, height: 800 });
  await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => {
    const log = document.getElementById('debugLog')?.textContent || '';
    return log.includes('App module loaded.') || log.includes('App module failed:');
  });
  const debugText = await page.evaluate(() => document.getElementById('debugLog')?.textContent || '');
  expect(debugText).toContain('App module loaded.');

  await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');
  const trefData = {
    strips: [
      [-7.0, 0.0, 0.1, 0.2, 0.15, -0.01, 1],
      [0.0, 0.0, 0.15, 0.25, 0.2, -0.015, 1],
      [7.0, 0.0, 0.1, 0.2, 0.15, -0.01, 1],
    ],
    surfaces: [{ start: 0, count: 3 }],
    cref: 1.0,
  };
  await page.evaluate((data) => {
    window.__trefftzTestHook.setTrefftzData(data);
  }, trefData);
  await page.evaluate(async () => {
    if (document.fonts) {
      await document.fonts.ready;
    }
  });
  await page.evaluate((data) => {
    window.__trefftzTestHook.setTrefftzData(data);
  }, trefData);
  await page.waitForFunction(() => window.__trefftzTestHook?.layoutReady === true);
  await page.evaluate(() => new Promise(requestAnimationFrame));

  const gridInfo = await page.evaluate(() => ({
    gridY: window.__trefftzTestHook?.gridY || [],
    zeroLine: window.__trefftzTestHook?.zeroLine ?? null,
  }));

  console.log('Trefftz grid debug', gridInfo);
  expect(gridInfo.gridY.length).toBeGreaterThan(0);
  expect(Number.isFinite(gridInfo.zeroLine)).toBe(true);

  server.close();
});

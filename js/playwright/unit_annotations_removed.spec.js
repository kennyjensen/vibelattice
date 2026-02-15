import { test, expect } from '@playwright/test';
import path from 'node:path';
import http from 'node:http';
import fs from 'node:fs';

test('flight and trefftz labels have no unit annotations', async ({ page }) => {
  const root = process.cwd();
  const server = http.createServer((req, res) => {
    const urlPath = decodeURIComponent(req.url || '/');
    const safePath = urlPath.split('?')[0].replace(/^\/+/, '');
    const filePath = path.join(root, safePath || 'js/dist/index.html');
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

  await page.goto(`http://127.0.0.1:${port}/js/dist/index.html`, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

  await expect(page.locator('label[for="gee"] .label-text')).toHaveCount(0);
  await expect(page.locator('#settingsCol .flight-conditions .flight-row .label-text').nth(0)).toHaveText('Gravity');
  await expect(page.locator('#settingsCol .flight-conditions .flight-row .label-text').nth(1)).toHaveText('Air density');
  await expect(page.locator('#settingsCol .flight-conditions .flight-row .label-text').nth(2)).toHaveText('Mass');
  await expect(page.locator('#settingsCol .flight-conditions .flight-row .label-text').nth(3)).toHaveText('Velocity');
  await expect(page.locator('#levelInputs .label-text')).toHaveText('Bank angle');
  await expect(page.locator('#loopInputs .label-text').first()).toHaveText('Turn radius');

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
  await page.waitForSelector('.trefftz-axis-label.x');
  await expect(page.locator('.trefftz-axis-label.x')).toHaveText('Span');

  await new Promise((resolve) => server.close(resolve));
});

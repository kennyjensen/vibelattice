import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function parseCellNumber(text) {
  const match = String(text || '').replace(/\u00a0/g, ' ').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

test('plane.run applies CDo so total-forces drag is nonzero', async ({ page }) => {
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.avl') || filePath.endsWith('.mass')) {
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
    await page.setViewportSize({ width: 1400, height: 900 });
    for (const entry of ['/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });

      const runPath = path.resolve('third_party/avl/runs/plane.run');
      await page.setInputFiles('#runCasesInput', runPath);
      await expect(page.locator('#runCasesMeta')).toContainText('plane.run');

      await page.evaluate(() => { document.getElementById('trimBtn')?.click(); });
      await page.waitForFunction(() => {
        const cdEl = document.getElementById('outCD');
        const cdpEl = document.getElementById('outCDvis');
        const t1 = String(cdEl?.textContent || '').trim();
        const t2 = String(cdpEl?.textContent || '').trim();
        return t1 && t1 !== '-' && t2 && t2 !== '-';
      });

      const values = await page.evaluate(() => ({
        cd: String(document.getElementById('outCD')?.textContent || ''),
        cdp: String(document.getElementById('outCDvis')?.textContent || ''),
      }));
      const cd = parseCellNumber(values.cd);
      const cdp = parseCellNumber(values.cdp);

      expect(Number.isFinite(cdp)).toBeTruthy();
      expect(cdp).toBeGreaterThan(0.005);
      expect(Number.isFinite(cd)).toBeTruthy();
      expect(cd).toBeGreaterThanOrEqual(cdp - 1e-4);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

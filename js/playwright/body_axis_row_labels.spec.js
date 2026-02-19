import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`Body-axis derivative rows use u/v/w/p/q/r labels on ${entry}`, async ({ page }) => {
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
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await page.click('#trimBtn');

      await expect.poll(async () => {
        const labels = await page.locator('#outBodyDeriv .stability-cell.stability-head').allInnerTexts();
        return labels.map((s) => s.trim()).filter(Boolean);
      }, {
        timeout: 30000,
      }).toEqual(expect.arrayContaining(['u', 'v', 'w', 'p', 'q', 'r']));

      const labels = (await page.locator('#outBodyDeriv .stability-cell.stability-head').allInnerTexts())
        .map((s) => s.trim())
        .filter(Boolean);

      expect(labels).not.toContain('α');
      expect(labels).not.toContain('β');
      expect(labels).toContain('d1');
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

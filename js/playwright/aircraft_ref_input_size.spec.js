import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`Aircraft reference inputs use tighter spacing and larger width on ${entry}`, async ({ page }) => {
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
      await page.setViewportSize({ width: 1500, height: 1000 });
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });

      const planeText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'plane.avl'), 'utf8');
      await page.fill('#fileText', planeText);
      await page.dispatchEvent('#fileText', 'input');

      await expect(page.locator('#fileSummary')).not.toHaveClass(/hidden/);

      const metrics = await page.evaluate(() => {
        const grid = document.querySelector('.file-ref-grid');
        const gridStyle = getComputedStyle(grid);
        const ids = ['#fileSref', '#fileCref', '#fileBref', '#fileXref', '#fileYref', '#fileZref'];
        const widths = ids.map((sel) => Number(document.querySelector(sel).getBoundingClientRect().width.toFixed(2)));
        return {
          columnGap: gridStyle.columnGap,
          rowGap: gridStyle.rowGap,
          widths,
        };
      });

      expect(metrics.columnGap).toBe('2px');
      expect(metrics.rowGap).toBe('6px');
      for (const w of metrics.widths) expect(w).toBeGreaterThanOrEqual(62);
      const minW = Math.min(...metrics.widths);
      const maxW = Math.max(...metrics.widths);
      expect(maxW - minW).toBeLessThanOrEqual(1.0);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

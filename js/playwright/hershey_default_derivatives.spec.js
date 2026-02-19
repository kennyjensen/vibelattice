import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

const entrypoints = ['/index.html', '/js/dist/index.html'];

function parseSigned(text) {
  const t = String(text || '').replace(/\u00a0/g, ' ').trim();
  return Number(t);
}

for (const entry of entrypoints) {
  test(`hershey default derivatives render Fortran-style CLq and CLd1 on ${entry}`, async ({ page }) => {
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

      const hersheyText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'hershey.avl'), 'utf8');
      await page.fill('#fileText', hersheyText);
      await page.dispatchEvent('#fileText', 'input');

      await page.fill('#mass', '1.0');
      await page.fill('#gee', '1.0');
      await page.fill('#rho', '1.0');
      await page.fill('#vel', '0.0');
      await page.fill('#cl', '0.0');
      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      const clqCell = page.locator('#outStability .stability-cell[title="CLq"] .stability-num');
      const cld1Cell = page.locator('#outStability .stability-cell[title="CLd1"] .stability-num');
      await expect(clqCell).not.toHaveText(/^\s*-\s*$/);
      await expect(cld1Cell).not.toHaveText(/^\s*-\s*$/);

      const clq = parseSigned(await clqCell.innerText());
      const cld1 = parseSigned(await cld1Cell.innerText());

      expect(clq).toBeGreaterThan(4.5);
      expect(clq).toBeLessThan(5.3);
      expect(cld1).toBeGreaterThan(0.02);
      expect(cld1).toBeLessThan(0.04);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

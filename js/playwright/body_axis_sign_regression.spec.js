import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

function parseSigned(text) {
  const t = String(text || '').replace(/\u00a0/g, ' ').trim();
  return Number(t);
}

for (const entry of entrypoints) {
  test(`body-axis sign regression stays Fortran-consistent on ${entry}`, async ({ page }) => {
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
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });

      const hersheyText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'hershey.avl'), 'utf8');
      await page.fill('#fileText', hersheyText);
      await page.dispatchEvent('#fileText', 'input');

      await page.fill('#mass', '1.0');
      await page.fill('#gee', '1.0');
      await page.fill('#rho', '1.0');
      await page.fill('#vel', '1.0');
      await page.fill('#cl', '0.0');
      await page.selectOption('.constraint-row[data-var="alpha"] .constraint-select', 'alpha');
      await page.selectOption('.constraint-row[data-var="beta"] .constraint-select', 'beta');
      await page.selectOption('.constraint-row[data-var="p"] .constraint-select', 'p');
      await page.selectOption('.constraint-row[data-var="q"] .constraint-select', 'q');
      await page.selectOption('.constraint-row[data-var="r"] .constraint-select', 'r');
      await page.fill('.constraint-row[data-var="alpha"] .constraint-value', '1.0');
      await page.fill('.constraint-row[data-var="beta"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="p"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="q"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="r"] .constraint-value', '0.0');
      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      const get = async (title) => {
        const txt = await page.locator(`#outBodyDeriv .stability-cell[title="${title}"] .stability-num`).innerText();
        return parseSigned(txt);
      };

      await expect.poll(async () => Math.abs(await get('CZq')), { timeout: 30000 }).toBeGreaterThan(1);

      const cxq = await get('CXq');
      const czq = await get('CZq');
      const cxd1 = await get('CXd1');
      const czd1 = await get('CZd1');

      expect(cxq).toBeGreaterThan(0);
      expect(czq).toBeLessThan(0);
      expect(cxd1).toBeGreaterThan(0);
      expect(czd1).toBeLessThan(0);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`surface-force Cl/Cn sign regression stays Fortran-consistent on ${entry}`, async ({ page }) => {
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

      const avlText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'hershey.avl'), 'utf8');
      await page.fill('#fileText', avlText);
      await page.dispatchEvent('#fileText', 'input');

      await page.fill('#mass', '1.0');
      await page.fill('#gee', '1.0');
      await page.fill('#rho', '1.0');
      await page.fill('#vel', '1.0');
      await page.fill('#cl', '0.0');
      await page.selectOption('.constraint-row[data-var="alpha"] .constraint-select', 'alpha');
      await page.selectOption('.constraint-row[data-var="beta"] .constraint-select', 'beta');
      await page.fill('.constraint-row[data-var="alpha"] .constraint-value', '1.0');
      await page.fill('.constraint-row[data-var="beta"] .constraint-value', '2.0');
      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      await expect.poll(async () => {
        const count = await page.locator('#outForcesSurface .stability-num').count();
        return count;
      }, { timeout: 30000 }).toBeGreaterThan(0);

      const comparison = await page.evaluate(() => {
        const hook = window.__trefftzTestHook;
        const result = hook?.getLastExecResult?.();
        if (!result) return { error: 'No exec result available' };

        const dir = (typeof result.LNASA_SA === 'boolean') ? (result.LNASA_SA ? -1.0 : 1.0) : -1.0;
        const cms = result.CMSURF || [];
        const idx = cms.length > 1 ? 1 : -1;
        if (idx < 1) return { error: 'No surface moments found' };

        const cols = 7;
        const cells = Array.from(document.querySelectorAll('#outForcesSurface .stability-cell'))
          .map((el) => String(el.textContent || '').replace(/\u00a0/g, ' ').trim());
        const rowStart = idx * cols;
        const actualCl = Number(cells[rowStart + 4]);
        const actualCn = Number(cells[rowStart + 6]);

        return {
          idx,
          expectedCl: dir * Number(cms[idx]?.[0] || 0),
          expectedCn: dir * Number(cms[idx]?.[2] || 0),
          actualCl,
          actualCn,
        };
      });

      expect(comparison.error).toBeUndefined();
      expect(comparison.actualCl).toBeCloseTo(comparison.expectedCl, 4);
      expect(comparison.actualCn).toBeCloseTo(comparison.expectedCn, 4);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

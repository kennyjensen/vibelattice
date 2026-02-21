import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('supra run case 1 syncs solved CL into Flight Conditions CL field', async ({ page }) => {
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
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await expect(page.locator('.constraint-row[data-var="alpha"] .constraint-select')).toHaveValue('alpha');
    await expect(page.locator('.constraint-row[data-var="alpha"] .constraint-value')).toHaveValue(/^\s*5(\.0+)?\s*$/);

    await page.waitForFunction(() => {
      const r = window.__trefftzTestHook?.getLastExecResult?.();
      return Boolean(r && Number.isFinite(r.CLTOT) && r.CLTOT > 0.8);
    }, null, { timeout: 30000 });

    const vals = await page.evaluate(() => {
      const r = window.__trefftzTestHook?.getLastExecResult?.();
      return {
        clInput: Number(document.querySelector('#cl')?.value || NaN),
        clTot: Number(r?.CLTOT),
      };
    });

    expect(Number.isFinite(vals.clInput)).toBe(true);
    expect(Number.isFinite(vals.clTot)).toBe(true);
    expect(vals.clTot).toBeGreaterThan(0.8);
    expect(Math.abs(vals.clInput - vals.clTot)).toBeLessThan(0.02);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

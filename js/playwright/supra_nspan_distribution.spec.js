import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

test('supra uses SURFACE Nspan distribution across sections (inner/outer wing)', async ({ page }) => {
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
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect.poll(async () => page.evaluate(() => {
      const items = window.__trefftzTestHook?.getDisplaySpanPanelSummary?.() || [];
      return items.length;
    }), { timeout: 30000 }).toBeGreaterThan(0);

    const summary = await page.evaluate(() => window.__trefftzTestHook?.getDisplaySpanPanelSummary?.() || []);
    const clean = (v) => String(v || '').trim();
    const inner = summary.find((s) => clean(s?.name) === 'Inner Wing');
    const outer = summary.find((s) => clean(s?.name) === 'Outer Wing');

    expect(inner).toBeTruthy();
    expect(outer).toBeTruthy();
    expect(inner.total).toBe(8);
    expect(Array.isArray(inner.counts) ? inner.counts.length : 0).toBe(1);
    expect(outer.total).toBe(18);
    expect(Array.isArray(outer.counts) ? outer.counts.length : 0).toBe(4);
    expect((outer.counts || []).some((v) => Number(v) > 1)).toBeTruthy();
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

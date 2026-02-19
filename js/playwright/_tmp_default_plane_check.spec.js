import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`tmp default plane outputs on ${entry}`, async ({ page }) => {
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
      page.on('pageerror', (e) => console.log('PAGEERROR', String(e)));
      page.on('console', (m) => console.log('CONSOLE', m.type(), m.text()));
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await page.click('#trimBtn');
      await expect.poll(async () => page.evaluate(() => {
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        return Boolean(r && r.CFTOT && r.CLTOT_U);
      }), { timeout: 40000 }).toBe(true);

      const out = await page.evaluate(() => {
        const get = (id) => (document.querySelector(id)?.textContent || '').trim();
        const count = (id) => document.querySelectorAll(id).length;
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        return {
          alpha: get('#outAlpha'),
          cl: get('#outCL'),
          cd: get('#outCD'),
          outStabilityCells: count('#outStability .stability-cell'),
          outBodyCells: count('#outBodyDeriv .stability-cell'),
          outSurfaceCells: count('#outForcesSurface .stability-cell'),
          outHingeCells: count('#outHinge .stability-cell'),
          keys: r ? Object.keys(r).slice(0, 30) : [],
          chinge: r?.CHINGE,
          cftot: r?.CFTOT,
          cltotu: r?.CLTOT_U,
        };
      });
      console.log('OUT', JSON.stringify(out, null, 2));
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

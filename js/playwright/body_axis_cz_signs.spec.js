import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

const entrypoints = ['/index.html', '/js/dist/index.html'];

function parseNum(text) {
  return Number(String(text || '').replace(/\u00a0/g, ' ').trim());
}

for (const entry of entrypoints) {
  test(`Body-axis CZ derivatives follow AVL sign convention on ${entry}`, async ({ page }) => {
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

      await expect.poll(async () => page.evaluate(() => {
        const hook = window.__trefftzTestHook;
        if (!hook || typeof hook.getLastExecResult !== 'function') return false;
        const r = hook.getLastExecResult();
        return Boolean(r && r.CFTOT_U && r.CFTOT_U[2] && Number.isFinite(r.BREF) && Number.isFinite(r.CREF));
      }), { timeout: 30000 }).toBe(true);

      const exec = await page.evaluate(() => window.__trefftzTestHook.getLastExecResult());
      const dir = exec.LNASA_SA ? -1 : 1;
      const cz = exec.CFTOT_U[2];
      const czd = exec.CFTOT_D?.[2] || [];
      const bref = Number(exec.BREF);
      const cref = Number(exec.CREF);

      const expected = {
        CZu: -Number(cz[0] ?? 0),
        CZv: -(dir * Number(cz[1] ?? 0)),
        CZw: -Number(cz[2] ?? 0),
        CZp: Number(cz[3] ?? 0) * 2 / bref,
        CZq: dir * Number(cz[4] ?? 0) * 2 / cref,
        CZr: Number(cz[5] ?? 0) * 2 / bref,
        CZd1: dir * Number(czd[0] ?? 0),
      };

      for (const key of ['CZu', 'CZv', 'CZw', 'CZp', 'CZq', 'CZr', 'CZd1']) {
        const shown = parseNum(await page.locator(`#outBodyDeriv .stability-cell[title="${key}"] .stability-num`).innerText());
        expect(shown).toBeCloseTo(expected[key], 6);
      }
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

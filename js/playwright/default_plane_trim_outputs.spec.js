import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`default plane trim renders core output grids on ${entry}`, async ({ page }) => {
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
      await expect.poll(async () => page.evaluate(() => (document.querySelector('#runCasesMeta')?.textContent || '')), { timeout: 30000 })
        .toContain('plane.run');
      await expect.poll(async () => page.evaluate(() => (document.querySelector('#massPropsMeta')?.textContent || '')), { timeout: 30000 })
        .toContain('plane.mass');
      await page.check('#useWasmExec');
      await page.click('#trimBtn');
      await expect.poll(async () => page.evaluate(() => {
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        return Boolean(r?.CFTOT && r?.CMTOT && r?.CLTOT_U);
      }), { timeout: 30000 }).toBe(true);

      const report = await page.evaluate(() => {
        const txt = (id) => String(document.querySelector(id)?.textContent || '').trim();
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        return {
          alpha: txt('#outAlpha'),
          cl: txt('#outCL'),
          cd: txt('#outCD'),
          stabilityCells: document.querySelectorAll('#outStability .stability-cell').length,
          bodyCells: document.querySelectorAll('#outBodyDeriv .stability-cell').length,
          surfaceCells: document.querySelectorAll('#outForcesSurface .stability-cell').length,
          hingeCells: document.querySelectorAll('#outHinge .stability-cell').length,
          CDTOT: r?.CDTOT,
          CLTOT: r?.CLTOT,
          CFTOT: r?.CFTOT,
          CMTOT: r?.CMTOT,
          CDVTOT: r?.CDVTOT,
          PARVAL: r?.PARVAL ? [r.PARVAL[43], r.PARVAL[44], r.PARVAL[42]] : null, // rho, gee, vee-ish slots
          PARVAL_MASS: r?.PARVAL ? [r.PARVAL[50], r.PARVAL[30], r.PARVAL[31], r.PARVAL[32], r.PARVAL[33], r.PARVAL[34], r.PARVAL[35]] : null,
          debug: String(document.querySelector('#debugLog')?.textContent || '').split('\n').slice(-12),
        };
      });
      // eslint-disable-next-line no-console
      console.log('default-report', entry, report);

      expect(report.alpha).not.toBe('-');
      expect(report.cl).not.toBe('-');
      expect(report.cd).not.toBe('-');
      expect(report.stabilityCells).toBeGreaterThan(0);
      expect(report.bodyCells).toBeGreaterThan(0);
      expect(report.surfaceCells).toBeGreaterThan(0);
      expect(report.hingeCells).toBeGreaterThan(0);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

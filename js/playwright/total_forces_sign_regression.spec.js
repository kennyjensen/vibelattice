import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

function parseSigned(text) {
  const t = String(text || '').replace(/\u00a0/g, ' ').replace(/−/g, '-').trim();
  if (t === '-' || t === '') return 0;
  return Number(t);
}

for (const entry of entrypoints) {
  test(`total-forces sign regression stays Fortran-consistent on ${entry}`, async ({ page }) => {
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

      await page.selectOption('.constraint-row[data-var="alpha"] .constraint-select', 'alpha');
      await page.selectOption('.constraint-row[data-var="beta"] .constraint-select', 'beta');
      await page.fill('.constraint-row[data-var="alpha"] .constraint-value', '1.0');
      await page.fill('.constraint-row[data-var="beta"] .constraint-value', '2.0');
      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      await expect.poll(async () => page.evaluate(() => {
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        return Boolean(r?.CFTOT && r?.CMTOT && r?.WROT);
      }), { timeout: 30000 }).toBe(true);

      const expected = await page.evaluate(() => {
        const r = window.__trefftzTestHook.getLastExecResult();
        const dir = (typeof r.LNASA_SA === 'boolean') ? (r.LNASA_SA ? -1.0 : 1.0) : -1.0;
        return {
          pb: dir * Number(r.WROT?.[0] || 0),
          qc: Number(r.WROT?.[1] || 0),
          rb: dir * Number(r.WROT?.[2] || 0),
          cy: Number(r.CFTOT?.[1] || 0),
          cl: dir * Number(r.CMTOT?.[0] || 0),
          cm: Number(r.CMTOT?.[1] || 0),
          cn: dir * Number(r.CMTOT?.[2] || 0),
        };
      });

      const actual = {
        pb: parseSigned(await page.locator('#outPb2v').innerText()),
        qc: parseSigned(await page.locator('#outQc2v').innerText()),
        rb: parseSigned(await page.locator('#outRb2v').innerText()),
        cy: parseSigned(await page.locator('#outCYtot').innerText()),
        cl: parseSigned(await page.locator('#outCltot').innerText()),
        cm: parseSigned(await page.locator('#outCmtot').innerText()),
        cn: parseSigned(await page.locator('#outCntot').innerText()),
      };

      expect(Math.abs(actual.pb - expected.pb)).toBeLessThanOrEqual(1e-3);
      expect(Math.abs(actual.qc - expected.qc)).toBeLessThanOrEqual(1e-3);
      expect(Math.abs(actual.rb - expected.rb)).toBeLessThanOrEqual(1e-3);
      expect(Math.abs(actual.cy - expected.cy)).toBeLessThanOrEqual(1e-5);
      expect(Math.abs(actual.cl - expected.cl)).toBeLessThanOrEqual(1e-5);
      expect(Math.abs(actual.cm - expected.cm)).toBeLessThanOrEqual(1e-5);
      expect(Math.abs(actual.cn - expected.cn)).toBeLessThanOrEqual(1e-5);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

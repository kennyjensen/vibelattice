import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function parseNum(text) {
  const m = String(text || '').replace(/\u00a0/g, ' ').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return m ? Number(m[0]) : Number.NaN;
}

test('Xnp displays dimensional neutral point using EXEC reference point (circle case)', async ({ page }) => {
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.avl') || filePath.endsWith('.mass')) {
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
    await page.setViewportSize({ width: 1400, height: 900 });
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => {
      const log = document.getElementById('debugLog')?.textContent || '';
      return log.includes('App module loaded.') || log.includes('App module failed:');
    }, { timeout: 30000 });

    await expect(page.locator('#runCasesMeta')).toContainText('plane.run', { timeout: 30000 });

    const circleAvl = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'circle.avl'), 'utf8');
    await page.fill('#fileText', circleAvl);
    await page.dispatchEvent('#fileText', 'input');
    await page.setInputFiles('#runCasesInput', path.join(root, 'third_party', 'avl', 'runs', 'circle.run'));
    await expect(page.locator('#runCasesMeta')).toContainText('circle.run', { timeout: 30000 });

    await page.evaluate(() => { document.getElementById('trimBtn')?.click(); });
    await page.waitForFunction(() => {
      const txt = String(document.getElementById('outStabilityNeutral')?.textContent || '');
      return /Xnp\s*=\s*[-+]?\d/.test(txt.replace(/\u00a0/g, ' '));
    }, null, { timeout: 30000 });

    const values = await page.evaluate(() => {
      const cells = Array.from(document.querySelectorAll('#outStability .stability-cell'));
      const text = cells.map((el) => String(el.textContent || '').replace(/\s+/g, ' ').trim());
      const colCount = 6;
      const alphaRow = colCount;
      const exec = window.__trefftzTestHook?.getLastExecResult?.() || null;
      return {
        claText: text[alphaRow + 1] || '',
        cmaText: text[alphaRow + 4] || '',
        xnpText: String(document.getElementById('outStabilityNeutral')?.textContent || ''),
        xcgText: String(document.getElementById('massXcg')?.value || ''),
        cref: Number(exec?.CREF),
      };
    });

    const cla = parseNum(values.claText);
    const cma = parseNum(values.cmaText);
    const xnp = parseNum(values.xnpText);
    const cref = Number(values.cref);
    const xcg = parseNum(values.xcgText);

    expect(Number.isFinite(cla)).toBeTruthy();
    expect(Number.isFinite(cma)).toBeTruthy();
    expect(Number.isFinite(xnp)).toBeTruthy();
    expect(Number.isFinite(cref)).toBeTruthy();
    expect(Math.abs(cref)).toBeGreaterThan(1e-9);
    expect(Number.isFinite(xcg)).toBeTruthy();
    expect(Math.abs(cla)).toBeGreaterThan(1e-9);

    const expected = xcg - (cref * (cma / cla));
    expect(Math.abs(xnp - expected)).toBeLessThan(1e-3);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

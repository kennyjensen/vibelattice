import { test } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

test('tmp debug default trim', async ({ page }) => {
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
    await page.click('#trimBtn');
    await page.waitForTimeout(4000);
    const out = await page.evaluate(() => {
      const r = window.__trefftzTestHook?.getLastExecResult?.();
      return {
        debug: document.querySelector('#debugLog')?.textContent || '',
        keys: r ? Object.keys(r).slice(0, 50) : [],
        cd: r?.CDTOT,
        cl: r?.CLTOT,
        cftot: r?.CFTOT,
        cmtot: r?.CMTOT,
        stableCells: document.querySelectorAll('#outStability .stability-cell').length,
        bodyCells: document.querySelectorAll('#outBodyDeriv .stability-cell').length,
        surfCells: document.querySelectorAll('#outForcesSurface .stability-cell').length,
        hingeCells: document.querySelectorAll('#outHinge .stability-cell').length,
      };
    });
    console.log(JSON.stringify(out, null, 2));
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

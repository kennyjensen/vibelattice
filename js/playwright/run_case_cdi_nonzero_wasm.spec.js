import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function parseNum(text) {
  const match = String(text || '').replace(/\u00a0/g, ' ').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

test('plane.run has nonzero CDi with wasm-enabled EXEC path', async ({ page }) => {
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
    for (const entry of ['/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

      await page.evaluate(() => {
        const el = document.getElementById('useWasmExec');
        if (!el) return;
        el.checked = true;
        el.dispatchEvent(new Event('change', { bubbles: true }));
      });
      await page.setInputFiles('#runCasesInput', path.resolve('third_party/avl/runs/plane.run'));
      await page.evaluate(() => { document.getElementById('trimBtn')?.click(); });

      await page.waitForFunction(() => {
        const txt = String(document.getElementById('outCDind')?.textContent || '').trim();
        const summary = window.__trefftzTestHook?.getLastExecSummary?.();
        return txt && txt !== '-' && summary && Number.isFinite(summary.CDTOT) && Number.isFinite(summary.CDVTOT);
      });

      const values = await page.evaluate(() => ({
        cdiText: String(document.getElementById('outCDind')?.textContent || ''),
        summary: window.__trefftzTestHook?.getLastExecSummary?.() || null,
      }));
      const cdi = parseNum(values.cdiText);
      const cdiRaw = Number(values.summary?.CDTOT) - Number(values.summary?.CDVTOT);

      expect(Number.isFinite(cdiRaw)).toBeTruthy();
      expect(cdiRaw).toBeGreaterThan(0.001);
      expect(Number.isFinite(cdi)).toBeTruthy();
      expect(cdi).toBeGreaterThan(0.001);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

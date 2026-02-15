import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('flow cloud count stays constant when bref changes from 15 to 30', async ({ page }) => {
  test.setTimeout(60000);
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.setFlowFieldActive));

    const setBrefAndWait = async (bref) => {
      await page.locator('#fileBref').fill(String(bref));
      await page.locator('#fileBref').press('Enter');
      await page.waitForFunction((target) => {
        const el = document.getElementById('fileBref');
        const v = Number(el?.value);
        return Number.isFinite(v) && Math.abs(v - target) < 0.02;
      }, bref);
    };

    await setBrefAndWait(15);
    await page.evaluate(() => window.__trefftzTestHook.setFlowFieldActive?.(true));
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getFlowFieldStats?.();
      return Boolean(s && s.isTracer && Number.isFinite(s.count) && s.count > 100);
    });
    const count15 = await page.evaluate(() => window.__trefftzTestHook.getFlowFieldStats?.()?.count ?? null);
    expect(count15).toBe(961);

    await page.evaluate(() => window.__trefftzTestHook.setFlowFieldActive?.(false));
    await setBrefAndWait(30);
    await page.evaluate(() => window.__trefftzTestHook.setFlowFieldActive?.(true));
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getFlowFieldStats?.();
      return Boolean(s && s.isTracer && Number.isFinite(s.count) && s.count > 100);
    });
    const count30 = await page.evaluate(() => window.__trefftzTestHook.getFlowFieldStats?.()?.count ?? null);

    expect(count30).toBe(961);
    expect(count30).toBe(count15);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

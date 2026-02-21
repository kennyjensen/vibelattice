import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('surface labels scale with chord: supra labels larger than plane and 10-char width ~= 1 chord', async ({ page }) => {
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
    await page.selectOption('#loadExampleSelect', 'plane.avl');
    await expect(page.locator('#fileMeta')).toContainText('plane.avl', { timeout: 30000 });
    await expect.poll(async () => {
      const s = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
      return Number(s?.labelCount || 0);
    }, { timeout: 30000 }).toBeGreaterThan(0);
    const planeSummary = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);

    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect.poll(async () => {
      const s = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
      return Number(s?.labelCount || 0);
    }, { timeout: 30000 }).toBeGreaterThan(0);

    const supraSummary = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
    expect(planeSummary).toBeTruthy();
    expect(supraSummary).toBeTruthy();
    expect(Number(planeSummary.labelCount || 0)).toBeGreaterThan(0);
    expect(Number(supraSummary.labelCount || 0)).toBeGreaterThan(0);
    expect(Math.abs(Number(planeSummary.labelMaxWidth || 0) - Number(planeSummary.labelMinWidth || 0))).toBeLessThan(1e-6);
    expect(Math.abs(Number(supraSummary.labelMaxWidth || 0) - Number(supraSummary.labelMinWidth || 0))).toBeLessThan(1e-6);
    expect(Number(supraSummary.labelMaxWidth || 0)).toBeGreaterThan(Number(planeSummary.labelMaxWidth || 0) * 2.5);

    // 10-char rule: label width should be one chord scaled by canvas/text metric (~1.51 with Consolas 28px on 256px canvas).
    expect(Number(planeSummary.labelMinWidthToChord || 0)).toBeGreaterThan(1.3);
    expect(Number(planeSummary.labelMaxWidthToChord || 0)).toBeLessThan(1.8);
    expect(Number(supraSummary.labelMinWidthToChord || 0)).toBeGreaterThan(1.3);
    expect(Number(supraSummary.labelMaxWidthToChord || 0)).toBeLessThan(1.8);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

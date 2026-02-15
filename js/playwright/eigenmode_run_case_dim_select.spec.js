import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('inactive run-case eigenmodes are dimmed and not selectable', async ({ page }) => {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });
    await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

    await page.click('#runCaseAddBtn');
    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(2);
    await page.locator('#runCaseList .run-case-item').first().click();
    await expect(page.locator('#runCaseList .run-case-item').first()).toHaveClass(/active/);

    const points = await page.evaluate(() => {
      window.__trefftzTestHook.setEigenModes([
        { name: 'Case0', re: -0.20, im: 0.10, runCaseIndex: 0, vec: { rx: 0, ry: 0, rz: 0, tx: 0, ty: 0, tz: 0 } },
        { name: 'Case1', re: -0.05, im: 0.02, runCaseIndex: 1, vec: { rx: 0, ry: 0, rz: 0, tx: 0, ty: 0, tz: 0 } },
      ]);
      return window.__trefftzTestHook.getEigenPoints();
    });
    expect(points.length).toBeGreaterThanOrEqual(2);
    expect(points[0].selectable).toBeTruthy();
    expect(points[0].dimmed).toBeFalsy();
    expect(points[1].selectable).toBeFalsy();
    expect(points[1].dimmed).toBeTruthy();

    await page.evaluate((pt) => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new MouseEvent('click', {
        bubbles: true,
        clientX: rect.left + pt.x,
        clientY: rect.top + pt.y,
      }));
    }, points[1]);
    await page.waitForTimeout(150);
    await page.waitForFunction(() => window.__trefftzTestHook.getSelectedEigenMode() === -1);

    await page.evaluate((pt) => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new MouseEvent('click', {
        bubbles: true,
        clientX: rect.left + pt.x,
        clientY: rect.top + pt.y,
      }));
    }, points[0]);
    await page.waitForFunction((idx) => window.__trefftzTestHook.getSelectedEigenMode() === idx, points[0].idx);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('eigenmode panel sits between trefftz and AVL editor, and canvas click selects a mode', async ({ page }) => {
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

    await expect(page.locator('#eigenPlot')).toHaveCount(1);

    const order = await page.evaluate(() => {
      const trefftz = document.querySelector('#trefftz')?.closest('.stage');
      const eigen = document.querySelector('#eigenPlot')?.closest('.stage');
      const editor = document.querySelector('#fileEditor')?.closest('.panel');
      if (!trefftz || !eigen || !editor || !trefftz.parentElement) return null;
      const children = Array.from(trefftz.parentElement.children);
      return {
        trefftzIdx: children.indexOf(trefftz),
        eigenIdx: children.indexOf(eigen),
        editorIdx: children.indexOf(editor),
      };
    });

    expect(order).toBeTruthy();
    expect(order.eigenIdx).toBeGreaterThan(order.trefftzIdx);
    expect(order.eigenIdx).toBeLessThan(order.editorIdx);

    const trefftzPanel = page.locator('#trefftzPanel');
    const eigenPanel = page.locator('#eigenPanel');
    await expect(trefftzPanel.locator('.panel-toggle')).toHaveCount(1);
    await expect(eigenPanel.locator('.panel-toggle')).toHaveCount(1);

    await expect(page.locator('#trefftz')).toBeVisible();
    await trefftzPanel.locator('.panel-toggle').click();
    await expect(page.locator('#trefftz')).toBeHidden();
    await trefftzPanel.locator('.panel-toggle').click();
    await expect(page.locator('#trefftz')).toBeVisible();

    await expect(page.locator('#eigenPlot')).toBeVisible();
    await eigenPanel.locator('.panel-toggle').click();
    await expect(page.locator('#eigenPlot')).toBeHidden();
    await eigenPanel.locator('.panel-toggle').click();
    await expect(page.locator('#eigenPlot')).toBeVisible();

    await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');
    const points = await page.evaluate(() => {
      window.__trefftzTestHook.setEigenModes([
        { name: 'Test A', re: -0.2, im: 0.12, vec: { rx: 0.05, ry: 0, rz: 0, tx: 0, ty: 0, tz: 0 } },
        { name: 'Test B', re: -0.05, im: 0.02, vec: { rx: 0, ry: 0.05, rz: 0, tx: 0.1, ty: 0, tz: 0 } },
      ]);
      return window.__trefftzTestHook.getEigenPoints();
    });
    expect(Array.isArray(points)).toBeTruthy();
    expect(points.length).toBeGreaterThan(0);
    const activeCaseColor = await page.locator('#runCaseList .run-case-item').first().locator('.run-case-color').inputValue();
    expect(points[0].color).toBe(activeCaseColor.toLowerCase());
    const viewportBeforeZoomIn = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportBeforeZoomIn).toBeTruthy();

    const p = points[0];
    await page.evaluate(() => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new WheelEvent('wheel', {
        bubbles: true,
        cancelable: true,
        deltaY: -120,
        clientX: rect.left + rect.width * 0.8,
        clientY: rect.top + rect.height * 0.4,
      }));
    });
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, viewportBeforeZoomIn.zoom);
    const viewportAfterZoomIn = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportAfterZoomIn.maxRe).toBeLessThan(viewportBeforeZoomIn.maxRe);
    expect(viewportAfterZoomIn.xMid).toBeLessThan(viewportBeforeZoomIn.xMid);

    await page.evaluate(() => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      canvas.dispatchEvent(new WheelEvent('wheel', { bubbles: true, cancelable: true, deltaY: 120 }));
    });
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom < z, viewportAfterZoomIn.zoom);
    const viewportAfterWheelZoomOut = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());

    await page.evaluate(() => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const makeTouchEvent = (type, d) => {
        const ev = new Event(type, { bubbles: true, cancelable: true });
        Object.defineProperty(ev, 'touches', {
          value: [
            { clientX: 100, clientY: 100 },
            { clientX: 100 + d, clientY: 100 },
          ],
        });
        return ev;
      };
      canvas.dispatchEvent(makeTouchEvent('touchstart', 100));
      canvas.dispatchEvent(makeTouchEvent('touchmove', 140));
    });
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, viewportAfterWheelZoomOut.zoom);
    const pointsAfterZoom = await page.evaluate(() => window.__trefftzTestHook.getEigenPoints());
    const p2 = pointsAfterZoom[0] || points[0];
    expect(p2).toBeTruthy();

    await page.evaluate((point) => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new MouseEvent('click', {
        bubbles: true,
        clientX: rect.left + point.x,
        clientY: rect.top + point.y,
      }));
    }, p2);

    await page.waitForFunction((idx) => window.__trefftzTestHook.getSelectedEigenMode() === idx, p2.idx);

    await page.evaluate((point) => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new MouseEvent('click', {
        bubbles: true,
        clientX: rect.left + point.x,
        clientY: rect.top + point.y,
      }));
    }, p2);
    await page.waitForFunction(() => window.__trefftzTestHook.getSelectedEigenMode() === -1);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

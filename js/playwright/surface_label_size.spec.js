import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('surface labels default on, toggle with T button, and long names are not clipped', async ({ page }) => {
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
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getSurfaceVisualState));
    await expect.poll(async () => {
      const s = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
      return Number(s?.labelCount || 0);
    }, { timeout: 30000 }).toBeGreaterThan(0);

    await expect(page.locator('#viewerText')).toHaveCount(1);
    await expect(page.locator('#viewerText')).toHaveClass(/active/);
    await expect.poll(async () => {
      const s = await page.evaluate(() => window.__trefftzTestHook?.getViewerOverlayState?.() || null);
      return Boolean(s?.showSurfaceText);
    }).toBe(true);

    const longName = 'VERY_LONG_SURFACE_NAME_FOR_LABEL_FIT_TEST_1234567890';
    const changed = await page.evaluate((nextName) => {
      const editor = document.getElementById('fileText');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      const lines = String(editor.value || '').split(/\r?\n/);
      const idx = lines.findIndex((line) => line.trim().toUpperCase() === 'SURFACE');
      if (idx < 0 || idx + 1 >= lines.length) return false;
      lines[idx + 1] = nextName;
      editor.value = lines.join('\n');
      editor.dispatchEvent(new Event('input', { bubbles: true }));
      return true;
    }, longName);
    expect(changed).toBeTruthy();

    await expect.poll(async () => {
      const s = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
      const labels = Array.isArray(s?.labels) ? s.labels : [];
      return labels.some((entry) => String(entry?.text || '') === longName);
    }, { timeout: 30000 }).toBe(true);

    const summary = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
    expect(summary).toBeTruthy();
    expect(Number(summary.labelCount || 0)).toBeGreaterThan(0);
    expect(Number(summary.labelVisibleCount || 0)).toBe(Number(summary.labelCount || 0));
    expect(Number(summary.labelMaxTextLength || 0)).toBeGreaterThanOrEqual(longName.length);
    expect(Number(summary.labelClipFailures || 0)).toBe(0);

    const labelCanvasCheck = await page.evaluate((font) => {
      const state = window.__trefftzTestHook?.getSurfaceVisualState?.() || {};
      const labels = Array.isArray(state?.labels) ? state.labels : [];
      const canvas = document.createElement('canvas');
      const ctx = canvas.getContext('2d');
      if (ctx) ctx.font = font;
      return labels.map((entry) => {
        const text = String(entry?.text || '');
        const measured = ctx ? Number(ctx.measureText(text).width) : Number.NaN;
        const canvasWidth = Number(entry?.canvasWidthPx);
        const padding = Number(entry?.paddingXPx);
        const safePadding = Number.isFinite(padding) ? padding : 24;
        const usable = Number.isFinite(canvasWidth) ? Math.max(0, canvasWidth - (safePadding * 2)) : Number.NaN;
        return {
          text,
          measured,
          canvasWidth,
          usable,
          fits: Number.isFinite(measured) && Number.isFinite(usable) ? (measured <= usable + 0.5) : false,
        };
      });
    }, '28px Consolas, "Courier New", monospace');
    const longEntries = labelCanvasCheck.filter((entry) => entry.text === longName);
    expect(longEntries.length).toBeGreaterThan(0);
    expect(longEntries.every((entry) => entry.fits)).toBeTruthy();

    await page.click('#viewerText');
    await expect(page.locator('#viewerText')).not.toHaveClass(/active/);
    await expect.poll(async () => {
      const [overlay, state] = await page.evaluate(() => [
        window.__trefftzTestHook?.getViewerOverlayState?.() || null,
        window.__trefftzTestHook?.getSurfaceVisualState?.() || null,
      ]);
      return {
        showSurfaceText: Boolean(overlay?.showSurfaceText),
        labelVisibleCount: Number(state?.labelVisibleCount || 0),
      };
    }).toEqual({ showSurfaceText: false, labelVisibleCount: 0 });

    await page.click('#viewerText');
    await expect(page.locator('#viewerText')).toHaveClass(/active/);
    await expect.poll(async () => {
      const [overlay, state] = await page.evaluate(() => [
        window.__trefftzTestHook?.getViewerOverlayState?.() || null,
        window.__trefftzTestHook?.getSurfaceVisualState?.() || null,
      ]);
      return {
        showSurfaceText: Boolean(overlay?.showSurfaceText),
        labelVisibleCount: Number(state?.labelVisibleCount || 0),
        labelCount: Number(state?.labelCount || 0),
      };
    }).toEqual(expect.objectContaining({ showSurfaceText: true }));
    const onState = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
    expect(Number(onState.labelVisibleCount || 0)).toBe(Number(onState.labelCount || 0));
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

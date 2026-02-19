import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('plane.avl draws multiple spanwise lattice/vortex segments in 3D overlays', async ({ page }) => {
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
    for (const entry of ['/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState));
      await page.waitForFunction(() => {
        const el = document.getElementById('fileNVort');
        return String(el?.textContent || '').trim() === '56';
      });

      await page.click('#viewerPanelSpacing');
      await page.click('#viewerVortices');

      await page.waitForFunction(() => {
        const state = window.__trefftzTestHook.getViewerOverlayState?.();
        return Boolean(
          state
          && state.hasPanelSpacing
          && state.hasVortices
          && state.panelSpacingLineSegments >= 45
          && state.vortexBoundSegments >= 30,
        );
      });

      const state = await page.evaluate(() => window.__trefftzTestHook.getViewerOverlayState?.());
      expect(state).toBeTruthy();
      expect(state.panelSpacingLineSegments).toBeGreaterThanOrEqual(45);
      expect(state.vortexBoundSegments).toBeGreaterThanOrEqual(30);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

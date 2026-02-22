import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('viewer right-side button order is stable and pressure stays in bottom row', async ({ page }) => {
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
    const entrypoints = ['/index.html'];
    const expectedOrder = [
      'viewerHome',
      'viewerZoomIn',
      'viewerZoomOut',
      'viewerPan',
      'viewerView',
      'viewerGrid',
      'viewerText',
    ];
    const expectedBottomOrder = [
      'viewerSurface',
      'viewerPanelSpacing',
      'viewerVortices',
      'viewerPressure',
      'viewerLoad',
      'viewerFlow',
    ];

    for (const entry of entrypoints) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });

      const topIds = await page.locator('.viewer-overlay > button').evaluateAll((nodes) =>
        nodes.map((n) => n.id),
      );
      expect(topIds.slice(0, expectedOrder.length)).toEqual(expectedOrder);
      const bottomIds = await page.locator('.viewer-overlay-bottom > button').evaluateAll((nodes) =>
        nodes.map((n) => n.id),
      );
      expect(bottomIds).toEqual(expectedBottomOrder);

      await expect(page.locator('.viewer-overlay-bottom #viewerPressure')).toHaveCount(1);
      await expect(page.locator('.viewer-overlay #viewerPressure')).toHaveCount(0);
      await expect(page.locator('.viewer-overlay-bottom #viewerLoad')).toHaveCount(1);
      await expect(page.locator('.viewer-overlay #viewerLoad')).toHaveCount(0);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

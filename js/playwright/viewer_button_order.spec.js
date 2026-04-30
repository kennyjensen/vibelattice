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
      'viewerProjection',
      'viewerQuad',
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
      'eigenDivergence',
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
      await expect(page.locator('.viewer-overlay-bottom #eigenDivergence')).toHaveCount(1);
      await expect(page.locator('.eigen-overlay #eigenDivergence')).toHaveCount(0);

      const allViewerButtonIds = [...topIds, ...bottomIds];
      const tooltipState = await page.evaluate((ids) => ids.map((id) => {
        const node = document.getElementById(id);
        return {
          id,
          title: node?.getAttribute('title') || '',
          ariaLabel: node?.getAttribute('aria-label') || '',
          tooltip: node?.getAttribute('data-tooltip') || '',
        };
      }), allViewerButtonIds);
      expect(tooltipState.every((item) => item.title.length === 0)).toBeTruthy();
      expect(tooltipState.every((item) => item.ariaLabel.length > 0)).toBeTruthy();
      expect(tooltipState.every((item) => item.tooltip === item.ariaLabel)).toBeTruthy();

      await page.hover('#viewerHome');
      const instantTooltip = await page.evaluate(() => {
        const node = document.getElementById('viewerHome');
        const style = window.getComputedStyle(node, '::after');
        return {
          content: style.content,
          opacity: style.opacity,
          transitionDelay: style.transitionDelay,
          transitionDuration: style.transitionDuration,
          visibility: style.visibility,
        };
      });
      expect(instantTooltip.content).toContain('Fit to model');
      expect(instantTooltip.opacity).toBe('1');
      expect(instantTooltip.visibility).toBe('visible');
      expect(instantTooltip.transitionDelay).toBe('0s');
      expect(instantTooltip.transitionDuration).toBe('0s');

      await page.hover('#viewerSurface');
      const bottomTooltip = await page.evaluate(() => {
        const node = document.getElementById('viewerSurface');
        const style = window.getComputedStyle(node, '::after');
        return {
          content: style.content,
          left: style.left,
          transform: style.transform,
        };
      });
      expect(bottomTooltip.content).toContain('Render: wireframe only');
      expect(bottomTooltip.left).toBe('0px');
      expect(bottomTooltip.transform).toBe('none');
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

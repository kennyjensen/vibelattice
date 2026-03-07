import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

async function startStaticServer() {
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
      else if (filePath.endsWith('.avl') || filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.dat')) {
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
  return { server, port };
}

async function loadSimpleTrefftz(page) {
  await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.setTrefftzData));
  await page.evaluate(() => {
    window.__trefftzTestHook?.setTrefftzData?.({
      cref: 1.0,
      strips: [
        [-1.0, 0.0, 0.10, 0.20, 0.25, -0.03],
        [0.0, 0.0, 0.20, 0.40, 0.45, -0.05],
        [1.0, 0.0, 0.30, 0.60, 0.65, -0.07],
      ],
      surfaces: [{ start: 0, count: 3 }],
    });
  });
  await expect.poll(async () => {
    const state = await page.evaluate(() => ({
      plot: window.__trefftzTestHook?.trefftzPlotBounds || null,
      legend: document.querySelector('.trefftz-legend') ? 1 : 0,
    }));
    return Boolean(state.plot) && state.legend === 1;
  }, { timeout: 20000 }).toBe(true);
}

test('Trefftz and Eigenmodes panels have matching plot height, legend aligns left to plot, and hover shows span + markers', async ({ page }) => {
  const { server, port } = await startStaticServer();
  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await loadSimpleTrefftz(page);

    const sizing = await page.evaluate(() => {
      const trefftz = document.getElementById('trefftz')?.getBoundingClientRect();
      const eigen = document.getElementById('eigenPlot')?.getBoundingClientRect();
      const panel = document.getElementById('trefftz')?.parentElement?.getBoundingClientRect();
      const legend = document.querySelector('.trefftz-legend')?.getBoundingClientRect();
      const bounds = window.__trefftzTestHook?.trefftzPlotBounds || null;
      const plotLeft = bounds ? Number(bounds.x0) : Number.NaN;
      const legendLeft = (panel && legend) ? (legend.left - panel.left) : Number.NaN;
      return {
        trefftzHeight: Number(trefftz?.height || 0),
        eigenHeight: Number(eigen?.height || 0),
        plotLeft,
        legendLeft,
      };
    });
    expect(sizing.trefftzHeight).toBeGreaterThan(0);
    expect(sizing.eigenHeight).toBeGreaterThan(0);
    expect(Math.abs(sizing.trefftzHeight - sizing.eigenHeight)).toBeLessThanOrEqual(1);
    expect(Math.abs(sizing.legendLeft - sizing.plotLeft)).toBeLessThanOrEqual(2.5);

    const hoverPoint = await page.evaluate(() => {
      const canvas = document.getElementById('trefftz');
      const rect = canvas?.getBoundingClientRect();
      const bounds = window.__trefftzTestHook?.trefftzPlotBounds || null;
      if (!rect || !bounds) return null;
      return {
        x: rect.left + ((Number(bounds.x0) + Number(bounds.x1)) * 0.5),
        y: rect.top + ((Number(bounds.y0) + Number(bounds.y1)) * 0.5),
      };
    });
    expect(hoverPoint).toBeTruthy();

    await page.dispatchEvent('#trefftz', 'pointermove', {
      clientX: hoverPoint.x,
      clientY: hoverPoint.y,
      pointerType: 'mouse',
      buttons: 0,
    });

    await expect.poll(async () => {
      const hover = await page.evaluate(() => window.__trefftzTestHook?.trefftzHover || null);
      const spanLabel = String(hover?.spanLabel || '');
      return {
        active: Boolean(hover?.active),
        markerCount: Number(hover?.markerCount || 0),
        hasNumericSpanLabel: /^[-+]?\d+(\.\d+)?$/.test(spanLabel),
      };
    }, { timeout: 20000 }).toEqual({
      active: true,
      markerCount: 4,
      hasNumericSpanLabel: true,
    });
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

test.describe('mobile touch hover', () => {
  test.use({
    viewport: { width: 390, height: 844 },
    isMobile: true,
    hasTouch: true,
  });

  test('Trefftz hover remains visible after touch release on mobile', async ({ page }) => {
    const { server, port } = await startStaticServer();
    try {
      await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
      await loadSimpleTrefftz(page);

      const hoverPoint = await page.evaluate(() => {
        const canvas = document.getElementById('trefftz');
        const rect = canvas?.getBoundingClientRect();
        const bounds = window.__trefftzTestHook?.trefftzPlotBounds || null;
        if (!rect || !bounds) return null;
        return {
          x: rect.left + ((Number(bounds.x0) + Number(bounds.x1)) * 0.55),
          y: rect.top + ((Number(bounds.y0) + Number(bounds.y1)) * 0.45),
        };
      });
      expect(hoverPoint).toBeTruthy();

      await page.dispatchEvent('#trefftz', 'pointermove', {
        clientX: hoverPoint.x,
        clientY: hoverPoint.y,
        pointerType: 'touch',
        buttons: 1,
      });
      await expect.poll(async () => {
        const hover = await page.evaluate(() => window.__trefftzTestHook?.trefftzHover || null);
        return Boolean(hover?.active);
      }, { timeout: 20000 }).toBe(true);

      await page.dispatchEvent('#trefftz', 'pointerleave', {
        clientX: hoverPoint.x,
        clientY: hoverPoint.y,
        pointerType: 'touch',
        buttons: 0,
      });
      await page.waitForTimeout(120);

      const persisted = await page.evaluate(() => window.__trefftzTestHook?.trefftzHover || null);
      expect(Boolean(persisted?.active)).toBe(true);
      expect(Number(persisted?.markerCount || 0)).toBeGreaterThan(0);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
});

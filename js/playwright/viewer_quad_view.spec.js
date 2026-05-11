import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function paneDistance(pane) {
  const pos = pane?.cameraPosition;
  const target = pane?.target;
  if (!pos || !target) return NaN;
  return Math.hypot(
    Number(pos.x) - Number(target.x),
    Number(pos.y) - Number(target.y),
    Number(pos.z) - Number(target.z),
  );
}

test('viewer quad mode cycles scaled, fit, then single iso layouts', async ({ page }) => {
  await page.setViewportSize({ width: 1500, height: 1200 });
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
    await page.goto(`http://127.0.0.1:${port}/index.html?debug=1`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => {
      const log = document.getElementById('debugLog')?.textContent || '';
      return log.includes('App module loaded.') || log.includes('App module failed:');
    }, { timeout: 20000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerQuadState));

    const beforeBox = await page.locator('#viewer').boundingBox();
    expect(beforeBox).toBeTruthy();
    let quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(false);
    expect(quadState.layoutMode).toBe('single');
    expect(quadState.projectionMode).toBe('perspective');

    await page.click('#viewerQuad');
    await expect(page.locator('#viewer')).toHaveClass(/quad-view/);
    await expect(page.locator('#viewer')).toHaveClass(/quad-view-scale/);
    await expect(page.locator('#viewerQuad')).toHaveClass(/active/);
    await expect(page.locator('#viewerQuad')).toHaveAttribute('data-tooltip', /scaled 3-view/);
    await expect(page.locator('#viewerQuadOverlay')).toBeVisible();
    await expect(page.locator('.viewer-quad-label-top')).toHaveText('Top');
    await expect(page.locator('.viewer-quad-label-side')).toHaveText('Side');
    await expect(page.locator('.viewer-quad-label-front')).toHaveText('Front');
    await expect(page.locator('.viewer-quad-label-iso')).toHaveText('Iso');

    const afterBox = await page.locator('#viewer').boundingBox();
    expect(afterBox.height).toBeGreaterThan(beforeBox.height * 1.7);

    const paneCenters = {
      side: { x: afterBox.x + afterBox.width * 0.25, y: afterBox.y + afterBox.height * 0.25 },
      front: { x: afterBox.x + afterBox.width * 0.75, y: afterBox.y + afterBox.height * 0.25 },
      top: { x: afterBox.x + afterBox.width * 0.25, y: afterBox.y + afterBox.height * 0.75 },
      iso: { x: afterBox.x + afterBox.width * 0.75, y: afterBox.y + afterBox.height * 0.75 },
    };

    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(true);
    expect(quadState.layoutMode).toBe('quad-scale');
    expect(quadState.overlayHidden).toBe(false);
    expect(quadState.paneLabels).toEqual(['Side', 'Front', 'Top', 'Iso']);
    expect(quadState.panes.map((pane) => pane.id)).toEqual(['side', 'front', 'top', 'iso']);
    expect(quadState.panes.map((pane) => [pane.id, pane.col, pane.row])).toEqual([
      ['side', 0, 0],
      ['front', 1, 0],
      ['top', 0, 1],
      ['iso', 1, 1],
    ]);
    const sidePane = quadState.panes.find((pane) => pane.id === 'side');
    const frontPane = quadState.panes.find((pane) => pane.id === 'front');
    const topPane = quadState.panes.find((pane) => pane.id === 'top');
    expect(sidePane.mode).toBe('side-left');
    expect(Number(sidePane.cameraPosition.y)).toBeLessThan(Number(sidePane.target.y));
    expect(Number(sidePane.cameraUp.z)).toBeGreaterThan(0.9);
    expect(frontPane.mode).toBe('front');
    expect(Number(frontPane.cameraPosition.x)).toBeLessThan(Number(frontPane.target.x));
    expect(Number(frontPane.cameraUp.z)).toBeGreaterThan(0.9);
    expect(Number(topPane.cameraPosition.z)).toBeGreaterThan(Number(topPane.target.z));
    expect(Number(topPane.cameraUp.y)).toBeGreaterThan(0.9);
    expect(quadState.panes.filter((pane) => pane.id !== 'iso').every((pane) => pane.cameraType === 'OrthographicCamera')).toBeTruthy();
    expect(quadState.panes.filter((pane) => pane.id !== 'iso').every((pane) => pane.projectionMode === 'orthographic')).toBeTruthy();
    expect(quadState.panes.filter((pane) => pane.id !== 'iso').every((pane) => pane.scaleMode === 'shared-scale')).toBeTruthy();
    expect(quadState.panes.find((pane) => pane.id === 'iso').cameraType).toBe('PerspectiveCamera');
    expect(quadState.panes.find((pane) => pane.id === 'iso').projectionMode).toBe('perspective');

    const beforeScaledZoom = quadState.panes
      .filter((pane) => pane.id !== 'iso')
      .map((pane) => Number(pane.paneZoom));
    expect(beforeScaledZoom.every((value) => Number.isFinite(value) && value > 0)).toBeTruthy();
    const beforeScaledWorldUnits = quadState.panes
      .filter((pane) => pane.id !== 'iso')
      .map((pane) => Number(pane.worldUnitsPerPixel));
    expect(beforeScaledWorldUnits.every((value) => Number.isFinite(value) && value > 0)).toBeTruthy();
    expect(Math.max(...beforeScaledWorldUnits) - Math.min(...beforeScaledWorldUnits)).toBeLessThan(1e-9);
    const isoScaleDistanceBefore = paneDistance(quadState.panes.find((pane) => pane.id === 'iso'));

    await page.mouse.move(paneCenters.top.x, paneCenters.top.y);
    await page.mouse.wheel(0, -360);
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    const afterTopZoom = quadState.panes
      .filter((pane) => pane.id !== 'iso')
      .map((pane) => Number(pane.paneZoom));
    const afterTopWorldUnits = quadState.panes
      .filter((pane) => pane.id !== 'iso')
      .map((pane) => Number(pane.worldUnitsPerPixel));
    expect(Math.max(...afterTopZoom) - Math.min(...afterTopZoom)).toBeLessThan(1e-9);
    expect(afterTopZoom.every((value) => value > beforeScaledZoom[0])).toBeTruthy();
    expect(Math.max(...afterTopWorldUnits) - Math.min(...afterTopWorldUnits)).toBeLessThan(1e-9);
    expect(afterTopWorldUnits.every((value, index) => value < beforeScaledWorldUnits[index])).toBeTruthy();
    expect(paneDistance(quadState.panes.find((pane) => pane.id === 'iso'))).toBeCloseTo(isoScaleDistanceBefore, 6);

    await page.mouse.move(paneCenters.side.x, paneCenters.side.y);
    await page.mouse.wheel(0, -240);
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    const afterSideZoom = quadState.panes
      .filter((pane) => pane.id !== 'iso')
      .map((pane) => Number(pane.paneZoom));
    expect(Math.max(...afterSideZoom) - Math.min(...afterSideZoom)).toBeLessThan(1e-9);
    expect(afterSideZoom.every((value) => value > afterTopZoom[0])).toBeTruthy();
    expect(paneDistance(quadState.panes.find((pane) => pane.id === 'iso'))).toBeCloseTo(isoScaleDistanceBefore, 6);

    const isoBefore = await page.evaluate(() => {
      const state = window.__trefftzTestHook.getViewerQuadState();
      return state.panes.find((pane) => pane.id === 'iso');
    });
    const isoDistanceBefore = paneDistance(isoBefore);
    expect(Number.isFinite(isoDistanceBefore)).toBeTruthy();
    await page.mouse.move(paneCenters.iso.x, paneCenters.iso.y);
    await page.mouse.wheel(0, -600);
    await page.waitForFunction((before) => {
      const state = window.__trefftzTestHook.getViewerQuadState();
      const pane = state.panes.find((entry) => entry.id === 'iso');
      const pos = pane?.cameraPosition;
      const target = pane?.target;
      if (!pos || !target) return false;
      const distance = Math.hypot(
        Number(pos.x) - Number(target.x),
        Number(pos.y) - Number(target.y),
        Number(pos.z) - Number(target.z),
      );
      return Number.isFinite(distance) && distance < before * 0.97;
    }, isoDistanceBefore, { timeout: 5000 });
    const isoAfter = await page.evaluate(() => {
      const state = window.__trefftzTestHook.getViewerQuadState();
      return state.panes.find((pane) => pane.id === 'iso');
    });
    expect(paneDistance(isoAfter)).toBeLessThan(isoDistanceBefore * 0.97);

    await page.click('#viewerProjection');
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(true);
    expect(quadState.projectionMode).toBe('orthographic');
    expect(quadState.panes.every((pane) => pane.cameraType === 'OrthographicCamera')).toBeTruthy();
    expect(quadState.panes.find((pane) => pane.id === 'iso').projectionMode).toBe('orthographic');
    await expect(page.locator('#viewerProjection')).toHaveText('O');

    await page.click('#viewerProjection');
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(true);
    expect(quadState.projectionMode).toBe('perspective');
    expect(quadState.panes.filter((pane) => pane.id !== 'iso').every((pane) => pane.cameraType === 'OrthographicCamera')).toBeTruthy();
    expect(quadState.panes.find((pane) => pane.id === 'iso').cameraType).toBe('PerspectiveCamera');

    await page.click('#viewerQuad');
    await expect(page.locator('#viewer')).toHaveClass(/quad-view-fit/);
    await expect(page.locator('#viewerQuad')).toHaveAttribute('data-tooltip', /fitted panes/);
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(true);
    expect(quadState.layoutMode).toBe('quad-fit');
    expect(quadState.panes.filter((pane) => pane.id !== 'iso').every((pane) => pane.scaleMode === 'pane-fit')).toBeTruthy();
    expect(quadState.panes.find((pane) => pane.id === 'iso').cameraType).toBe('PerspectiveCamera');

    for (const [paneId, point] of Object.entries(paneCenters).filter(([paneId]) => paneId !== 'iso')) {
      const beforePane = (await page.evaluate((id) => {
        const state = window.__trefftzTestHook.getViewerQuadState();
        return state.panes.find((pane) => pane.id === id);
      }, paneId));
      await page.mouse.move(point.x, point.y);
      await page.mouse.wheel(0, -360);
      await page.mouse.down({ button: 'left' });
      await page.mouse.move(point.x + 50, point.y + 30, { steps: 4 });
      await page.mouse.up({ button: 'left' });
      const afterPane = (await page.evaluate((id) => {
        const state = window.__trefftzTestHook.getViewerQuadState();
        return state.panes.find((pane) => pane.id === id);
      }, paneId));
      expect(afterPane.scaleMode).toBe('pane-fit');
      expect(Number(afterPane.paneZoom)).toBeGreaterThan(Number(beforePane.paneZoom));
      const offsetDelta = Math.abs(Number(afterPane.paneOffsetX) - Number(beforePane.paneOffsetX))
        + Math.abs(Number(afterPane.paneOffsetY) - Number(beforePane.paneOffsetY));
      expect(offsetDelta).toBeGreaterThan(1e-4);
      if (paneId === 'side') {
        expect(Number(afterPane.target?.x)).toBeLessThan(Number(beforePane.target?.x));
      }
    }

    await page.click('#viewerView');
    let viewState = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
    expect(viewState.mode).not.toBe('iso');

    await page.click('#viewerQuad');
    await expect(page.locator('#viewer')).not.toHaveClass(/quad-view/);
    await expect(page.locator('#viewerQuadOverlay')).toBeHidden();
    quadState = await page.evaluate(() => window.__trefftzTestHook.getViewerQuadState());
    expect(quadState.enabled).toBe(false);
    expect(quadState.layoutMode).toBe('single');
    viewState = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
    expect(viewState.mode).toBe('iso');
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

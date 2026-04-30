import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('view button cycles top, forward, side presets with matching camera axes', async ({ page }) => {
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
    for (const entry of entrypoints) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await page.waitForFunction(() => {
        const log = document.getElementById('debugLog')?.textContent || '';
        return log.includes('App module loaded.') || log.includes('App module failed:');
      }, { timeout: 20000 });
      await page.waitForFunction(() => Boolean(
        window.__trefftzTestHook?.getViewerViewState
        && window.__trefftzTestHook?.getViewerMouseControlState,
      ));
      await page.waitForFunction(() => (
        window.__trefftzTestHook.getViewerMouseControlState().left === 'PAN'
      ), { timeout: 20000 });

      const mouseControls = await page.evaluate(() => window.__trefftzTestHook.getViewerMouseControlState());
      expect(mouseControls.left).toBe('PAN');
      expect(mouseControls.middle).toBeNull();
      expect(mouseControls.right).toBe('ROTATE');

      const canvasBox = await page.locator('#viewer canvas').boundingBox();
      expect(canvasBox).toBeTruthy();
      const dragX = canvasBox.x + canvasBox.width * 0.5;
      const dragY = canvasBox.y + canvasBox.height * 0.5;
      const zBefore = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      await page.mouse.move(dragX, dragY);
      await page.mouse.down({ button: 'middle' });
      await page.mouse.move(dragX, dragY - 80);
      await page.mouse.up({ button: 'middle' });
      const zAfter = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(Number(zAfter.cameraPosition.z)).toBeLessThan(Number(zBefore.cameraPosition.z));
      expect(Number(zAfter.controlsTarget.z)).toBeLessThan(Number(zBefore.controlsTarget.z));

      const horizontalBefore = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      await page.mouse.move(dragX, dragY);
      await page.mouse.down({ button: 'middle' });
      await page.mouse.move(dragX + 90, dragY);
      await page.mouse.up({ button: 'middle' });
      const horizontalAfter = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      const targetPan = Math.hypot(
        Number(horizontalAfter.controlsTarget.x) - Number(horizontalBefore.controlsTarget.x),
        Number(horizontalAfter.controlsTarget.y) - Number(horizontalBefore.controlsTarget.y),
      );
      const cameraPan = Math.hypot(
        Number(horizontalAfter.cameraPosition.x) - Number(horizontalBefore.cameraPosition.x),
        Number(horizontalAfter.cameraPosition.y) - Number(horizontalBefore.cameraPosition.y),
      );
      expect(targetPan).toBeGreaterThan(1e-4);
      expect(cameraPan).toBeGreaterThan(1e-4);
      expect(Math.abs(Number(horizontalAfter.controlsTarget.z) - Number(horizontalBefore.controlsTarget.z))).toBeLessThan(1e-8);
      expect(Math.abs(Number(horizontalAfter.cameraPosition.z) - Number(horizontalBefore.cameraPosition.z))).toBeLessThan(1e-8);

      await page.click('#viewerView');
      let state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('top');
      expect(state.projectionMode).toBe('perspective');
      expect(state.cameraType).toBe('PerspectiveCamera');
      if (state.cameraPosition) {
        expect(Math.abs(state.cameraPosition.z)).toBeGreaterThan(Math.abs(state.cameraPosition.x));
        expect(Math.abs(state.cameraPosition.z)).toBeGreaterThan(Math.abs(state.cameraPosition.y));
      }
      await expect(page.locator('#viewerView')).toHaveAttribute('data-tooltip', /Top \(down\)/);

      await page.click('#viewerView');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('forward');
      if (state.cameraPosition) {
        expect(Math.abs(state.cameraPosition.x)).toBeGreaterThan(Math.abs(state.cameraPosition.y));
        expect(Math.abs(state.cameraPosition.x)).toBeGreaterThan(Math.abs(state.cameraPosition.z));
      }
      await expect(page.locator('#viewerView')).toHaveAttribute('data-tooltip', /Forward \(aft\)/);

      await page.click('#viewerView');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('side');
      if (state.cameraPosition) {
        expect(Math.abs(state.cameraPosition.y)).toBeGreaterThan(Math.abs(state.cameraPosition.x));
        expect(Math.abs(state.cameraPosition.y)).toBeGreaterThan(Math.abs(state.cameraPosition.z));
      }
      await expect(page.locator('#viewerView')).toHaveAttribute('data-tooltip', /Side \(Y axis\)/);

      await page.click('#viewerView');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.mode).toBe('iso');
      if (state.cameraPosition) {
        expect(Math.abs(state.cameraPosition.x)).toBeGreaterThan(0.1);
        expect(Math.abs(state.cameraPosition.y)).toBeGreaterThan(0.1);
        expect(Math.abs(state.cameraPosition.z)).toBeGreaterThan(0.1);
      }
      await expect(page.locator('#viewerView')).toHaveAttribute('data-tooltip', /Isometric/);

      await page.click('#viewerProjection');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.projectionMode).toBe('orthographic');
      expect(state.cameraType).toBe('OrthographicCamera');
      expect(state.mode).toBe('iso');
      await expect(page.locator('#viewerProjection')).toHaveText('O');
      await expect(page.locator('#viewerProjection')).toHaveClass(/active/);
      await expect(page.locator('#viewerProjection')).toHaveAttribute('data-tooltip', /orthographic/);

      const zoomBefore = Number(state.cameraZoom);
      await page.click('#viewerZoomIn');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.projectionMode).toBe('orthographic');
      expect(Number(state.cameraZoom)).toBeGreaterThan(zoomBefore);

      await page.click('#viewerProjection');
      state = await page.evaluate(() => window.__trefftzTestHook.getViewerViewState());
      expect(state.projectionMode).toBe('perspective');
      expect(state.cameraType).toBe('PerspectiveCamera');
      await expect(page.locator('#viewerProjection')).toHaveText('P');
      await expect(page.locator('#viewerProjection')).not.toHaveClass(/active/);
      await expect(page.locator('#viewerProjection')).toHaveAttribute('data-tooltip', /perspective/);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

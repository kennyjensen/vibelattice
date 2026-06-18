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
    await expect(page.locator('#eigenHome')).toHaveCount(1);
    await expect(page.locator('#eigenZoomIn')).toHaveCount(1);
    await expect(page.locator('#eigenZoomOut')).toHaveCount(1);
    await expect(page.locator('#eigenPan')).toHaveCount(1);
    await expect(page.locator('#eigenScaleLock')).toHaveCount(1);
    await expect(page.locator('#eigenDivergence')).toHaveCount(1);
    await expect(page.locator('#eigenDownload')).toHaveCount(1);
    const eigenButtonIds = ['eigenHome', 'eigenZoomIn', 'eigenZoomOut', 'eigenPan', 'eigenScaleLock', 'eigenDownload'];
    const eigenTooltipState = await page.evaluate((ids) => ids.map((id) => {
      const node = document.getElementById(id);
      return {
        id,
        title: node?.getAttribute('title') || '',
        ariaLabel: node?.getAttribute('aria-label') || '',
        tooltip: node?.getAttribute('data-tooltip') || '',
      };
    }), eigenButtonIds);
    expect(eigenTooltipState.every((item) => item.title.length === 0)).toBeTruthy();
    expect(eigenTooltipState.every((item) => item.ariaLabel.length > 0)).toBeTruthy();
    expect(eigenTooltipState.every((item) => item.tooltip === item.ariaLabel)).toBeTruthy();
    await page.hover('#eigenHome');
    const eigenInstantTooltip = await page.evaluate(() => {
      const node = document.getElementById('eigenHome');
      const style = window.getComputedStyle(node, '::after');
      return {
        content: style.content,
        opacity: style.opacity,
        transitionDelay: style.transitionDelay,
        transitionDuration: style.transitionDuration,
        visibility: style.visibility,
      };
    });
    expect(eigenInstantTooltip.content).toContain('Reset eigenmode view');
    expect(eigenInstantTooltip.opacity).toBe('1');
    expect(eigenInstantTooltip.visibility).toBe('visible');
    expect(eigenInstantTooltip.transitionDelay).toBe('0s');
    expect(eigenInstantTooltip.transitionDuration).toBe('0s');

    await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook.getLastExecResult?.()), null, { timeout: 15000 }).catch(() => {});
    const points = await page.evaluate(() => {
      const stateOrder = ['u', 'w', 'q', 'theta', 'v', 'p', 'r', 'phi', 'x', 'y', 'z', 'psi'];
      const eigenvector = (components) => ({
        re: stateOrder.map((key) => Number(components[key]) || 0),
        im: stateOrder.map(() => 0),
      });
      window.__trefftzTestHook.setRunCasesForTest([
        { name: 'Case Alpha', color: '#ff5500' },
        { name: 'Case Beta', color: '#00aa88' },
      ], 0);
      window.__trefftzTestHook.setEigenModes([
        {
          name: 'Test A',
          re: -0.2,
          im: 0.12,
          runCaseIndex: 0,
          stateOrder,
          eigenvector: eigenvector({ q: 1, theta: 0.8, w: 0.5 }),
          vec: { rx: 0, ry: 0.05, rz: 0, tx: 0, ty: 0, tz: 0 },
        },
        {
          name: 'Test A conjugate',
          re: -0.2,
          im: -0.12,
          runCaseIndex: 0,
          stateOrder,
          eigenvector: eigenvector({ q: 1, theta: 0.8, w: 0.5 }),
          vec: { rx: 0, ry: 0.05, rz: 0, tx: 0, ty: 0, tz: 0 },
        },
        {
          name: 'Pitch-like extra root',
          re: -0.11,
          im: 0.04,
          runCaseIndex: 0,
          stateOrder,
          eigenvector: eigenvector({ q: 1, theta: 0.75, w: 0.5 }),
          vec: { rx: 0, ry: 0.04, rz: 0, tx: 0, ty: 0, tz: 0 },
        },
        {
          name: 'Test B',
          re: -0.05,
          im: 0.02,
          runCaseIndex: 1,
          stateOrder,
          eigenvector: eigenvector({ v: 1, r: 0.8, psi: 0.6 }),
          vec: { rx: 0, ry: 0, rz: 0.05, tx: 0, ty: 0.1, tz: 0 },
        },
      ]);
      return window.__trefftzTestHook.getEigenPoints();
    });
    expect(Array.isArray(points)).toBeTruthy();
    expect(points.length).toBeGreaterThan(0);
    const activeCaseColor = await page.locator('#runCaseList .run-case-item').first().locator('.run-case-color').inputValue();
    expect(points[0].color).toBe(activeCaseColor.toLowerCase());
    expect(points[0].classLabel).toBe('Short period');
    const renderState = await page.evaluate(() => window.__trefftzTestHook.getEigenPlotRenderState());
    expect(renderState.labels.map((label) => label.classLabel)).toContain('Short period');
    expect(renderState.labels.map((label) => label.classLabel)).toContain('Dutch roll');
    expect(renderState.labels.filter((label) => label.classLabel === 'Short period' && label.visible)).toHaveLength(2);
    expect(renderState.labels.map((label) => label.classLabel)).toContain('Unclassified');
    expect(renderState.axisLabels).toEqual({ real: 'ℜ', imag: 'ℑ' });
    const axisLayout = await page.evaluate(() => {
      const state = window.__trefftzTestHook.getEigenPlotRenderState();
      const canvas = document.querySelector('#eigenPlot');
      return {
        canvasWidth: canvas?.clientWidth || 0,
        plotRight: (state?.viewport?.x0 || 0) + (state?.viewport?.pw || 0),
        realLabelX: state?.axisLabelPositions?.real?.x || 0,
      };
    });
    expect(axisLayout.plotRight).toBeLessThan(axisLayout.canvasWidth - 45);
    expect(axisLayout.realLabelX).toBeLessThan(axisLayout.canvasWidth - 60);
    expect(renderState.reTicks.length).toBeGreaterThan(2);
    expect(renderState.imTicks.length).toBeGreaterThan(2);
    expect(renderState.dampingLabel).toBe('ζ=0.707');
    expect(renderState.dampingSegments).toBeGreaterThan(0);
    await expect(page.locator('#eigenScaleLock')).toHaveAttribute('aria-pressed', 'false');
    await page.click('#eigenScaleLock');
    await expect(page.locator('#eigenScaleLock')).toHaveAttribute('aria-pressed', 'true');
    await expect(page.locator('#eigenScaleLock')).not.toHaveAttribute('title', /.+/);
    await expect(page.locator('#eigenScaleLock')).toHaveAttribute('data-tooltip', 'Eigenmode axes locked to equal scale');
    await page.waitForFunction(() => window.__trefftzTestHook.getEigenViewport()?.scaleLocked);
    const lockedViewport = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(Math.abs(lockedViewport.reUnitsPerPixel - lockedViewport.imUnitsPerPixel)).toBeLessThan(1e-9);
    await page.click('#eigenZoomIn');
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, lockedViewport.zoom);
    const lockedZoomViewport = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(Math.abs(lockedZoomViewport.reUnitsPerPixel - lockedZoomViewport.imUnitsPerPixel)).toBeLessThan(1e-9);
    const scaleLockedDrag = await page.evaluate(() => {
      const canvas = document.querySelector('#eigenPlot');
      const rect = canvas?.getBoundingClientRect();
      const vp = window.__trefftzTestHook.getEigenViewport();
      return {
        startX: rect.left + vp.x0 + vp.pw * 0.20,
        startY: rect.top + vp.y0 + vp.ph * 0.25,
        endX: rect.left + vp.x0 + vp.pw * 0.65,
        endY: rect.top + vp.y0 + vp.ph * 0.35,
      };
    });
    await page.mouse.move(scaleLockedDrag.startX, scaleLockedDrag.startY);
    await page.mouse.down();
    await page.mouse.move(scaleLockedDrag.endX, scaleLockedDrag.endY, { steps: 8 });
    await page.waitForFunction(() => window.__trefftzTestHook.getEigenPlotRenderState()?.boxZoomActive);
    const lockedBoxPreview = await page.evaluate(() => {
      const state = window.__trefftzTestHook.getEigenPlotRenderState();
      const rect = state.boxZoomRect;
      return {
        scaleLocked: Boolean(rect?.scaleLocked),
        width: Number(rect?.width),
        height: Number(rect?.height),
        rawAspect: Number(rect?.rawWidth) / Number(rect?.rawHeight),
        previewAspect: Number(rect?.width) / Number(rect?.height),
        targetAspect: Number(state?.viewport?.pw) / Number(state?.viewport?.ph),
      };
    });
    expect(lockedBoxPreview.scaleLocked).toBe(true);
    expect(lockedBoxPreview.width).toBeGreaterThan(8);
    expect(lockedBoxPreview.height).toBeGreaterThan(8);
    expect(Math.abs(lockedBoxPreview.rawAspect - lockedBoxPreview.targetAspect)).toBeGreaterThan(0.5);
    expect(Math.abs(lockedBoxPreview.previewAspect - lockedBoxPreview.targetAspect)).toBeLessThan(1e-6);
    await page.mouse.up();
    await page.waitForFunction(() => !window.__trefftzTestHook.getEigenPlotRenderState()?.boxZoomActive);
    await page.click('#eigenScaleLock');
    await expect(page.locator('#eigenScaleLock')).toHaveAttribute('aria-pressed', 'false');
    await page.click('#eigenHome');
    const viewportBeforeZoomIn = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportBeforeZoomIn).toBeTruthy();

    const p = points[0];
    await page.click('#eigenZoomIn');
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, viewportBeforeZoomIn.zoom);
    const viewportAfterZoomIn = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportAfterZoomIn.maxRe).toBeLessThan(viewportBeforeZoomIn.maxRe);

    await page.click('#eigenZoomOut');
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom < z, viewportAfterZoomIn.zoom);
    const viewportAfterZoomOut = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());

    await page.click('#eigenPan');
    await expect.poll(async () => page.evaluate(() => window.__trefftzTestHook.getEigenPanMode())).toBe(true);
    await expect(page.locator('#eigenPan')).not.toHaveAttribute('title', /.+/);
    await expect(page.locator('#eigenPan')).toHaveAttribute('data-tooltip', 'Pan eigenmode plot on');
    const viewportBeforePan = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    const canvasBox = await page.locator('#eigenPlot').boundingBox();
    expect(canvasBox).toBeTruthy();
    await page.mouse.move(canvasBox.x + (canvasBox.width * 0.5), canvasBox.y + (canvasBox.height * 0.5));
    await page.mouse.down();
    await page.mouse.move(canvasBox.x + (canvasBox.width * 0.65), canvasBox.y + (canvasBox.height * 0.58), { steps: 8 });
    await page.mouse.up();
    await page.waitForFunction(
      ({ re, im }) => {
        const vp = window.__trefftzTestHook.getEigenViewport();
        return Math.abs((vp?.centerRe || 0) - re) > 1e-4 || Math.abs((vp?.centerIm || 0) - im) > 1e-4;
      },
      { re: viewportBeforePan.centerRe, im: viewportBeforePan.centerIm },
    );
    await page.click('#eigenHome');
    await page.waitForFunction(() => {
      const vp = window.__trefftzTestHook.getEigenViewport();
      return vp && Math.abs(vp.zoom - 1) < 1e-6 && Math.abs(vp.centerRe) < 1e-6 && Math.abs(vp.centerIm) < 1e-6;
    });
    await page.click('#eigenPan');
    await expect.poll(async () => page.evaluate(() => window.__trefftzTestHook.getEigenPanMode())).toBe(false);

    await page.evaluate(() => {
      const canvas = document.querySelector('#eigenPlot');
      if (!canvas) return;
      canvas.dispatchEvent(new WheelEvent('wheel', { bubbles: true, cancelable: true, deltaY: -360 }));
    });
    await page.waitForTimeout(80);
    const viewportAfterWheel = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportAfterWheel.zoom).toBeCloseTo(viewportAfterZoomOut.zoom, 6);
    expect(viewportAfterWheel.maxRe).toBeCloseTo(viewportAfterZoomOut.maxRe, 6);

    const viewportBeforeBox = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    const boxCanvas = await page.locator('#eigenPlot').boundingBox();
    expect(boxCanvas).toBeTruthy();
    await page.mouse.move(boxCanvas.x + (boxCanvas.width * 0.30), boxCanvas.y + (boxCanvas.height * 0.30));
    await page.mouse.down();
    await page.mouse.move(boxCanvas.x + (boxCanvas.width * 0.58), boxCanvas.y + (boxCanvas.height * 0.62), { steps: 8 });
    await page.mouse.up();
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, viewportBeforeBox.zoom);
    const viewportAfterBox = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());
    expect(viewportAfterBox.maxRe).toBeLessThan(viewportBeforeBox.maxRe);

    await page.dblclick('#eigenPlot', { position: { x: canvasBox.width * 0.5, y: canvasBox.height * 0.5 } });
    await page.waitForFunction(() => {
      const vp = window.__trefftzTestHook.getEigenViewport();
      return vp && Math.abs(vp.zoom - 1) < 1e-6 && Math.abs(vp.centerRe) < 1e-6 && Math.abs(vp.centerIm) < 1e-6;
    });
    const viewportBeforeTouch = await page.evaluate(() => window.__trefftzTestHook.getEigenViewport());

    const [download] = await Promise.all([
      page.waitForEvent('download'),
      page.click('#eigenDownload'),
    ]);
    expect(download.suggestedFilename()).toContain('_eigenmodes.csv');
    const downloadPath = await download.path();
    const eigenCsv = await fs.readFile(downloadPath, 'utf8');
    expect(eigenCsv).toContain('run case,mode class,real eigenvalue,imag eigenvalue');
    expect(eigenCsv).toContain('1,Short period,-0.200000,0.120000');
    expect(eigenCsv).toContain('2,Dutch roll,-0.050000,0.020000');

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
    await page.waitForFunction((z) => window.__trefftzTestHook.getEigenViewport()?.zoom > z, viewportBeforeTouch.zoom);
    const pointsAfterZoom = await page.evaluate(() => window.__trefftzTestHook.getEigenPoints());
    const p2 = pointsAfterZoom.find((point) => point.selectable !== false) || pointsAfterZoom[0] || points[0];
    expect(p2).toBeTruthy();

    const selectCanvas = await page.locator('#eigenPlot').boundingBox();
    expect(selectCanvas).toBeTruthy();
    await page.waitForTimeout(300);
    await page.mouse.click(selectCanvas.x + p2.x, selectCanvas.y + p2.y);

    await page.waitForFunction((idx) => window.__trefftzTestHook.getSelectedEigenMode() === idx, p2.idx);

    await page.mouse.click(selectCanvas.x + p2.x, selectCanvas.y + p2.y);
    await page.waitForFunction(() => window.__trefftzTestHook.getSelectedEigenMode() === -1);

    await expect(page.locator('#eigenDivergence')).toHaveAttribute('aria-pressed', 'false');
    const decayAnimation = await page.evaluate(() => {
      const stateOrder = ['u', 'w', 'q', 'theta', 'v', 'p', 'r', 'phi', 'x', 'y', 'z', 'psi'];
      window.__trefftzTestHook.setEigenModes([{
        name: 'Unstable pitch',
        re: 0.8,
        im: 0.6,
        runCaseIndex: 0,
        stateOrder,
        eigenvector: {
          re: stateOrder.map((key) => ({ q: 1, theta: 0.8, w: 0.5 }[key] || 0)),
          im: stateOrder.map(() => 0),
        },
        vec: { rx: 0, ry: 0.05, rz: 0, tx: 0.1, ty: 0, tz: 0 },
      }]);
      window.__trefftzTestHook.startEigenModeAnimationForTest(0);
      return window.__trefftzTestHook.setModeAnimationElapsedForTest(6);
    });
    expect(decayAnimation.showDivergence).toBe(false);
    expect(decayAnimation.sigma).toBeLessThan(0);
    expect(decayAnimation.amplitude).toBeLessThan(1);

    await page.click('#eigenDivergence');
    await expect(page.locator('#eigenDivergence')).toHaveAttribute('aria-pressed', 'true');
    const initialGrowthAnimation = await page.evaluate(() => window.__trefftzTestHook.getModeAnimationState());
    expect(initialGrowthAnimation.showDivergence).toBe(true);
    expect(initialGrowthAnimation.initialAmplitude).toBeGreaterThan(0);
    expect(initialGrowthAnimation.initialAmplitude).toBeLessThan(0.2);
    expect(initialGrowthAnimation.amplitude).toBeGreaterThanOrEqual(initialGrowthAnimation.initialAmplitude);
    expect(initialGrowthAnimation.amplitude).toBeLessThan(initialGrowthAnimation.initialAmplitude * 1.02);
    const growthAnimation = await page.evaluate(() => window.__trefftzTestHook.setModeAnimationElapsedForTest(6));
    expect(growthAnimation.showDivergence).toBe(true);
    expect(growthAnimation.sigma).toBeGreaterThan(0);
    expect(growthAnimation.amplitude).toBeGreaterThan(initialGrowthAnimation.amplitude);
    expect(growthAnimation.amplitude).toBeLessThanOrEqual(growthAnimation.amplitudeCap);
    expect(growthAnimation.resetPositionError).toBeGreaterThan(growthAnimation.referenceSpan * 2.9);
    const loopedAnimation = await page.evaluate(() => window.__trefftzTestHook.setModeAnimationElapsedForTest(1000.4167));
    expect(loopedAnimation.loopCount).toBeGreaterThan(growthAnimation.loopCount);
    expect(loopedAnimation.elapsed).toBeCloseTo(0, 6);
    expect(loopedAnimation.positionError).toBeCloseTo(0, 6);
    expect(loopedAnimation.amplitude).toBeCloseTo(loopedAnimation.initialAmplitude, 6);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

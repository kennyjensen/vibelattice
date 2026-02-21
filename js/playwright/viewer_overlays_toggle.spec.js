import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('viewer overlay buttons toggle panel spacing, vortices, and flow field independently', async ({ page }) => {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 20000 });
    await expect(page.locator('#viewerCoord')).toHaveCount(0);
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState));
    await page.evaluate(() => {
      const nvor = 1;
      const dim = 4;
      const rv1 = new Array(dim * (nvor + 1)).fill(0);
      const rv2 = new Array(dim * (nvor + 1)).fill(0);
      const setVortex = (arr, i, x, y, z) => {
        arr[1 + (dim * i)] = x;
        arr[2 + (dim * i)] = y;
        arr[3 + (dim * i)] = z;
      };
      // Single horseshoe with spanwise bound segment gives a strong downwash pocket.
      setVortex(rv1, 1, 0.0, 0.0, 0.0);
      setVortex(rv2, 1, 0.0, 1.0, 0.0);
      window.__trefftzTestHook.setFlowSolverData?.({
        BETM: 1.0,
        IYSYM: 0,
        IZSYM: 0,
        YSYM: 0,
        ZSYM: 0,
        NVOR: nvor,
        GAM: [0, 1.0],
        IJFRST: [0, 1],
        NVSTRP: [0, 1],
        RV1: rv1,
        RV2: rv2,
      });
    });

    await expect(page.locator('#viewerPanelSpacing')).toHaveCount(1);
    await expect(page.locator('#viewerVortices')).toHaveCount(1);
    await expect(page.locator('#viewerFlow')).toHaveCount(1);

    await page.click('#viewerPanelSpacing');
    await expect(page.locator('#viewerPanelSpacing')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getViewerOverlayState();
      return s.showPanelSpacing && s.hasPanelSpacing && s.panelSpacingVisible;
    });

    await page.click('#viewerVortices');
    await expect(page.locator('#viewerVortices')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getViewerOverlayState();
      return s.showVortices && s.hasVortices && s.vorticesVisible && s.showPanelSpacing;
    });

    await page.click('#viewerFlow');
    await expect(page.locator('#viewerFlow')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getViewerOverlayState();
      return s.showFlowField && s.hasFlow && s.flowVisible && s.showPanelSpacing && s.showVortices;
    });
    const flowStats = await page.evaluate(() => window.__trefftzTestHook.getFlowFieldStats?.());
    const bref = Number(await page.locator('#fileBref').inputValue());
    expect(flowStats).toBeTruthy();
    expect(flowStats.count).toBeGreaterThan(50);
    expect(flowStats.maxRadius).toBeLessThanOrEqual(bref + 0.05);
    expect(Array.isArray(flowStats.lineOpacities)).toBeTruthy();
    expect(flowStats.lineOpacities.length).toBe(0);
    expect(flowStats.xUniqueCount).toBeGreaterThan(50);
    expect(flowStats.source).toBe('solver-vortices');
    expect(flowStats.vortexCount).toBeGreaterThan(0);
    expect(flowStats.mode).toBe('induced');
    expect(flowStats.deltaOnly).toBeTruthy();
    expect(flowStats.includesRigidBodyRotation).toBeFalsy();
    expect(flowStats.attachedToAircraft).toBeFalsy();
    expect(flowStats.hasAlphaAttr).toBeTruthy();
    expect(flowStats.hasArrowheads).toBeFalsy();
    expect(flowStats.hasTrails).toBeTruthy();
    expect(flowStats.cycleSeconds).toBeNull();
    expect(flowStats.isTracer).toBeTruthy();
    const markerAlign = await page.evaluate(() => {
      const probe = window.__trefftzTestHook.sampleInducedAtWorld?.(0, 0, 0);
      const ref = window.__trefftzTestHook.getReferenceMarkerPosition?.();
      const lines = String(document.getElementById('fileText')?.value || '').split(/\r?\n/);
      const refTokens = String(lines[4] || '')
        .trim()
        .split(/\s+/)
        .map((t) => Number(t))
        .filter((n) => Number.isFinite(n));
      const xref = Number(refTokens[0] || 0);
      const yref = Number(refTokens[1] || 0);
      const zref = Number(refTokens[2] || 0);
      const off = probe?.offset || { x: 0, y: 0, z: 0 };
      return {
        ref,
        expected: {
          x: xref + (Number(off.x) || 0),
          y: yref + (Number(off.y) || 0),
          z: zref + (Number(off.z) || 0),
        },
      };
    });
    expect(markerAlign).toBeTruthy();
    expect(markerAlign.ref).toBeTruthy();
    expect(Math.abs(markerAlign.ref.x - markerAlign.expected.x)).toBeLessThan(1e-3);
    expect(Math.abs(markerAlign.ref.y - markerAlign.expected.y)).toBeLessThan(1e-3);
    expect(Math.abs(markerAlign.ref.z - markerAlign.expected.z)).toBeLessThan(1e-3);
    const sampleAndArrow = await page.evaluate(() => {
      const align = window.__trefftzTestHook.getOverlayAlignmentStats?.();
      const vc = align?.vortexCenter;
      if (!vc) return null;
      const tx = vc.x + 0.20;
      const ty = vc.y;
      const tz = vc.z + 0.10;
      // Probe near the bound-vortex region where downwash should be negative for this setup.
      return {
        sample: window.__trefftzTestHook.sampleInducedAtWorld?.(tx, ty, tz),
        arrow: window.__trefftzTestHook.getNearestFlowArrowTo?.(tx, ty, tz),
      };
    });
    expect(sampleAndArrow).toBeTruthy();
    expect(sampleAndArrow.sample).toBeTruthy();
    expect(Number(sampleAndArrow.sample.count)).toBeGreaterThan(0);
    expect(Number(sampleAndArrow.sample.w)).toBeLessThan(0);
    expect(sampleAndArrow.arrow).toBeTruthy();
    await page.click('#viewerFlow');
    await expect(page.locator('#viewerFlow')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const stats = window.__trefftzTestHook.getFlowFieldStats?.();
      return stats?.mode === 'induced+rotation'
        && stats?.deltaOnly === true
        && stats?.includesRigidBodyRotation === true;
    });
    await page.click('#viewerFlow');
    await expect(page.locator('#viewerFlow')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const stats = window.__trefftzTestHook.getFlowFieldStats?.();
      return stats?.mode === 'full'
        && stats?.deltaOnly === false
        && stats?.includesRigidBodyRotation === true;
    });
    await page.click('#viewerFlow');
    await expect(page.locator('#viewerFlow')).not.toHaveClass(/active/);
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getViewerOverlayState();
      return !s.showFlowField
        && s.hasFlow
        && !s.flowVisible
        && s.flowMode === 'induced'
        && s.showPanelSpacing
        && s.showVortices;
    });
    await page.click('#viewerFlow');
    await expect(page.locator('#viewerFlow')).toHaveClass(/active/);
    await page.waitForFunction(() => {
      const stats = window.__trefftzTestHook.getFlowFieldStats?.();
      return stats?.mode === 'induced'
        && stats?.deltaOnly === true
        && stats?.includesRigidBodyRotation === false;
    });
    const probe0 = await page.evaluate(() => window.__trefftzTestHook.getFlowAnimationProbe?.());
    expect(probe0).toBeTruthy();
    await page.waitForFunction((prev) => {
      const curr = window.__trefftzTestHook.getFlowAnimationProbe?.();
      if (!prev || !curr) return false;
      const ds = Math.hypot(curr.sx - prev.sx, curr.sy - prev.sy, curr.sz - prev.sz);
      const de = Math.hypot(curr.ex - prev.ex, curr.ey - prev.ey, curr.ez - prev.ez);
      return ds > 1e-4 || de > 1e-4;
    }, probe0);
    const trailStart = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    const expectedTrail = await page.evaluate(() => {
      const bref = Math.max(1e-6, Math.abs(Number(document.getElementById('fileBref')?.value) || 1));
      const trailStepSeconds = 0.5;
      const trailDurationSeconds = Math.max(trailStepSeconds, 5 * (bref / 15));
      const trailCount = Math.floor(trailDurationSeconds / trailStepSeconds) + 1;
      return { trailStepSeconds, trailDurationSeconds, trailCount };
    });
    expect(trailStart).toBeTruthy();
    expect(Number(trailStart.trailCount)).toBe(expectedTrail.trailCount);
    expect(Number(trailStart.trailStepSeconds)).toBeCloseTo(expectedTrail.trailStepSeconds, 6);
    expect(Number(trailStart.trailDurationSeconds)).toBeCloseTo(expectedTrail.trailDurationSeconds, 6);
    expect(trailStart.cycleSeconds).toBeNull();
    expect(trailStart.isTracer).toBeTruthy();
    const trailGeomStart = await page.evaluate(() => {
      const main = window.__trefftzTestHook.getFlowAnimationProbe?.();
      const trails = window.__trefftzTestHook.getFlowTrailProbe?.()?.trails || [];
      const mainLen = main
        ? Math.hypot(
          Number(main.ex) - Number(main.sx),
          Number(main.ey) - Number(main.sy),
          Number(main.ez) - Number(main.sz),
        )
        : 0;
      const trailLens = trails.map((t) => Math.hypot(
        Number(t.ex) - Number(t.sx),
        Number(t.ey) - Number(t.sy),
        Number(t.ez) - Number(t.sz),
      ));
      return { mainLen, trailLens };
    });
    expect(trailGeomStart).toBeTruthy();
    expect(trailGeomStart.mainLen).toBeGreaterThan(1e-4);
    expect(trailGeomStart.trailLens.every((l) => Number.isFinite(l) && l >= 0)).toBeTruthy();
    const visibleTrailStart = (trailStart.trails || []).filter((t) => Number(t.alpha0) > 0.1 && Number(t.alpha1) > 0.1).length;
    expect(visibleTrailStart).toBe(1);
    const t0 = await page.evaluate(() => performance.now());
    await page.waitForTimeout(5200);
    const t1 = await page.evaluate(() => performance.now());
    const dt1 = (t1 - t0) / 1000;
    expect(dt1).toBeGreaterThan(5.0);
    expect(dt1).toBeLessThan(7.0);
    const trailAfter1s = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    expect(Number(trailAfter1s.trailStoredPoints)).toBeGreaterThanOrEqual(9);
    const visibleTrail1 = (trailAfter1s.trails || []).filter((t) => Number(t.alpha0) > 0.1 && Number(t.alpha1) > 0.1).length;
    expect(visibleTrail1).toBeGreaterThanOrEqual(9);
    expect(visibleTrail1).toBeLessThanOrEqual(13);
    await page.waitForTimeout(5200);
    const trailAfter3s = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    expect(Number(trailAfter3s.trailStoredPoints)).toBeGreaterThanOrEqual(Math.min(expectedTrail.trailCount, 11));
    const visibleTrail3 = (trailAfter3s.trails || []).filter((t) => Number(t.alpha0) > 0.1 && Number(t.alpha1) > 0.1).length;
    expect(visibleTrail3).toBeGreaterThanOrEqual(9);
    expect(visibleTrail3).toBeLessThanOrEqual(expectedTrail.trailCount);

    await page.click('#viewerVortices');
    await expect(page.locator('#viewerVortices')).not.toHaveClass(/active/);
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook.getViewerOverlayState();
      return !s.showVortices && s.showPanelSpacing && s.showFlowField;
    });
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

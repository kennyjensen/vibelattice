import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('flow tracer advects while active and resets to start when inactive', async ({ page }) => {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
    await page.click('#viewerFlow');
    await page.waitForFunction(() => {
      const p = window.__trefftzTestHook?.getFlowTracerProbe?.();
      return Boolean(p && Number.isFinite(p.x) && Number.isFinite(p.startX));
    });
    const initial = await page.evaluate(() => ({
      t: performance.now(),
      probe: window.__trefftzTestHook.getFlowTracerProbe?.(),
      stats: window.__trefftzTestHook.getFlowFieldStats?.(),
    }));
    expect(initial.stats).toBeTruthy();
    expect(initial.stats.isTracer).toBeTruthy();
    expect(initial.stats.count).toBeGreaterThan(50);
    expect(initial.stats.hasTrails).toBeTruthy();
    const initialTrail = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    const expectedTrail = await page.evaluate(() => {
      const bref = Math.max(1e-6, Math.abs(Number(document.getElementById('fileBref')?.value) || 1));
      const trailStepSeconds = 0.5;
      const trailDurationSeconds = Math.max(trailStepSeconds, 5 * (bref / 15));
      const trailCount = Math.floor(trailDurationSeconds / trailStepSeconds) + 1;
      return { trailStepSeconds, trailDurationSeconds, trailCount };
    });
    expect(initialTrail).toBeTruthy();
    expect(initialTrail.isTracer).toBeTruthy();
    expect(Number(initialTrail.trailStepSeconds)).toBeCloseTo(expectedTrail.trailStepSeconds, 6);
    expect(Number(initialTrail.trailDurationSeconds)).toBeCloseTo(expectedTrail.trailDurationSeconds, 6);
    expect(Number(initialTrail.trailCount)).toBe(expectedTrail.trailCount);

    await page.waitForFunction((t0) => performance.now() >= (t0 + 3200), initial.t);
    const moved = await page.evaluate(() => window.__trefftzTestHook.getFlowTracerProbe?.());
    expect(moved).toBeTruthy();
    const moveDist = Math.hypot(
      Number(moved.x) - Number(initial.probe.x),
      Number(moved.y) - Number(initial.probe.y),
      Number(moved.z) - Number(initial.probe.z),
    );
    expect(moveDist).toBeGreaterThan(1e-4);
    const trailAfter3s = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    expect(trailAfter3s).toBeTruthy();
    expect(Number(trailAfter3s.trailStoredPoints)).toBeGreaterThanOrEqual(2);
    expect(Number(trailAfter3s.cloudVisibleSegments)).toBeGreaterThan(50);
    const activeTrails = (trailAfter3s.trails || []).filter((t) => Math.hypot(
      Number(t.ex) - Number(t.sx),
      Number(t.ey) - Number(t.sy),
      Number(t.ez) - Number(t.sz),
    ) > 1e-6);
    expect(activeTrails.length).toBeGreaterThanOrEqual(2);
    const newestAlpha = Number(activeTrails[0]?.alpha0 ?? 0);
    const oldestAlpha = Number(activeTrails[activeTrails.length - 1]?.alpha0 ?? 0);
    expect(newestAlpha).toBeLessThanOrEqual(0.5 + 1e-6);
    expect(newestAlpha).toBeGreaterThan(0.45);
    expect(oldestAlpha).toBeLessThan(newestAlpha);

    await page.evaluate(() => window.__trefftzTestHook.setFlowFieldActive?.(false));
    await page.waitForTimeout(120);
    const reset = await page.evaluate(() => window.__trefftzTestHook.getFlowTracerProbe?.());
    expect(reset).toBeTruthy();
    const resetDist = Math.hypot(
      Number(reset.x) - Number(reset.startX),
      Number(reset.y) - Number(reset.startY),
      Number(reset.z) - Number(reset.startZ),
    );
    expect(resetDist).toBeLessThan(1e-5);
    expect(Number(reset.trailStoredPoints)).toBe(1);

    await page.evaluate(() => window.__trefftzTestHook.setFlowFieldActive?.(true));
    const reactivateStart = await page.evaluate(() => ({
      t: performance.now(),
      p: window.__trefftzTestHook.getFlowTracerProbe?.(),
    }));
    await page.waitForFunction((t0) => performance.now() >= (t0 + 800), reactivateStart.t);
    const reactivateEnd = await page.evaluate(() => window.__trefftzTestHook.getFlowTracerProbe?.());
    const reactivateDist = Math.hypot(
      Number(reactivateEnd.x) - Number(reactivateStart.p.x),
      Number(reactivateEnd.y) - Number(reactivateStart.p.y),
      Number(reactivateEnd.z) - Number(reactivateStart.p.z),
    );
    expect(reactivateDist).toBeGreaterThan(1e-4);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

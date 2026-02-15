import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('flow tracer evolves at real browser time across 0s/0.75s/1.5s checkpoints', async ({ page }) => {
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
      const p = window.__trefftzTestHook?.getFlowAnimationProbe?.();
      return Boolean(p && Number.isFinite(p.sx));
    });

    const t0AndProbe = await page.evaluate(() => ({
      t0: performance.now(),
      p: window.__trefftzTestHook.getFlowAnimationProbe?.(),
    }));
    expect(t0AndProbe.p).toBeTruthy();

    const trailMeta = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    const expectedTrail = await page.evaluate(() => {
      const bref = Math.max(1e-6, Math.abs(Number(document.getElementById('fileBref')?.value) || 1));
      const trailStepSeconds = 0.5;
      const trailDurationSeconds = Math.max(trailStepSeconds, 5 * (bref / 15));
      return { trailStepSeconds, trailDurationSeconds };
    });
    expect(trailMeta).toBeTruthy();
    expect(trailMeta.isTracer).toBeTruthy();
    expect(Number(trailMeta.trailStepSeconds)).toBeCloseTo(expectedTrail.trailStepSeconds, 6);
    expect(Number(trailMeta.trailDurationSeconds)).toBeCloseTo(expectedTrail.trailDurationSeconds, 6);

    await page.waitForFunction((t0) => performance.now() >= (t0 + 750), t0AndProbe.t0);
    const p075 = await page.evaluate(() => window.__trefftzTestHook.getFlowAnimationProbe?.());
    expect(p075).toBeTruthy();

    await page.waitForFunction((t0) => performance.now() >= (t0 + 1500), t0AndProbe.t0);
    const p150 = await page.evaluate(() => window.__trefftzTestHook.getFlowAnimationProbe?.());
    expect(p150).toBeTruthy();

    const dist = (a, b) => Math.hypot(
      (Number(a.sx) - Number(b.sx)),
      (Number(a.sy) - Number(b.sy)),
      (Number(a.sz) - Number(b.sz)),
      (Number(a.ex) - Number(b.ex)),
      (Number(a.ey) - Number(b.ey)),
      (Number(a.ez) - Number(b.ez)),
    );

    const d0_075 = dist(t0AndProbe.p, p075);
    const d075_150 = dist(p075, p150);
    const d0_150 = dist(t0AndProbe.p, p150);

    // Real-time checkpoints must produce clearly different geometry.
    expect(d0_150).toBeGreaterThan(0.003);
    expect(Math.max(d0_075, d075_150)).toBeGreaterThan(0.0015);

    const trailMetaLate = await page.evaluate(() => window.__trefftzTestHook.getFlowTrailProbe?.());
    expect(Number(trailMetaLate.trailStoredPoints)).toBeGreaterThanOrEqual(1);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

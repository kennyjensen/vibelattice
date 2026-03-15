import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

function signOf(value, tol = 1e-4) {
  if (!Number.isFinite(value) || Math.abs(value) <= tol) return 0;
  return value > 0 ? 1 : -1;
}

test('supergee duplicated aileron deflection is antisymmetric across YDUP surfaces', async ({ page }) => {
  const app = await startAppServer();
  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getSurfaceControlDeflectionSummary), null, { timeout: 30000 });
    await page.selectOption('#loadExampleSelect', 'supergee.avl');
    await expect(page.locator('#fileMeta')).toContainText('supergee.avl', { timeout: 30000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.setViewerControlDeflectionsForTest), null, { timeout: 30000 });

    const summary = async () => page.evaluate(() => {
      return (window.__trefftzTestHook?.getSurfaceControlDeflectionSummary?.() || []).filter((item) => item.controlName === 'aileron');
    });

    await page.evaluate(() => window.__trefftzTestHook.setViewerControlDeflectionsForTest(null));
    await expect.poll(async () => (await summary()).length, { timeout: 30000 }).toBeGreaterThan(0);
    const baseline = await summary();
    await page.evaluate(() => window.__trefftzTestHook.setViewerControlDeflectionsForTest({ aileron: 20 }));
    await expect.poll(async () => {
      const items = await summary();
      return items.some((item) => {
        const before = baseline.find((entry) => entry.surfaceName === item.surfaceName && entry.controlName === item.controlName);
        return before && Math.abs(Number(item.avgZ) - Number(before.avgZ)) > 0.01;
      });
    }, { timeout: 30000 }).toBe(true);
    const deflected = await summary();

    const deltas = deflected.map((item) => {
      const before = baseline.find((entry) => entry.surfaceName === item.surfaceName && entry.controlName === item.controlName);
      return {
        surfaceName: item.surfaceName,
        avgY: Number(item.avgY),
        deltaZ: before ? (Number(item.avgZ) - Number(before.avgZ)) : Number(item.avgZ),
      };
    });

    const dup = deltas.find((item) => item.surfaceName === 'Wing-ydup');
    const base = deltas.find((item) => item.surfaceName === 'Wing');
    expect(base).toBeTruthy();
    expect(dup).toBeTruthy();
    expect(Math.abs(base.deltaZ)).toBeGreaterThan(0.01);
    expect(Math.abs(dup.deltaZ)).toBeGreaterThan(0.01);
    expect(base.avgY).toBeGreaterThanOrEqual(0);
    expect(dup.avgY).toBeLessThanOrEqual(0);
    expect(signOf(base.deltaZ)).toBe(-signOf(dup.deltaZ));
  } finally {
    await app.close();
  }
});

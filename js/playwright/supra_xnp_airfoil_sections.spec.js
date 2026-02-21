import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('supra run case 1 Xnp is near Fortran reference and section lines remain visible in surface mode', async ({ page }) => {
  const app = await startAppServer();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-select')).toHaveValue('alpha');
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-value')).toHaveValue(/^\s*5(\.0+)?\s*$/);

    await expect.poll(async () => {
      const txt = String(await page.locator('#outCL').textContent() || '');
      const match = txt.match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
      return match ? Number(match[0]) : Number.NaN;
    }, { timeout: 45000 }).toBeGreaterThan(0.82);
    await expect(page.locator('#cl')).toHaveValue(/^\s*0\.7+\s*$/);
    await expect.poll(async () => {
      const txt = String(await page.locator('#outStabilityNeutral').textContent() || '');
      const match = txt.match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
      return match ? Number(match[0]) : Number.NaN;
    }, { timeout: 45000 }).toBeGreaterThan(4.70);
    const xnpText = String(await page.locator('#outStabilityNeutral').textContent() || '');
    const xnpMatch = xnpText.match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
    const xnp = xnpMatch ? Number(xnpMatch[0]) : Number.NaN;
    expect(Number.isFinite(xnp)).toBeTruthy();
    expect(xnp).toBeLessThan(4.73);
    await expect.poll(async () => {
      const txt = String(await page.locator('#outBodyDeriv').textContent() || '').trim();
      return txt;
    }, { timeout: 10000 }).not.toBe('-');

    // Render mode cycle: wireframe -> both -> surface
    await page.click('#viewerSurface');
    await page.click('#viewerSurface');
    const surfaceState = await page.evaluate(() => window.__trefftzTestHook?.getSurfaceVisualState?.() || null);
    expect(surfaceState).toBeTruthy();
    const visibleSectionLines = Number(surfaceState.airfoilOutlineVisible || 0) + Number(surfaceState.camberLineVisible || 0);
    expect(visibleSectionLines).toBeGreaterThan(0);
  } finally {
    await app.close();
  }
});

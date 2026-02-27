import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('mass points with z=0 align with viewer origin axes', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getMassPointVisualState), null, { timeout: 30000 });
    await expect(page.locator('#massPropsMeta')).toContainText('supra.mass', { timeout: 30000 });

    await page.click('#viewerMassPoints');

    await expect.poll(async () => page.evaluate(() => {
      const hook = window.__trefftzTestHook;
      const state = hook?.getMassPointVisualState?.() || {};
      return Number(state.markerVisibleCount || 0);
    }), { timeout: 15000 }).toBeGreaterThan(10);

    await expect.poll(async () => page.evaluate(() => {
      const hook = window.__trefftzTestHook;
      const state = hook?.getMassPointVisualState?.() || {};
      return Boolean(state.zeroZPointWorld) && Boolean(state.axisWorld);
    }), { timeout: 15000 }).toBeTruthy();

    const aligned = await page.evaluate(() => {
      const hook = window.__trefftzTestHook;
      const state = hook?.getMassPointVisualState?.() || {};
      return {
        markerVisibleCount: Number(state.markerVisibleCount || 0),
        zeroZAxisDz: Number(state.zeroZAxisDz),
        hasZeroZPoint: Boolean(state.zeroZPointWorld),
        hasAxis: Boolean(state.axisWorld),
      };
    });

    expect(aligned.markerVisibleCount).toBeGreaterThan(10);
    expect(aligned.hasZeroZPoint).toBeTruthy();
    expect(aligned.hasAxis).toBeTruthy();
    expect(Math.abs(aligned.zeroZAxisDz)).toBeLessThan(1e-6);
  } finally {
    await browser.close();
    await app.close();
  }
});

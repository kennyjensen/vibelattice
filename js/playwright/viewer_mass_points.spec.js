import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('viewer mass-point toggle renders markers, labels, and hover tooltip', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getMassPointVisualState), null, { timeout: 30000 });
    await expect(page.locator('#massPropsMeta')).toContainText('supra.mass', { timeout: 30000 });
    await expect(page.locator('#viewerMassPoints')).toBeVisible();

    await page.click('#viewerMassPoints');

    await expect.poll(async () => page.evaluate(() => {
      const hooks = window.__trefftzTestHook;
      return hooks?.getMassPointVisualState?.() || null;
    }), { timeout: 15000 }).toMatchObject({
      showMassPoints: true,
    });

    await expect.poll(async () => page.evaluate(() => {
      const hooks = window.__trefftzTestHook;
      const state = hooks?.getMassPointVisualState?.() || {};
      return {
        markerVisibleCount: Number(state.markerVisibleCount || 0),
        labelVisibleCount: Number(state.labelVisibleCount || 0),
      };
    }), { timeout: 15000 }).toEqual(expect.objectContaining({
      markerVisibleCount: expect.any(Number),
      labelVisibleCount: expect.any(Number),
    }));

    const afterToggle = await page.evaluate(() => {
      const hooks = window.__trefftzTestHook;
      return hooks?.getMassPointVisualState?.() || null;
    });
    expect(afterToggle?.markerVisibleCount).toBeGreaterThan(10);
    expect(afterToggle?.labelVisibleCount).toBeGreaterThan(0);
    expect(String(afterToggle?.firstName || '').length).toBeGreaterThan(0);

    const firstScreen = afterToggle?.firstScreen;
    expect(firstScreen && Number.isFinite(firstScreen.x) && Number.isFinite(firstScreen.y)).toBeTruthy();
    await page.mouse.move(firstScreen.x, firstScreen.y);

    await expect.poll(async () => page.evaluate(() => {
      const hooks = window.__trefftzTestHook;
      const state = hooks?.getMassPointVisualState?.() || {};
      return {
        hoverVisible: Boolean(state.hoverVisible),
        hoverText: String(state.hoverText || ''),
      };
    }), { timeout: 10000 }).toEqual(expect.objectContaining({
      hoverVisible: true,
      hoverText: expect.stringContaining('m='),
    }));

    await page.click('#viewerText');
    await expect.poll(async () => page.evaluate(() => {
      const hooks = window.__trefftzTestHook;
      const state = hooks?.getMassPointVisualState?.() || {};
      return Number(state.labelVisibleCount || 0);
    }), { timeout: 10000 }).toBe(0);
  } finally {
    await browser.close();
    await app.close();
  }
});

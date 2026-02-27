import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('mass properties panel shows Mass file vs Active columns and Apply copies file values to active', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'allegro.avl');
    await expect(page.locator('#massPropsMeta')).toContainText('allegro.mass', { timeout: 30000 });

    await expect(page.locator('.mass-props-head').nth(1)).toHaveText('Mass file');
    await expect(page.locator('.mass-props-head').nth(2)).toHaveText('Active');

    const baseline = await page.evaluate(() => ({
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
      fileXcg: Number(document.querySelector('#massFileXcg')?.value),
      activeMass: Number(document.querySelector('#massTotal')?.value),
      activeXcg: Number(document.querySelector('#massXcg')?.value),
    }));

    expect(Number.isFinite(baseline.fileMass)).toBeTruthy();
    expect(Number.isFinite(baseline.fileXcg)).toBeTruthy();
    expect(Math.abs(baseline.fileMass - baseline.activeMass)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(baseline.fileXcg - baseline.activeXcg)).toBeGreaterThan(1e-3);

    await page.fill('#massTotal', '9.87');
    await page.fill('#massXcg', '1.23');

    await expect.poll(async () => page.evaluate(() => ({
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
      fileXcg: Number(document.querySelector('#massFileXcg')?.value),
      activeMass: Number(document.querySelector('#massTotal')?.value),
      activeXcg: Number(document.querySelector('#massXcg')?.value),
      flightMass: Number(document.querySelector('#mass')?.value),
      flightXcg: Number(document.querySelector('#xcg')?.value),
    })), { timeout: 10000 }).toMatchObject({
      fileMass: baseline.fileMass,
      fileXcg: baseline.fileXcg,
      activeMass: 9.87,
      activeXcg: 1.23,
      flightMass: 9.87,
      flightXcg: 1.23,
    });

    await page.click('#massPropsApplyBtn');

    await expect.poll(async () => page.evaluate(() => ({
      activeMass: Number(document.querySelector('#massTotal')?.value),
      activeXcg: Number(document.querySelector('#massXcg')?.value),
      flightMass: Number(document.querySelector('#mass')?.value),
      flightXcg: Number(document.querySelector('#xcg')?.value),
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
      fileXcg: Number(document.querySelector('#massFileXcg')?.value),
    })), { timeout: 10000 }).toMatchObject({
      activeMass: baseline.fileMass,
      activeXcg: baseline.fileXcg,
      flightMass: baseline.fileMass,
      flightXcg: baseline.fileXcg,
      fileMass: baseline.fileMass,
      fileXcg: baseline.fileXcg,
    });
  } finally {
    await browser.close();
    await app.close();
  }
});

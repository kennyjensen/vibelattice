import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

test('desktop workbench columns can be resized and reset', async ({ page }) => {
  test.setTimeout(60000);
  await page.setViewportSize({ width: 1600, height: 900 });
  const app = await startAppServer();

  try {
    await page.goto(`${app.baseUrl}/index.html?debug=1`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
    await expect(page.locator('#resizeSettingsPlots')).toBeVisible();
    await expect(page.locator('#resizePlotsOutputs')).toBeVisible();

    const before = await page.evaluate(() => ({
      settings: document.querySelector('#settingsCol')?.getBoundingClientRect().width || 0,
      outputs: document.querySelector('#outputsCol')?.getBoundingClientRect().width || 0,
    }));
    expect(before.settings).toBeGreaterThan(250);
    expect(before.outputs).toBeGreaterThan(250);

    const settingsHandle = await page.locator('#resizeSettingsPlots').boundingBox();
    expect(settingsHandle).toBeTruthy();
    const settingsY = Math.min(settingsHandle.y + 120, 420);
    await page.mouse.move(settingsHandle.x + settingsHandle.width / 2, settingsY);
    await page.mouse.down();
    await page.mouse.move(settingsHandle.x + settingsHandle.width / 2 + 140, settingsY, { steps: 8 });
    await page.mouse.up();

    await expect.poll(async () => page.evaluate(() => (
      document.querySelector('#settingsCol')?.getBoundingClientRect().width || 0
    ))).toBeGreaterThan(before.settings + 90);

    const outputHandle = await page.locator('#resizePlotsOutputs').boundingBox();
    expect(outputHandle).toBeTruthy();
    const outputsY = Math.min(outputHandle.y + 120, 420);
    await page.mouse.move(outputHandle.x + outputHandle.width / 2, outputsY);
    await page.mouse.down();
    await page.mouse.move(outputHandle.x + outputHandle.width / 2 - 120, outputsY, { steps: 8 });
    await page.mouse.up();

    await expect.poll(async () => page.evaluate(() => (
      document.querySelector('#outputsCol')?.getBoundingClientRect().width || 0
    ))).toBeGreaterThan(before.outputs + 70);

    const stored = await page.evaluate(() => JSON.parse(localStorage.getItem('vibelattice.columnLayout.v1') || '{}'));
    expect(Number(stored.settings)).toBeGreaterThan(before.settings + 90);
    expect(Number(stored.outputs)).toBeGreaterThan(before.outputs + 70);

    await page.locator('#resizeSettingsPlots').dblclick();
    await expect.poll(async () => page.evaluate(() => localStorage.getItem('vibelattice.columnLayout.v1'))).toBeNull();
    const inlineVars = await page.evaluate(() => ({
      settings: document.querySelector('#appRoot')?.style.getPropertyValue('--settings-col-width') || '',
      outputs: document.querySelector('#appRoot')?.style.getPropertyValue('--outputs-col-width') || '',
    }));
    expect(inlineVars).toEqual({ settings: '', outputs: '' });
  } finally {
    await app.close();
  }
});

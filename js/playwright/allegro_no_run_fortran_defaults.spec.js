import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

function parseNum(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : Number.NaN;
}

test('allegro without run file seeds CL/velocity defaults from first EXEC like Fortran c1 behavior', async ({ page }) => {
  const app = await startAppServer();
  try {
    await loadExampleAndWait(page, app.baseUrl, 'allegro.avl');
    await expect(page.locator('#fileMeta')).toContainText('allegro.avl', { timeout: 30000 });
    await expect(page.locator('#massPropsMeta')).toContainText('allegro.mass', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toHaveText('No run cases loaded.');

    await expect.poll(async () => parseNum(await page.locator('#cl').inputValue()), { timeout: 90000 }).toBeGreaterThan(0.42);
    await expect.poll(async () => parseNum(await page.locator('#cl').inputValue()), { timeout: 90000 }).toBeLessThan(0.46);

    await expect.poll(async () => parseNum(await page.locator('#vel').inputValue()), { timeout: 90000 }).toBeGreaterThan(7.2);
    await expect.poll(async () => parseNum(await page.locator('#vel').inputValue()), { timeout: 90000 }).toBeLessThan(7.7);
  } finally {
    await app.close();
  }
});

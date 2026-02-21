import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

function firstNumber(text) {
  const m = String(text || '').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return m ? Number(m[0]) : Number.NaN;
}

test('supra run case 5 trefftz cl*c/cref axis matches fortran scale', async ({ page }) => {
  const app = await startAppServer();
  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await page.waitForFunction(() => document.querySelectorAll('#runCaseList .run-case-item').length >= 5, null, { timeout: 30000 });

    await page.locator('#runCaseList .run-case-item').nth(4).click();

    await expect.poll(async () => firstNumber(await page.locator('#outCL').textContent()), { timeout: 90000 }).toBeLessThan(0.2);

    await expect.poll(async () => {
      const info = await page.evaluate(() => window.__trefftzTestHook?.getTrefftzCurveMaxes?.() || null);
      return Number(info?.maxCncOverCref);
    }, { timeout: 90000 }).toBeGreaterThan(0.13);
    await expect.poll(async () => {
      const info = await page.evaluate(() => window.__trefftzTestHook?.getTrefftzCurveMaxes?.() || null);
      return Number(info?.maxCncOverCref);
    }, { timeout: 90000 }).toBeLessThan(0.17);
  } finally {
    await app.close();
  }
});

import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

test('loading AVL without companion run/mass resets run-case and mass parameters to session defaults', async ({ page }) => {
  const app = await startAppServer();
  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => {
      const select = document.querySelector('#loadExampleSelect');
      if (!select) return false;
      const values = [...select.querySelectorAll('option')].map((o) => o.value);
      return values.includes('supra.avl') && values.includes('hershey.avl');
    }, null, { timeout: 30000 });

    await page.selectOption('#loadExampleSelect', 'hershey.avl');
    await expect(page.locator('#fileMeta')).toContainText('hershey.avl', { timeout: 30000 });
    const baseline = {
      flightMode: await page.locator('#flightMode').inputValue(),
      bank: await page.locator('#bank').inputValue(),
      cl: await page.locator('#cl').inputValue(),
      vel: await page.locator('#vel').inputValue(),
      mass: await page.locator('#mass').inputValue(),
      rho: await page.locator('#rho').inputValue(),
      gee: await page.locator('#gee').inputValue(),
    };

    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });
    await expect(page.locator('#massPropsMeta')).toContainText('supra.mass', { timeout: 30000 });

    await page.selectOption('#loadExampleSelect', 'hershey.avl');
    await expect(page.locator('#fileMeta')).toContainText('hershey.avl', { timeout: 30000 });

    await expect(page.locator('#runCasesMeta')).toHaveText('No run cases loaded.');
    await expect(page.locator('#massPropsMeta')).toHaveText('No mass file loaded.');

    await expect(page.locator('#flightMode')).toHaveValue(baseline.flightMode);
    await expect(page.locator('#bank')).toHaveValue(baseline.bank);
    await expect(page.locator('#cl')).toHaveValue(baseline.cl);
    await expect(page.locator('#vel')).toHaveValue(baseline.vel);
    await expect(page.locator('#mass')).toHaveValue(baseline.mass);
    await expect(page.locator('#rho')).toHaveValue(baseline.rho);
    await expect(page.locator('#gee')).toHaveValue(baseline.gee);
  } finally {
    await app.close();
  }
});

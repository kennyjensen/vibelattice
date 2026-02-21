import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('rapid constraint changes do not leave stability outputs blank', async ({ page }) => {
  const app = await startAppServer();
  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');

    // Wait until EXEC is underway, then repeatedly perturb alpha.
    await expect.poll(async () => {
      const dbg = String(await page.locator('#debugLog').textContent() || '');
      return dbg.includes('Worker EXEC start');
    }, { timeout: 30000 }).toBeTruthy();

    for (let i = 0; i < 10; i += 1) {
      const v = (i % 2 === 0) ? 7 : 8;
      await page.evaluate((value) => {
        const input = document.querySelector('.constraint-row[data-var="alpha"] .constraint-value');
        if (!input) return;
        input.value = String(value);
        input.dispatchEvent(new Event('input', { bubbles: true }));
        input.dispatchEvent(new Event('change', { bubbles: true }));
      }, v);
      await page.waitForTimeout(120);
    }

    await expect.poll(async () => {
      const txt = String(await page.locator('#outAlpha').textContent() || '');
      const m = txt.match(/[-+]?\d+(?:\.\d+)?/);
      return m ? Number(m[0]) : Number.NaN;
    }, { timeout: 60000 }).toBeGreaterThan(6.9);

    await expect(page.locator('#outStabilityNeutral')).not.toContainText('Xnp = -', { timeout: 30000 });
    await expect.poll(async () => {
      const txt = String(await page.locator('#outStability').textContent() || '');
      return txt.includes('α') && !txt.includes('Xnp = -') && !txt.includes('α\n-');
    }, { timeout: 30000 }).toBeTruthy();
  } finally {
    await app.close();
  }
});

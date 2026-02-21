import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('trim requests are queued while EXEC is in-flight (no worker termination storm)', async ({ page }) => {
  const app = await startAppServer();
  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');

    await expect.poll(async () => {
      const dbg = String(await page.locator('#debugLog').textContent() || '');
      return dbg.includes('Worker EXEC start');
    }, { timeout: 30000 }).toBeTruthy();

    for (let i = 0; i < 6; i += 1) {
      await page.evaluate((v) => {
        const input = document.querySelector('.constraint-row[data-var="alpha"] .constraint-value');
        if (!input) return;
        input.value = String(v);
        input.dispatchEvent(new Event('input', { bubbles: true }));
        input.dispatchEvent(new Event('change', { bubbles: true }));
      }, 6 + (i % 2));
      await page.waitForTimeout(100);
    }

    await expect.poll(async () => {
      const dbg = String(await page.locator('#debugLog').textContent() || '');
      return dbg.includes('Trim queued: solver busy.');
    }, { timeout: 30000 }).toBeTruthy();

    await expect(page.locator('#outStabilityNeutral')).not.toContainText('Xnp = -', { timeout: 60000 });
    await expect.poll(async () => {
      const txt = String(await page.locator('#outStability').textContent() || '').trim();
      return txt;
    }, { timeout: 60000 }).not.toBe('-');
  } finally {
    await app.close();
  }
});


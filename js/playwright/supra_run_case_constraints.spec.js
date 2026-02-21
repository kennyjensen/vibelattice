import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('supra example applies first run-case constraints (alpha -> alpha = 5)', async ({ page }) => {
  const app = await startAppServer();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-select')).toHaveValue('alpha');
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-value')).toHaveValue(/^\s*5(\.0+)?\s*$/);
    await page.waitForTimeout(4000);
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-select')).toHaveValue('alpha');
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-value')).toHaveValue(/^\s*5(\.0+)?\s*$/);
  } finally {
    await app.close();
  }
});

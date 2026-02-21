import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('allegro example keeps flight Xcg/Ycg/Zcg at AVL reference defaults', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'allegro.avl');
    await expect(page.locator('#massPropsMeta')).toContainText('allegro.mass', { timeout: 30000 });

    await expect(page.locator('#xcg')).toHaveValue('3.2500');
    await expect(page.locator('#ycg')).toHaveValue('0.0000');
    await expect(page.locator('#zcg')).toHaveValue('0.5000');

    await expect(page.locator('#massXcg')).toHaveValue('3.4381');
    await expect(page.locator('#massZcg')).toHaveValue('0.4883');
  } finally {
    await browser.close();
    await app.close();
  }
});

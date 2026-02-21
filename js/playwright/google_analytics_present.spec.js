import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('root entrypoint includes Google Analytics gtag snippet', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const p = await browser.newPage();
  try {
    await p.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });

    await expect(p.locator('script[src*="googletagmanager.com/gtag/js?id=G-Q6EYYWH9QE"]')).toHaveCount(1);

    const hasConfig = await p.evaluate(() => {
      const scripts = Array.from(document.querySelectorAll('script'));
      return scripts.some((s) => String(s.textContent || '').includes("gtag('config', 'G-Q6EYYWH9QE')"));
    });
    expect(hasConfig).toBeTruthy();
  } finally {
    await browser.close();
    await app.close();
  }
});

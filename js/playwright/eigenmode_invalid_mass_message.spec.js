import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

if (process.env.PLAYWRIGHT_CHANNEL) {
  test.use({ channel: process.env.PLAYWRIGHT_CHANNEL });
}

test('ow example explains missing mass inertia instead of asking to rerun trim', async ({ page }) => {
  test.setTimeout(90000);
  const { server, baseUrl } = await startAppServer();

  try {
    await page.goto(`${baseUrl}/index.html?debug=1`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');
    await page.waitForFunction(() => {
      const select = document.querySelector('#loadExampleSelect');
      return Boolean(select && [...select.querySelectorAll('option')].some((opt) => opt.value === 'ow.avl'));
    }, { timeout: 30000 });

    await page.selectOption('#loadExampleSelect', 'ow.avl');
    await expect(page.locator('#fileMeta')).toContainText('Loaded example: ow.avl', { timeout: 30000 });
    await expect.poll(async () => page.evaluate(() => {
      const log = String(document.querySelector('#debugLog')?.textContent || '');
      return log.includes('Worker EXEC done');
    }), { timeout: 60000 }).toBe(true);

    const status = page.locator('#eigenStatus');
    await expect(status).toBeVisible();
    await expect(status).toContainText('Eigenmodes need positive principal inertias');
    await expect(status).toContainText('Ixx, Iyy, Izz');
    await expect(status).toContainText('Load a .mass file');

    const report = await page.evaluate(() => ({
      hookMessage: window.__trefftzTestHook.getEigenStatusMessage(),
      ariaLabel: document.querySelector('#eigenPlot')?.getAttribute('aria-label') || '',
      points: window.__trefftzTestHook.getEigenPoints(),
    }));
    expect(report.hookMessage).toContain('positive principal inertias');
    expect(report.ariaLabel).toContain('positive principal inertias');
    expect(report.hookMessage).not.toContain('Run trim/EXEC first');
    expect(report.points).toHaveLength(0);

    const statusColor = await status.evaluate((el) => getComputedStyle(el).color);
    expect(statusColor).toBe('rgb(251, 191, 36)');
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

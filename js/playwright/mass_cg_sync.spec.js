import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('flight conditions and mass properties keep mass/cg in sync both directions', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'allegro.avl');
    await expect(page.locator('#massPropsMeta')).toContainText('allegro.mass', { timeout: 30000 });

    await expect.poll(async () => page.evaluate(() => {
      const m = Number(document.querySelector('#mass')?.value);
      const mt = Number(document.querySelector('#massTotal')?.value);
      const x = Number(document.querySelector('#xcg')?.value);
      const mx = Number(document.querySelector('#massXcg')?.value);
      const y = Number(document.querySelector('#ycg')?.value);
      const my = Number(document.querySelector('#massYcg')?.value);
      const z = Number(document.querySelector('#zcg')?.value);
      const mz = Number(document.querySelector('#massZcg')?.value);
      const eq = (a, b, tol = 1e-9) => Number.isFinite(a) && Number.isFinite(b) && Math.abs(a - b) <= tol;
      return eq(m, mt) && eq(x, mx) && eq(y, my) && eq(z, mz);
    }), { timeout: 30000 }).toBe(true);

    await page.fill('#mass', '3.21');
    await page.fill('#xcg', '4.56');
    await page.fill('#ycg', '0.12');
    await page.fill('#zcg', '0.34');

    await expect.poll(async () => page.evaluate(() => {
      const mt = Number(document.querySelector('#massTotal')?.value);
      const mx = Number(document.querySelector('#massXcg')?.value);
      const my = Number(document.querySelector('#massYcg')?.value);
      const mz = Number(document.querySelector('#massZcg')?.value);
      const eq = (a, b, tol = 1e-9) => Number.isFinite(a) && Number.isFinite(b) && Math.abs(a - b) <= tol;
      return eq(mt, 3.21, 1e-6) && eq(mx, 4.56, 1e-6) && eq(my, 0.12, 1e-6) && eq(mz, 0.34, 1e-6);
    }), { timeout: 10000 }).toBe(true);

    await page.fill('#massTotal', '1.11');
    await page.fill('#massXcg', '2.22');
    await page.fill('#massYcg', '3.33');
    await page.fill('#massZcg', '4.44');

    await expect.poll(async () => page.evaluate(() => {
      const m = Number(document.querySelector('#mass')?.value);
      const x = Number(document.querySelector('#xcg')?.value);
      const y = Number(document.querySelector('#ycg')?.value);
      const z = Number(document.querySelector('#zcg')?.value);
      const eq = (a, b, tol = 1e-9) => Number.isFinite(a) && Number.isFinite(b) && Math.abs(a - b) <= tol;
      return eq(m, 1.11, 1e-6) && eq(x, 2.22, 1e-6) && eq(y, 3.33, 1e-6) && eq(z, 4.44, 1e-6);
    }), { timeout: 10000 }).toBe(true);
  } finally {
    await browser.close();
    await app.close();
  }
});

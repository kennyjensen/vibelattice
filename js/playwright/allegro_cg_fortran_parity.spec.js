import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

test('allegro companion mass auto-applies mass/inertias but leaves active CG unchanged', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await loadExampleAndWait(page, app.baseUrl, 'allegro.avl');
    await expect(page.locator('#massPropsMeta')).toContainText('allegro.mass', { timeout: 30000 });

    const massAndInertia = await page.evaluate(() => ({
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
      activeMass: Number(document.querySelector('#massTotal')?.value),
      fileIxx: Number(document.querySelector('#massFileIxx')?.value),
      activeIxx: Number(document.querySelector('#massIxx')?.value),
    }));
    expect(Math.abs(massAndInertia.fileMass - massAndInertia.activeMass)).toBeLessThanOrEqual(1e-6);
    expect(Math.abs(massAndInertia.fileIxx - massAndInertia.activeIxx)).toBeLessThanOrEqual(1e-6);

    await expect.poll(async () => page.evaluate(() => ({
      fileXcg: Number(document.querySelector('#massFileXcg')?.value),
      activeXcg: Number(document.querySelector('#massXcg')?.value),
      flightXcg: Number(document.querySelector('#xcg')?.value),
      fileZcg: Number(document.querySelector('#massFileZcg')?.value),
      activeZcg: Number(document.querySelector('#massZcg')?.value),
      flightZcg: Number(document.querySelector('#zcg')?.value),
    })), { timeout: 30000 }).toMatchObject({
      fileXcg: 3.4381,
      activeXcg: 3.25,
      flightXcg: 3.25,
      fileZcg: 0.4883,
      activeZcg: 0.5,
      flightZcg: 0.5,
    });
  } finally {
    await browser.close();
    await app.close();
  }
});

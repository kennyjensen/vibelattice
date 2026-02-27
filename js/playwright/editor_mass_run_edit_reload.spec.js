import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('editing mass/run tabs auto-reloads like Load Mass/Load Runs', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#massPropsMeta')).toContainText('supra.mass', { timeout: 30000 });

    const before = await page.evaluate(() => ({
      activeMass: Number(document.querySelector('#massTotal')?.value),
      activeXcg: Number(document.querySelector('#massXcg')?.value),
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
      fileXcg: Number(document.querySelector('#massFileXcg')?.value),
    }));

    await page.click('#editorTabMass');
    await page.evaluate(() => {
      const ta = document.querySelector('#fileText');
      if (!ta) return;
      const src = String(ta.value || '');
      const updated = src.replace(/(\n\s*)([-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?)(\s+[-+]?\d*\.?\d+)/, '$19.9990$3');
      ta.value = updated;
      ta.dispatchEvent(new Event('input', { bubbles: true }));
    });

    await expect.poll(async () => page.evaluate((baseline) => {
      const fileMass = Number(document.querySelector('#massFileTotal')?.value);
      return Math.abs(fileMass - baseline);
    }, before.fileMass), { timeout: 15000 }).toBeGreaterThan(1e-6);
    const afterMassEdit = await page.evaluate(() => ({
      activeMass: Number(document.querySelector('#massTotal')?.value),
      fileMass: Number(document.querySelector('#massFileTotal')?.value),
    }));
    expect(afterMassEdit.activeMass).toBeCloseTo(before.activeMass, 6);
    expect(Math.abs(afterMassEdit.fileMass - before.fileMass)).toBeGreaterThan(1e-6);

    await page.click('#editorTabRun');
    await page.evaluate(() => {
      const ta = document.querySelector('#fileText');
      if (!ta) return;
      const src = String(ta.value || '');
      const updated = src.replace(/(alpha\s*=\s*)([-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?)/i, '$16.000');
      ta.value = updated;
      ta.dispatchEvent(new Event('input', { bubbles: true }));
    });

    await expect.poll(async () => page.evaluate(() => {
      const row = document.querySelector('.constraint-row[data-var="alpha"] .constraint-value');
      return Number(row?.value);
    }), { timeout: 15000 }).toBeCloseTo(6.0, 6);
  } finally {
    await browser.close();
    await app.close();
  }
});

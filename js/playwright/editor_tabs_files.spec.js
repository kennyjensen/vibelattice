import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('editor panel uses tabs for AVL/mass/run loaded files', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });

    await expect(page.locator('#editorPanel .panel-title')).toHaveText('Editor');
    await expect(page.locator('#editorTabAvl')).toBeVisible();
    await expect(page.locator('#editorTabMass')).toBeVisible();
    await expect(page.locator('#editorTabRun')).toBeVisible();

    await expect.poll(async () => page.locator('#editorTabAvl').innerText(), { timeout: 30000 }).toContain('supra.avl');
    await expect.poll(async () => page.locator('#editorTabMass').innerText(), { timeout: 30000 }).toContain('supra.mass');
    await expect.poll(async () => page.locator('#editorTabRun').innerText(), { timeout: 30000 }).toContain('supra.run');

    await page.click('#editorTabMass');
    await expect.poll(async () => page.evaluate(() => ({
      readOnly: Boolean(document.querySelector('#fileText')?.readOnly),
      text: String(document.querySelector('#fileText')?.value || ''),
    })), { timeout: 10000 }).toMatchObject({ readOnly: false });
    await expect.poll(async () => page.inputValue('#fileText')).toContain('Lunit');

    await page.click('#editorTabRun');
    await expect.poll(async () => page.evaluate(() => ({
      readOnly: Boolean(document.querySelector('#fileText')?.readOnly),
      text: String(document.querySelector('#fileText')?.value || ''),
    })), { timeout: 10000 }).toMatchObject({ readOnly: false });
    await expect.poll(async () => page.inputValue('#fileText')).toContain('Run case');

    await page.click('#editorTabAvl');
    await expect.poll(async () => page.evaluate(() => ({
      readOnly: Boolean(document.querySelector('#fileText')?.readOnly),
      text: String(document.querySelector('#fileText')?.value || ''),
    })), { timeout: 10000 }).toMatchObject({ readOnly: false });
    await expect.poll(async () => page.inputValue('#fileText')).toContain('SURFACE');
  } finally {
    await browser.close();
    await app.close();
  }
});

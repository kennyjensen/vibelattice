import fs from 'node:fs/promises';
import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('Save Mass and Save Runs preserve original file formatting', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#massPropsMeta')).toContainText('supra.mass', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await page.click('#editorTabMass');
    const originalMassText = await page.locator('#fileText').inputValue();
    const [massDownload] = await Promise.all([
      page.waitForEvent('download'),
      page.click('#massPropsSaveBtn'),
    ]);
    expect(massDownload.suggestedFilename()).toBe('supra.mass');
    const massPath = await massDownload.path();
    expect(massPath).toBeTruthy();
    const savedMassText = await fs.readFile(massPath, 'utf8');
    expect(savedMassText).toBe(originalMassText);

    await page.click('#editorTabRun');
    const originalRunText = await page.locator('#fileText').inputValue();
    const [runDownload] = await Promise.all([
      page.waitForEvent('download'),
      page.click('#runCasesSaveBtn'),
    ]);
    expect(runDownload.suggestedFilename()).toBe('supra.run');
    const runPath = await runDownload.path();
    expect(runPath).toBeTruthy();
    const savedRunText = await fs.readFile(runPath, 'utf8');
    expect(savedRunText).toBe(originalRunText);
  } finally {
    await browser.close();
    await app.close();
  }
});

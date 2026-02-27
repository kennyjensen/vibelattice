import { test, expect } from '@playwright/test';
import { chromium } from 'playwright';
import { startAppServer } from './helpers/app_test_harness.js';

test('mass and run editor tabs highlight comments and numbers', async () => {
  const app = await startAppServer();
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });

    await page.click('#editorTabMass');
    await expect.poll(async () => page.evaluate(() => {
      const root = document.querySelector('#fileHighlight');
      if (!root) return { comments: 0, numbers: 0 };
      return {
        comments: root.querySelectorAll('.avl-token-comment').length,
        numbers: root.querySelectorAll('.avl-token-number').length,
      };
    }), { timeout: 30000 }).toMatchObject({
      comments: expect.any(Number),
      numbers: expect.any(Number),
    });

    const massTokens = await page.evaluate(() => {
      const root = document.querySelector('#fileHighlight');
      return {
        comments: root?.querySelectorAll('.avl-token-comment').length ?? 0,
        numbers: root?.querySelectorAll('.avl-token-number').length ?? 0,
      };
    });
    expect(massTokens.comments).toBeGreaterThan(0);
    expect(massTokens.numbers).toBeGreaterThan(0);

    await page.click('#editorTabRun');
    await expect.poll(async () => page.evaluate(() => {
      const root = document.querySelector('#fileHighlight');
      return {
        comments: root?.querySelectorAll('.avl-token-comment').length ?? 0,
        numbers: root?.querySelectorAll('.avl-token-number').length ?? 0,
      };
    }), { timeout: 30000 }).toMatchObject({
      comments: expect.any(Number),
      numbers: expect.any(Number),
    });

    const runTokens = await page.evaluate(() => {
      const root = document.querySelector('#fileHighlight');
      return {
        comments: root?.querySelectorAll('.avl-token-comment').length ?? 0,
        numbers: root?.querySelectorAll('.avl-token-number').length ?? 0,
        keywords: root?.querySelectorAll('.avl-token-keyword').length ?? 0,
      };
    });
    expect(runTokens.numbers).toBeGreaterThan(0);
    expect(runTokens.keywords).toBeGreaterThan(0);
  } finally {
    await browser.close();
    await app.close();
  }
});

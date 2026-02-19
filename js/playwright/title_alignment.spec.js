import { test, expect } from '@playwright/test';
import path from 'node:path';

test('title subheading aligns on desktop and mobile layouts', async ({ page }) => {
  const indexPath = path.resolve('index.html');

  await page.setViewportSize({ width: 1400, height: 900 });
  await page.goto(`file://${indexPath}`, { waitUntil: 'domcontentloaded' });

  const desktop = await page.evaluate(() => {
    const title = document.querySelector('.title');
    const sub = document.querySelector('.title-sub');
    if (!title || !sub) return { missing: true };
    const rectTitle = title.getBoundingClientRect();
    const rectSub = sub.getBoundingClientRect();
    const styles = window.getComputedStyle(sub);
    const lineHeight = Number.parseFloat(styles.lineHeight || '0');
    return {
      missing: false,
      titleBottom: rectTitle.bottom,
      subBottom: rectSub.bottom,
      diff: Math.abs(rectTitle.bottom - rectSub.bottom),
      subHeight: rectSub.height,
      lineHeight,
      subAbove: rectSub.top < rectTitle.top,
      subLeft: rectSub.left,
      titleLeft: rectTitle.left,
    };
  });

  expect(desktop.missing).toBe(false);
  expect(desktop.subLeft).toBeGreaterThanOrEqual(desktop.titleLeft - 2);

  await page.setViewportSize({ width: 390, height: 780 });
  await page.reload({ waitUntil: 'domcontentloaded' });

  const mobile = await page.evaluate(() => {
    const title = document.querySelector('.title');
    const sub = document.querySelector('.title-sub');
    if (!title || !sub) return { missing: true };
    const rectTitle = title.getBoundingClientRect();
    const rectSub = sub.getBoundingClientRect();
    return {
      missing: false,
      titleBottom: rectTitle.bottom,
      subBottom: rectSub.bottom,
      diff: Math.abs(rectTitle.bottom - rectSub.bottom),
      subLeft: rectSub.left,
      titleRight: rectTitle.right,
    };
  });

  expect(mobile.missing).toBe(false);
  expect(mobile.subLeft).toBeGreaterThanOrEqual(mobile.titleRight - 2);
  expect(mobile.diff).toBeLessThanOrEqual(2);
});

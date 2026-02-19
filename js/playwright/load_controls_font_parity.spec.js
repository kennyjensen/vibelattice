import { test, expect } from '@playwright/test';
import path from 'node:path';

const entrypoints = [
  'index.html',
  'js/dist/index.html',
];

for (const relPath of entrypoints) {
  test(`Load Example matches Load AVL font in ${relPath}`, async ({ page }) => {
    const root = path.resolve('.');
    const fileUrl = `file://${path.join(root, relPath)}`;
    await page.goto(fileUrl, { waitUntil: 'domcontentloaded' });

    const styles = await page.evaluate(() => {
      const loadAvl = document.querySelector('label.btn');
      const loadExample = document.getElementById('loadExampleSelect');
      if (!loadAvl || !loadExample) return null;
      const a = getComputedStyle(loadAvl);
      const b = getComputedStyle(loadExample);
      return {
        loadAvl: {
          fontFamily: a.fontFamily,
          fontSize: a.fontSize,
          fontWeight: a.fontWeight,
          lineHeight: a.lineHeight,
        },
        loadExample: {
          fontFamily: b.fontFamily,
          fontSize: b.fontSize,
          fontWeight: b.fontWeight,
          lineHeight: b.lineHeight,
        },
      };
    });

    expect(styles).toBeTruthy();
    expect(styles.loadExample.fontFamily).toBe(styles.loadAvl.fontFamily);
    expect(styles.loadExample.fontSize).toBe(styles.loadAvl.fontSize);
    expect(styles.loadExample.fontWeight).toBe(styles.loadAvl.fontWeight);
    expect(styles.loadExample.lineHeight).toBe(styles.loadAvl.lineHeight);
  });
}

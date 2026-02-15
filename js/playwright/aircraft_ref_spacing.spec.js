import { test, expect } from '@playwright/test';
import path from 'node:path';

test('editing aircraft refs preserves line spacing and keeps decimal digits', async ({ page }) => {
  const indexPath = path.resolve('index.html');
  await page.goto(`file://${indexPath}`, { waitUntil: 'domcontentloaded' });

  const avl = [
    'My Plane',
    '0.0',
    '0 0 0.0',
    '12.0     1.0      15.0',
    '0.0      0.0     0.0',
    '',
  ].join('\n');

  await page.fill('#fileText', avl);
  await page.dispatchEvent('#fileText', 'input');
  await page.waitForTimeout(400);

  const before = await page.evaluate(() => {
    const lines = (document.getElementById('fileText')?.value || '').split(/\r?\n/);
    return { sref: lines[3], xref: lines[4] };
  });

  await page.evaluate(() => {
    const set = (id, value) => {
      const el = document.getElementById(id);
      if (!el) return;
      el.value = value;
    };
    set('fileSref', '9');
    set('fileCref', '8');
    set('fileBref', '7');
    set('fileXref', '6');
    set('fileYref', '5');
    set('fileZref', '4');
    document.getElementById('fileZref')?.dispatchEvent(new Event('change', { bubbles: true }));
  });
  await page.waitForTimeout(300);

  const after = await page.evaluate(() => {
    const lines = (document.getElementById('fileText')?.value || '').split(/\r?\n/);
    return { sref: lines[3], xref: lines[4] };
  });

  const splitTriple = (line) => {
    const m = String(line).match(/^(\s*)(\S+)(\s+)(\S+)(\s+)(\S+)(\s*)$/);
    if (!m) return null;
    return { t1: m[2], s1: m[3], t2: m[4], s2: m[5], t3: m[6] };
  };

  const bS = splitTriple(before.sref);
  const bX = splitTriple(before.xref);
  const aS = splitTriple(after.sref);
  const aX = splitTriple(after.xref);

  expect(bS).not.toBeNull();
  expect(bX).not.toBeNull();
  expect(aS).not.toBeNull();
  expect(aX).not.toBeNull();

  // Preserve inter-number spacing separators.
  expect(aS.s1).toBe(bS.s1);
  expect(aS.s2).toBe(bS.s2);
  expect(aX.s1).toBe(bX.s1);
  expect(aX.s2).toBe(bX.s2);

  // Always keep at least one digit after decimal.
  [aS.t1, aS.t2, aS.t3, aX.t1, aX.t2, aX.t3].forEach((token) => {
    expect(token).toMatch(/^-?\d+\.\d+$/);
  });
});

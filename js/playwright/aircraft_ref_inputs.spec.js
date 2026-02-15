import { test, expect } from '@playwright/test';
import path from 'node:path';

test('aircraft reference parameters are one-per-row editable inputs', async ({ page }) => {
  const indexPath = path.resolve('index.html');
  await page.goto(`file://${indexPath}`, { waitUntil: 'domcontentloaded' });

  const result = await page.evaluate(() => {
    const summary = document.getElementById('fileSummary');
    if (!summary) return { missing: 'fileSummary' };
    summary.classList.remove('hidden');

    const rowEls = Array.from(summary.querySelectorAll('.file-ref-row'));
    const labels = rowEls.map((row) => row.querySelector('.label-text')?.textContent?.trim() || '');
    const fieldTags = rowEls.map((row) => {
      const field = row.querySelector('input, select');
      return field ? field.tagName.toLowerCase() : '';
    });
    const fieldIds = rowEls.map((row) => row.querySelector('input, select')?.id || '');
    const fieldTypes = rowEls.map((row) => row.querySelector('input')?.getAttribute('type') || '');

    const hasLegacyStrongRefs = Boolean(
      summary.querySelector('#fileSref strong, #fileCref strong, #fileBref strong, #fileXref strong, #fileYref strong, #fileZref strong'),
    );

    return {
      rowCount: rowEls.length,
      labels,
      fieldTags,
      fieldIds,
      fieldTypes,
      hasLegacyStrongRefs,
    };
  });

  expect(result.missing).toBeUndefined();
  expect(result.rowCount).toBe(9);
  expect(result.labels).toEqual(['IYSym', 'IZSym', 'ZSym', 'Sref', 'Cref', 'Bref', 'Xref', 'Yref', 'Zref']);
  expect(result.fieldTags).toEqual(['select', 'select', 'input', 'input', 'input', 'input', 'input', 'input', 'input']);
  expect(result.fieldIds).toEqual(['fileIysym', 'fileIzsym', 'fileZsym', 'fileSref', 'fileCref', 'fileBref', 'fileXref', 'fileYref', 'fileZref']);
  expect(result.fieldTypes).toEqual(['', '', 'number', 'number', 'number', 'number', 'number', 'number', 'number']);
  expect(result.hasLegacyStrongRefs).toBe(false);
});

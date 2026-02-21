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

    const hasLegacyAircraftRefInputs = Boolean(
      summary.querySelector('#fileXref, #fileYref, #fileZref'),
    );
    const flightCg = ['xcg', 'ycg', 'zcg'].map((id) => {
      const input = document.getElementById(id);
      const row = input?.closest('.flight-row');
      return {
        id,
        exists: Boolean(input),
        label: row?.querySelector('.label-text')?.textContent?.trim() || '',
        type: input?.getAttribute('type') || '',
      };
    });
    const flightRowIds = Array.from(document.querySelectorAll('.flight-conditions .flight-row'))
      .map((row) => row.querySelector('input, select')?.id || '')
      .filter(Boolean);

    return {
      rowCount: rowEls.length,
      labels,
      fieldTags,
      fieldIds,
      fieldTypes,
      hasLegacyAircraftRefInputs,
      flightCg,
      flightRowTail: flightRowIds.slice(-3),
    };
  });

  expect(result.missing).toBeUndefined();
  expect(result.rowCount).toBe(6);
  expect(result.labels).toEqual(['IYSym', 'IZSym', 'ZSym', 'Sref', 'Cref', 'Bref']);
  expect(result.fieldTags).toEqual(['select', 'select', 'input', 'input', 'input', 'input']);
  expect(result.fieldIds).toEqual(['fileIysym', 'fileIzsym', 'fileZsym', 'fileSref', 'fileCref', 'fileBref']);
  expect(result.fieldTypes).toEqual(['', '', 'number', 'number', 'number', 'number']);
  expect(result.hasLegacyAircraftRefInputs).toBe(false);
  expect(result.flightCg).toEqual([
    { id: 'xcg', exists: true, label: 'Xcg', type: 'number' },
    { id: 'ycg', exists: true, label: 'Ycg', type: 'number' },
    { id: 'zcg', exists: true, label: 'Zcg', type: 'number' },
  ]);
  expect(result.flightRowTail).toEqual(['xcg', 'ycg', 'zcg']);
});

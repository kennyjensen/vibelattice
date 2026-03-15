import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

function parseFirstNumber(text) {
  const match = String(text || '').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

test('testcdcl_surface app path enables viscous drag from CDCL data', async ({ page }) => {
  const app = await startAppServer();
  try {
    await loadExampleAndWait(page, app.baseUrl, 'testcdcl_surface.avl');
    await expect(page.locator('#fileMeta')).toContainText('testcdcl_surface.avl', { timeout: 30000 });
    await page.evaluate(() => {
      const setValue = (selector, value) => {
        const el = document.querySelector(selector);
        if (!el) throw new Error(`Missing ${selector}`);
        el.value = String(value);
        el.dispatchEvent(new Event('input', { bubbles: true }));
        el.dispatchEvent(new Event('change', { bubbles: true }));
      };
      setValue('#mass', 1);
      setValue('#rho', 1);
      setValue('#gee', 1);
      setValue('#vel', 1);
      const alphaRow = document.querySelector('.constraint-row[data-var="alpha"]');
      if (!alphaRow) throw new Error('Missing alpha constraint row');
      const select = alphaRow.querySelector('.constraint-select');
      const input = alphaRow.querySelector('.constraint-value');
      if (!select || !input) throw new Error('Missing alpha constraint controls');
      select.value = 'cl';
      select.dispatchEvent(new Event('change', { bubbles: true }));
      input.value = '0.6';
      input.dispatchEvent(new Event('input', { bubbles: true }));
      input.dispatchEvent(new Event('change', { bubbles: true }));
    });

    await expect.poll(async () => {
      const cl = parseFirstNumber(await page.locator('#outCL').textContent());
      const cdvis = parseFirstNumber(await page.locator('#outCDvis').textContent());
      return { cl, cdvis };
    }, { timeout: 30000 }).toEqual({
      cl: expect.closeTo(0.6, 4),
      cdvis: expect.closeTo(0.01104, 4),
    });

    const cdvis = parseFirstNumber(await page.locator('#outCDvis').textContent());
    const cdtot = parseFirstNumber(await page.locator('#outCD').textContent());
    const cltot = parseFirstNumber(await page.locator('#outCL').textContent());

    expect(cdvis).toBeGreaterThan(0.0109);
    expect(cdvis).toBeLessThan(0.0112);
    expect(cdtot).toBeGreaterThan(0.0274);
    expect(cdtot).toBeLessThan(0.0276);
    expect(cltot).toBeGreaterThan(0.5999);
    expect(cltot).toBeLessThan(0.6001);
  } finally {
    await app.close();
  }
});

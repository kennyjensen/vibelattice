import { test, expect } from '@playwright/test';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

function parseFirstNumber(text) {
  const match = String(text || '').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

test('supra wasm app path keeps all output panels populated with fortran-parity Xnp', async ({ page }) => {
  const app = await startAppServer();
  const logs = [];
  page.on('console', (msg) => logs.push(msg.text()));

  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await expect.poll(async () => {
      const txt = await page.locator('#outCL').textContent();
      return parseFirstNumber(txt);
    }, { timeout: 90000 }).toBeGreaterThan(0.82);

    const xnp = await page.waitForFunction(() => {
      const txt = document.querySelector('#outStabilityNeutral')?.textContent || '';
      const match = txt.match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
      const value = match ? Number(match[0]) : Number.NaN;
      return Number.isFinite(value) ? value : null;
    }, null, { timeout: 90000 }).then((h) => h.jsonValue());
    expect(xnp).toBeGreaterThan(4.70);
    expect(xnp).toBeLessThan(4.73);

    const dashCounts = await page.evaluate(() => {
      const countDash = (selector) => {
        const root = document.querySelector(selector);
        if (!root) return -1;
        return [...root.querySelectorAll('.stability-num')]
          .map((el) => (el.textContent || '').trim())
          .filter((t) => t === '-').length;
      };
      return {
        stability: countDash('#outStability'),
        body: countDash('#outBodyDeriv'),
        surface: countDash('#outForcesSurface'),
        hinge: countDash('#outHinge'),
      };
    });

    expect(dashCounts.stability).toBe(0);
    expect(dashCounts.body).toBe(0);
    expect(dashCounts.surface).toBe(0);
    expect(dashCounts.hinge).toBe(0);

    const sawNonFinite = logs.some((line) => line.includes('Trefftz non-finite'));
    expect(sawNonFinite).toBeFalsy();
  } finally {
    await app.close();
  }
});

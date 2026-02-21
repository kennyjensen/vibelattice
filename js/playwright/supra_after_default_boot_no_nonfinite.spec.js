import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

function parseFirstNumber(text) {
  const match = String(text || '').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

test('loading supra after default boot run keeps outputs finite and populated', async ({ page }) => {
  const app = await startAppServer();
  const logs = [];
  page.on('console', (msg) => logs.push(msg.text()));

  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => {
      const select = document.querySelector('#loadExampleSelect');
      return !!select && [...select.querySelectorAll('option')].some((opt) => opt.value === 'supra.avl');
    }, null, { timeout: 30000 });

    // Let default plane boot/trim/exec finish first (user-reported sequence).
    await page.waitForTimeout(1500);

    await page.selectOption('#loadExampleSelect', 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await expect.poll(async () => parseFirstNumber(await page.locator('#outCL').textContent()), { timeout: 90000 }).toBeGreaterThan(0.82);
    const xnp = await page.waitForFunction(() => {
      const txt = document.querySelector('#outStabilityNeutral')?.textContent || '';
      const m = txt.match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
      const n = m ? Number(m[0]) : Number.NaN;
      return Number.isFinite(n) ? n : null;
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

    expect(logs.some((line) => line.includes('Trefftz non-finite'))).toBeFalsy();
    expect(logs.some((line) => line.includes('EXEC kernels: solve=wasm gam=wasm aero=wasm aic=wasm lu=wasm'))).toBeTruthy();
  } finally {
    await app.close();
  }
});

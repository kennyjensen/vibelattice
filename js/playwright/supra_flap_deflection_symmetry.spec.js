import { test, expect } from '@playwright/test';
import { startAppServer } from './helpers/app_test_harness.js';

function signOf(value, tol = 1e-4) {
  if (!Number.isFinite(value) || Math.abs(value) <= tol) return 0;
  return value > 0 ? 1 : -1;
}

test('supra duplicated-wing control deflections match AVL symmetry semantics', async ({ page }) => {
  const app = await startAppServer();
  try {
    await page.goto(`${app.baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getSurfaceControlDeflectionSummary), null, { timeout: 30000 });
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.setViewerControlDeflectionsForTest), null, { timeout: 30000 });

    const getControlSummary = async (name) => page.evaluate((controlName) => {
      const items = window.__trefftzTestHook?.getSurfaceControlDeflectionSummary?.() || [];
      return items.filter((item) => item.controlName === controlName);
    }, name);

    const assertControlRelation = async (controlName, commandValue, expectedRelation) => {
      await page.evaluate(() => window.__trefftzTestHook.setViewerControlDeflectionsForTest(null));
      await expect.poll(() => getControlSummary(controlName), { timeout: 30000 }).not.toHaveLength(0);
      const baseline = await getControlSummary(controlName);

      await page.evaluate(({ name, value }) => {
        window.__trefftzTestHook.setViewerControlDeflectionsForTest({ [name]: value });
      }, { name: controlName, value: commandValue });
      await expect.poll(async () => {
        const items = await getControlSummary(controlName);
        return items.some((item) => {
          const before = baseline.find((entry) => entry.surfaceName === item.surfaceName && entry.controlName === item.controlName);
          return before && Math.abs(Number(item.avgZ) - Number(before.avgZ)) > 0.01;
        });
      }, { timeout: 30000 }).toBe(true);

      const deflected = await getControlSummary(controlName);
      const deltas = deflected.map((item) => {
        const before = baseline.find((entry) => entry.surfaceName === item.surfaceName && entry.controlName === item.controlName);
        return {
          surfaceName: item.surfaceName,
          avgY: Number(item.avgY),
          deltaZ: before ? (Number(item.avgZ) - Number(before.avgZ)) : Number.NaN,
        };
      });

      const paired = deltas
        .filter((item) => item.surfaceName.endsWith('-ydup'))
        .map((dup) => ({
          dup,
          base: deltas.find((item) => item.surfaceName === dup.surfaceName.slice(0, -5)),
        }))
        .filter((entry) => entry.base);

      expect(paired.length).toBeGreaterThan(0);
      paired.forEach(({ base, dup }) => {
        expect(Math.abs(base.deltaZ)).toBeGreaterThan(0.01);
        expect(Math.abs(dup.deltaZ)).toBeGreaterThan(0.01);
        if (expectedRelation === 'same') {
          expect(signOf(base.deltaZ)).toBe(signOf(dup.deltaZ));
        } else {
          expect(signOf(base.deltaZ)).toBe(-signOf(dup.deltaZ));
        }
        expect(base.avgY).toBeGreaterThanOrEqual(0);
        expect(dup.avgY).toBeLessThanOrEqual(0);
      });
    };

    await assertControlRelation('flap', -30, 'same');
    await assertControlRelation('aileron', 10, 'opposite');
    await page.evaluate(() => window.__trefftzTestHook.setViewerControlDeflectionsForTest(null));
  } finally {
    await app.close();
  }
});

import { test, expect } from '@playwright/test';
import fs from 'node:fs/promises';
import path from 'node:path';
import { startAppServer, loadExampleAndWait } from './helpers/app_test_harness.js';

const EIGEN_TOL = 0.05;

function parseEigenReference(text) {
  const byRun = new Map();
  const lines = String(text || '').split(/\r?\n/);
  for (const raw of lines) {
    const line = raw.trim();
    if (!line || line.startsWith('#')) continue;
    const parts = line.split(/\s+/);
    if (parts.length < 3) continue;
    const runCase = Number(parts[0]);
    const re = Number(parts[1]);
    const im = Number(parts[2]);
    if (!Number.isInteger(runCase) || !Number.isFinite(re) || !Number.isFinite(im)) continue;
    if (!byRun.has(runCase)) byRun.set(runCase, []);
    byRun.get(runCase).push({ re, im });
  }
  return byRun;
}

function sortModes(modes) {
  return [...modes].sort((a, b) => {
    const dre = Number(a.re) - Number(b.re);
    if (Math.abs(dre) > 1e-9) return dre;
    return Number(a.im) - Number(b.im);
  });
}

test('supra example eigenvalues match AVL reference for all run cases', async ({ page }) => {
  const app = await startAppServer();
  const root = path.resolve('.');
  const eigText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'supra.eig'), 'utf8');
  const referenceByRun = parseEigenReference(eigText);

  try {
    await loadExampleAndWait(page, app.baseUrl, 'supra.avl');
    await expect(page.locator('#fileMeta')).toContainText('supra.avl', { timeout: 30000 });
    await expect(page.locator('#runCasesMeta')).toContainText('supra.run', { timeout: 30000 });

    await expect.poll(async () => page.locator('.run-case-item').count(), { timeout: 30000 }).toBe(5);
    const runCaseCount = await page.locator('.run-case-item').count();
    expect(runCaseCount).toBe(5);

    for (let runIdx = 0; runIdx < runCaseCount; runIdx += 1) {
      const refModes = sortModes(referenceByRun.get(runIdx + 1) || []);
      expect(refModes.length).toBeGreaterThan(0);

      await page.locator('.run-case-item').nth(runIdx).click();

      await page.waitForFunction((idx) => {
        const hook = window.__trefftzTestHook;
        const result = hook?.getLastExecResult?.();
        const parval = result?.PARVAL;
        const velInput = Number(document.querySelector('#vel')?.value || Number.NaN);
        const velOutput = Number(document.querySelector('#outV')?.textContent || Number.NaN);
        const execVelocity = Number(parval?.[12 + 30] ?? Number.NaN);
        return hook?.getSelectedRunCaseIndex?.() === idx
          && Number.isFinite(velInput)
          && Number.isFinite(velOutput)
          && Number.isFinite(execVelocity)
          && Math.abs(velOutput - velInput) < 0.1
          && Math.abs(execVelocity - velInput) < 0.1
          && Array.isArray(result?.EIGEN?.modes)
          && result.EIGEN.modes.length > 0;
      }, runIdx, { timeout: 60000 });

      const actualModes = sortModes(await page.evaluate(() => {
        const result = window.__trefftzTestHook?.getLastExecResult?.();
        return (result?.EIGEN?.modes || []).map((mode) => ({
          re: Number(mode?.re),
          im: Number(mode?.im),
        }));
      }));

      expect(actualModes.length).toBe(refModes.length);
      for (let modeIdx = 0; modeIdx < refModes.length; modeIdx += 1) {
        expect(Math.abs(actualModes[modeIdx].re - refModes[modeIdx].re)).toBeLessThan(EIGEN_TOL);
        expect(Math.abs(actualModes[modeIdx].im - refModes[modeIdx].im)).toBeLessThan(EIGEN_TOL);
      }
    }
  } finally {
    await app.close();
  }
});

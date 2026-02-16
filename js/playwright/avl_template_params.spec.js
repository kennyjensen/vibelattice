import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('AVL template literals create Aircraft sliders and drive resolved geometry values', async ({ page }) => {
  test.setTimeout(120000);
  const root = path.resolve('.');
  const server = http.createServer(async (req, res) => {
    const reqPath = (req.url || '/').split('?')[0];
    const clean = decodeURIComponent(reqPath === '/' ? '/index.html' : reqPath);
    const filePath = path.join(root, clean);
    try {
      const data = await fs.readFile(filePath);
      if (filePath.endsWith('.html')) res.setHeader('Content-Type', 'text/html; charset=utf-8');
      else if (filePath.endsWith('.js')) res.setHeader('Content-Type', 'application/javascript; charset=utf-8');
      else if (filePath.endsWith('.css')) res.setHeader('Content-Type', 'text/css; charset=utf-8');
      else if (filePath.endsWith('.json')) res.setHeader('Content-Type', 'application/json; charset=utf-8');
      res.statusCode = 200;
      res.end(data);
    } catch {
      res.statusCode = 404;
      res.end('Not found');
    }
  });

  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const address = server.address();
  const port = typeof address === 'object' && address ? address.port : 0;

  const avlTemplate = [
    'Template Plane',
    '0.0',
    '0 0 0.0',
    '12.0 1.0 15.0',
    '0.0 0.0 0.0',
    'SURFACE',
    'Wing',
    '${nChord:4} 1.0 ${nSpan??8} 1.0',
    'SECTION',
    '0.0 0.0 0.0 ${foo:1.25} ${mach}',
    'SECTION',
    '0.0 5.0 0.0 ${foo * 0.5} ${(tw + 1)}',
    '',
  ].join('\n');

  try {
    for (const entry of ['/index.html', '/js/dist/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded', timeout: 60000 });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getTemplateParams));

      await page.fill('#fileText', avlTemplate);
      await page.dispatchEvent('#fileText', 'input');
      await expect(page.locator('#templateParamsPanel')).toBeVisible();
      await expect(page.locator('#templateParamsPanel')).toHaveClass(/template-params-inline/);
      await expect(page.locator('#templateParamsPanel')).not.toHaveClass(/template-params-panel/);
      await expect(page.locator('#templateParamsPanel .template-params-title')).toHaveCount(0);
      await expect(page.locator('#templateParamsList .template-param-row')).toHaveCount(5);
      await expect(page.locator('.template-param-row[data-template-param="nChord"] .template-param-name')).toHaveText('nChord');
      await expect(page.locator('.template-param-row[data-template-param="nSpan"] .template-param-name')).toHaveText('nSpan');
      await expect(page.locator('.template-param-row[data-template-param="foo"] .template-param-name')).toHaveText('foo');
      await expect(page.locator('.template-param-row[data-template-param="mach"] .template-param-name')).toHaveText('mach');
      await expect(page.locator('.template-param-row[data-template-param="nChord"] .template-param-name')).not.toContainText('${');
      await expect(page.locator('.template-param-row[data-template-param="nSpan"] .template-param-name')).not.toContainText('??');
      await expect(page.locator('.template-param-row[data-template-param="foo"] .template-param-name')).not.toContainText(':');

      const params = await page.evaluate(() => window.__trefftzTestHook.getTemplateParams());
      const byName = Object.fromEntries(params.map((p) => [p.name, p]));
      expect(byName.nChord).toBeTruthy();
      expect(byName.nSpan).toBeTruthy();
      expect(byName.foo).toBeTruthy();
      expect(byName.mach).toBeTruthy();
      expect(byName.tw).toBeTruthy();

      expect(byName.nChord.kind).toBe('int');
      expect(byName.nChord.defaultValue).toBe(4);
      expect(byName.nChord.min).toBe(1);
      expect(byName.nChord.max).toBe(40);
      expect(byName.nChord.step).toBe(1);
      expect(byName.nSpan.min).toBe(1);
      expect(byName.nSpan.max).toBe(80);

      expect(byName.foo.kind).toBe('float');
      expect(byName.foo.defaultValue).toBeCloseTo(1.25, 6);
      expect(byName.foo.min).toBeCloseTo(0.01, 6);
      expect(byName.foo.max).toBeCloseTo(12.5, 6);
      expect(byName.foo.positiveOnly).toBe(true);

      expect(byName.mach.kind).toBe('int');
      expect(byName.mach.defaultValue).toBe(1);
      expect(byName.mach.min).toBe(-10);
      expect(byName.mach.max).toBe(10);
      expect(byName.mach.positiveOnly).toBe(false);

      expect(byName.tw.defaultValue).toBe(1);
      expect(byName.tw.min).toBe(-10);
      expect(byName.tw.max).toBe(10);

      await page.waitForFunction(() => {
        const el = document.getElementById('fileNVort');
        return String(el?.textContent || '').trim() === '32';
      });

      const initialResolved = await page.evaluate(() => window.__trefftzTestHook.getResolvedAvlText());
      expect(initialResolved).toContain('4 1.0 8 1.0');
      expect(initialResolved).toContain('0.0 0.0 0.0 1.25 1');
      expect(initialResolved).toContain('0.0 5.0 0.0 0.63 2');

      const getSliderState = async (name) => page.evaluate((paramName) => {
        const slider = document.querySelector(`.template-param-row[data-template-param="${paramName}"] input[type="range"]`);
        if (!slider) return null;
        const min = Number(slider.min);
        const max = Number(slider.max);
        const step = Number(slider.step);
        const value = Number(slider.value);
        const count = step > 0 ? Math.round((max - min) / step) + 1 : 0;
        return { min, max, step, value, count };
      }, name);

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="tw"] input[type="range"]');
        if (!slider) return;
        slider.value = '4';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });
      const twBefore = await getSliderState('tw');
      expect(twBefore).toBeTruthy();
      await page.locator('.template-param-row[data-template-param="tw"] .template-param-name').dblclick();
      const twAfter = await getSliderState('tw');
      expect(twAfter).toBeTruthy();
      expect(Math.abs(twAfter.min - twBefore.min) > 1e-9 || Math.abs(twAfter.max - twBefore.max) > 1e-9).toBe(true);
      expect(twAfter.value).toBe(4);
      expect(Math.abs(((twAfter.min + twAfter.max) * 0.5) - twAfter.value)).toBeLessThanOrEqual((twAfter.step * 0.5) + 1e-9);
      expect(twAfter.count).toBeGreaterThanOrEqual(10);
      expect(twAfter.count).toBeLessThanOrEqual(20);

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="foo"] input[type="range"]');
        if (!slider) return;
        slider.value = '2.0';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });
      const fooBefore = await getSliderState('foo');
      expect(fooBefore).toBeTruthy();
      const fooName = page.locator('.template-param-row[data-template-param="foo"] .template-param-name');
      await fooName.dispatchEvent('pointerdown', { pointerType: 'touch', isPrimary: true, bubbles: true });
      await page.waitForTimeout(600);
      await fooName.dispatchEvent('pointerup', { pointerType: 'touch', isPrimary: true, bubbles: true });
      const fooAfter = await getSliderState('foo');
      expect(fooAfter).toBeTruthy();
      expect(Math.abs(fooAfter.min - fooBefore.min) > 1e-9 || Math.abs(fooAfter.max - fooBefore.max) > 1e-9).toBe(true);
      expect(fooAfter.value).toBeCloseTo(2.0, 6);
      expect(Math.abs(((fooAfter.min + fooAfter.max) * 0.5) - fooAfter.value)).toBeLessThanOrEqual((fooAfter.step * 0.5) + 1e-9);
      expect(fooAfter.count).toBeGreaterThanOrEqual(10);
      expect(fooAfter.count).toBeLessThanOrEqual(20);

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="mach"] input[type="range"]');
        if (!slider) return;
        slider.value = '-3';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });
      await expect(page.locator('.template-param-row[data-template-param="mach"] input[type="range"]')).toHaveValue('-3');

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="tw"] input[type="range"]');
        if (!slider) return;
        slider.value = '1';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="nChord"] input[type="range"]');
        if (!slider) return;
        slider.value = '6';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });

      await page.waitForFunction(() => {
        const el = document.getElementById('fileNVort');
        return String(el?.textContent || '').trim() === '48';
      });
      await expect(page.locator('.template-param-row[data-template-param="nChord"] input[type="range"]')).toHaveValue('6');
      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="nChord"] input[type="range"]');
        if (!slider) return;
        slider.value = '-3';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });
      await expect(page.locator('.template-param-row[data-template-param="nChord"] input[type="range"]')).toHaveValue('1');
      await page.waitForFunction(() => {
        const txt = window.__trefftzTestHook.getResolvedAvlText?.() || '';
        return txt.includes('1 1.0 8 1.0');
      });

      const resolvedAfter = await page.evaluate(() => window.__trefftzTestHook.getResolvedAvlText());
      expect(resolvedAfter).toContain('1 1.0 8 1.0');
      expect(resolvedAfter).not.toContain('1.0 1.0 8 1.0');

      await page.evaluate(() => {
        const slider = document.querySelector('.template-param-row[data-template-param="foo"] input[type="range"]');
        if (!slider) return;
        slider.value = '2';
        slider.dispatchEvent(new Event('input', { bubbles: true }));
      });
      await page.waitForTimeout(300);
      const resolvedMathAfter = await page.evaluate(() => window.__trefftzTestHook.getResolvedAvlText());
      expect(resolvedMathAfter).toContain('0.0 0.0 0.0 2.0 -3');
      expect(resolvedMathAfter).toContain('0.0 5.0 0.0 1.0 2');
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

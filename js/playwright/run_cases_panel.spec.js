import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('run cases panel uses editable rows with color and trash, and row click loads case', async ({ page }) => {
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

  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });

    await page.waitForSelector('#runCaseAddBtn');
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });

    await page.fill('#bank', '12.3');
    await page.fill('#cl', '0.77');
    const initialCount = await page.locator('#runCaseList .run-case-item').count();
    await page.click('#runCaseAddBtn');

    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(initialCount + 1);

    const newItem = page.locator('#runCaseList .run-case-item').last();
    await newItem.locator('.run-case-title').fill('Banked Test');
    await expect(newItem.locator('.run-case-title')).toHaveValue('Banked Test');

    await expect(page.locator('#runCaseList .run-case-color')).toHaveCount(initialCount + 1);
    await expect(page.locator('#runCaseList .run-case-delete')).toHaveCount(initialCount + 1);

    await page.fill('#bank', '1.0');
    await page.fill('#cl', '0.20');
    await newItem.evaluate((el) => {
      el.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    });

    await expect(page.locator('#bank')).toHaveValue('12.30');
    await expect(page.locator('#cl')).toHaveValue('0.770');
    await expect(newItem).toHaveClass(/active/);

    await newItem.locator('.run-case-delete').click();
    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(initialCount);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

test('run cases loader accepts AVL .run text format (plane.run)', async ({ page }) => {
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
      else if (filePath.endsWith('.run')) res.setHeader('Content-Type', 'text/plain; charset=utf-8');
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

  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });

    const runPath = path.resolve('third_party/avl/runs/plane.run');
    await page.setInputFiles('#runCasesInput', runPath);

    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(1);
    await expect(page.locator('#runCasesMeta')).toContainText('Loaded 1 run case(s) from plane.run');

    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-select')).toHaveValue('cl');
    await expect(page.locator('.constraint-row[data-var=\"beta\"] .constraint-select')).toHaveValue('beta');
    await expect(page.locator('.constraint-row[data-var=\"p\"] .constraint-select')).toHaveValue('p');
    await expect(page.locator('.constraint-row[data-var=\"q\"] .constraint-select')).toHaveValue('q');
    await expect(page.locator('.constraint-row[data-var=\"r\"] .constraint-select')).toHaveValue('r');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:aileron\"] .constraint-select')).toHaveValue('cmx');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:elevator\"] .constraint-select')).toHaveValue('cmy');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:rudder\"] .constraint-select')).toHaveValue('cmz');

    const vel = Number(await page.locator('#vel').inputValue());
    const rho = Number(await page.locator('#rho').inputValue());
    const gee = Number(await page.locator('#gee').inputValue());
    expect(vel).toBeGreaterThan(50);
    expect(vel).toBeLessThan(80);
    expect(rho).toBeGreaterThan(0);
    expect(rho).toBeLessThan(0.01);
    expect(gee).toBeGreaterThan(20);
    expect(gee).toBeLessThan(40);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

test('startup auto-loads plane.run/plane.mass with default plane, and loading a new AVL resets both panels', async ({ page }) => {
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.avl')) {
        res.setHeader('Content-Type', 'text/plain; charset=utf-8');
      }
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

  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });

    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(1);
    await expect(page.locator('#runCasesMeta')).toContainText('plane.run');
    await expect(page.locator('#massPropsMeta')).toContainText('plane.mass');
    await expect(page.locator('.constraint-row[data-var=\"alpha\"] .constraint-select')).toHaveValue('cl');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:aileron\"] .constraint-select')).toHaveValue('cmx');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:elevator\"] .constraint-select')).toHaveValue('cmy');
    await expect(page.locator('.constraint-row[data-var=\"ctrl:rudder\"] .constraint-select')).toHaveValue('cmz');
    const rhoAfterDefaults = Number(await page.locator('#rho').inputValue());
    expect(rhoAfterDefaults).toBeGreaterThan(0);
    expect(rhoAfterDefaults).toBeLessThan(0.001);

    await page.click('#trimBtn');
    await page.waitForFunction(() => {
      const s = window.__trefftzTestHook?.getLastExecSummary?.();
      return s && Number.isFinite(s.CDTOT) && Number.isFinite(s.CDVTOT);
    });
    const cdiRaw = await page.evaluate(() => {
      const s = window.__trefftzTestHook?.getLastExecSummary?.();
      return Number(s?.CDTOT) - Number(s?.CDVTOT);
    });
    expect(cdiRaw).toBeGreaterThan(0.001);

    const avlPath = path.resolve('third_party/avl/runs/plane.avl');
    await page.setInputFiles('#fileInput', avlPath);

    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(0);
    await expect(page.locator('#runCasesMeta')).toContainText('No run cases loaded.');
    await expect(page.locator('#massPropsMeta')).toContainText('model.mass');
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

test('Load AVL input routes .run and .mass files to run-cases and mass panels', async ({ page }) => {
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.avl')) {
        res.setHeader('Content-Type', 'text/plain; charset=utf-8');
      }
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

  try {
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });

    const runPayload = JSON.stringify({
      cases: [
        {
          name: 'LoadAVL-Case',
          inputs: {
            bank: 7.7,
            cl: 0.44,
            vel: 31.2,
            mass: 123.4,
            rho: 0.321,
            gee: 9.99,
            flightMode: 'level',
          },
          constraints: [
            { variable: 'alpha', constraint: 'cl', numeric: 0.44 },
          ],
        },
      ],
      selectedIndex: 0,
    }, null, 2);
    const massPayload = [
      'g = 9.81',
      'rho = 0.321',
      '12.5 0.1 0.2 0.3 1.1 2.2 3.3 0.0 0.0 0.0',
      '',
    ].join('\n');

    await page.setInputFiles('#fileInput', [
      { name: 'autoload.run', mimeType: 'text/plain', buffer: Buffer.from(runPayload, 'utf8') },
      { name: 'autoload.mass', mimeType: 'text/plain', buffer: Buffer.from(massPayload, 'utf8') },
    ]);

    await expect(page.locator('#runCaseList .run-case-item')).toHaveCount(1);
    await expect(page.locator('#runCasesMeta')).toContainText('autoload.run');
    await expect(page.locator('#massPropsMeta')).toContainText('autoload.mass');
    await expect(page.locator('#massTotal')).toHaveValue('12.5000');
    await expect(page.locator('#bank')).toHaveValue('7.70');
    await expect(page.locator('#cl')).toHaveValue('0.440');
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('Load Example menu loads AVL with companion run/mass and airfoils', async ({ page }) => {
  test.setTimeout(90000);
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.avl') || filePath.endsWith('.dat')) {
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
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 20000 });

    let wingCompanionRequests = 0;
    page.on('request', (req) => {
      const url = String(req.url() || '').toLowerCase();
      if (url.includes('/wing.run') || url.includes('/wing.mass')) {
        wingCompanionRequests += 1;
      }
    });

    const saveBox = await page.locator('#saveBtn').boundingBox();
    const exampleBox = await page.locator('#loadExampleSelect').boundingBox();
    expect(saveBox).toBeTruthy();
    expect(exampleBox).toBeTruthy();
    expect(exampleBox.x).toBeGreaterThan(saveBox.x);
    expect(Math.abs(exampleBox.height - saveBox.height)).toBeLessThanOrEqual(1.5);
    const exampleWidthBefore = Number(exampleBox.width);
    expect(exampleWidthBefore).toBeLessThanOrEqual(150);
    expect(exampleWidthBefore).toBeGreaterThanOrEqual(100);

    await page.waitForFunction(() => {
      const select = document.getElementById('loadExampleSelect');
      return Boolean(select && select.options.length > 1 && !select.disabled);
    });
    await expect(page.locator('#loadExampleSelect option[value="plane.avl"]')).toHaveCount(1);
    await expect(page.locator('#loadExampleSelect option[value="b737.avl"]')).toHaveCount(1);

    await page.selectOption('#loadExampleSelect', 'b737.avl');

    await expect(page.locator('#fileMeta')).toContainText('b737.avl', { timeout: 20000 });
    await expect(page.locator('#runCasesMeta')).toContainText('b737.run', { timeout: 20000 });
    await expect(page.locator('#massPropsMeta')).toContainText('b737.mass', { timeout: 20000 });
    const exampleBoxAfter = await page.locator('#loadExampleSelect').boundingBox();
    expect(exampleBoxAfter).toBeTruthy();
    expect(Math.abs(Number(exampleBoxAfter.width) - exampleWidthBefore)).toBeLessThanOrEqual(1.5);

    await page.waitForFunction(() => {
      const rows = Array.from(document.querySelectorAll('#airfoilFilesList .airfoil-file-row'));
      return rows.length > 0 && rows.every((row) => row.classList.contains('ok'));
    }, null, { timeout: 20000 });

    const rowCount = await page.locator('#airfoilFilesList .airfoil-file-row').count();
    expect(rowCount).toBeGreaterThan(0);
    await expect(page.locator('#airfoilFilesList .airfoil-file-row.missing')).toHaveCount(0);
    await expect(page.locator('#airfoilFilesList .airfoil-file-row.loading')).toHaveCount(0);

    await page.selectOption('#loadExampleSelect', 'wing.avl');
    await expect(page.locator('#fileMeta')).toContainText('wing.avl', { timeout: 20000 });
    await page.waitForTimeout(300);
    expect(wingCompanionRequests).toBe(0);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

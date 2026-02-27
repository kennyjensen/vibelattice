import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('mass properties panel loads plane.mass and populates cg/inertia fields', async ({ page }) => {
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
      else if (filePath.endsWith('.mass')) res.setHeader('Content-Type', 'text/plain; charset=utf-8');
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

    await expect(page.locator('#massPropsInput')).toHaveCount(1);
    await expect(page.locator('#massPropsSaveBtn')).toHaveCount(1);
    await expect(page.locator('#massPropsApplyBtn')).toHaveCount(1);
    await expect(page.locator('#massTotal')).toHaveCount(1);
    await expect(page.locator('#massIyz')).toHaveCount(1);

    const before = await page.evaluate(() => ({
      massTotal: document.querySelector('#massTotal')?.value || '',
      massXcg: document.querySelector('#massXcg')?.value || '',
      massYcg: document.querySelector('#massYcg')?.value || '',
      massZcg: document.querySelector('#massZcg')?.value || '',
    }));

    const massPath = path.resolve('third_party/avl/runs/plane.mass');
    await page.setInputFiles('#massPropsInput', massPath);

    await expect(page.locator('#massPropsMeta')).toHaveText('Loaded file: plane.mass');
    await expect(page.locator('#massFileTotal')).toHaveValue('0.1773');
    await expect(page.locator('#massFileXcg')).toHaveValue('0.0246');
    await expect(page.locator('#massFileYcg')).toHaveValue('0.0000');
    await expect(page.locator('#massFileZcg')).toHaveValue('0.2239');
    await expect(page.locator('#massFileIxx')).toHaveValue('1.3500');
    await expect(page.locator('#massFileIyy')).toHaveValue('0.7509');
    await expect(page.locator('#massFileIzz')).toHaveValue('2.0950');
    await expect(page.locator('#massTotal')).toHaveValue(before.massTotal);
    await expect(page.locator('#massXcg')).toHaveValue(before.massXcg);
    await expect(page.locator('#massYcg')).toHaveValue(before.massYcg);
    await expect(page.locator('#massZcg')).toHaveValue(before.massZcg);

    await page.click('#massPropsApplyBtn');
    await expect(page.locator('#massTotal')).toHaveValue('0.1773');
    await expect(page.locator('#massXcg')).toHaveValue('0.0246');
    await expect(page.locator('#massYcg')).toHaveValue('0.0000');
    await expect(page.locator('#massZcg')).toHaveValue('0.2239');
    await expect(page.locator('#massIxx')).toHaveValue('1.3500');
    await expect(page.locator('#massIyy')).toHaveValue('0.7509');
    await expect(page.locator('#massIzz')).toHaveValue('2.0950');
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

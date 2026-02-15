import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('github star link is in title bar on desktop and below eigen plot on mobile', async ({ page }) => {
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
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });

    await expect(page.locator('#githubStarDesktop')).toBeVisible();
    await expect(page.locator('#githubStarDesktop .github-button')).toHaveAttribute('href', 'https://github.com/kennyjensen/vibelattice');
    await expect(page.locator('#githubStarMobile')).toBeHidden();

    const desktopInTitleBar = await page.evaluate(() => {
      const desktop = document.querySelector('#githubStarDesktop');
      return Boolean(desktop?.closest('.title-bar'));
    });
    expect(desktopInTitleBar).toBeTruthy();

    await page.setViewportSize({ width: 430, height: 932 });
    await page.waitForTimeout(100);

    await expect(page.locator('#githubStarDesktop')).toBeHidden();
    await expect(page.locator('#githubStarMobile')).toBeVisible();
    await expect(page.locator('#githubStarMobile .github-button')).toHaveAttribute('href', 'https://github.com/kennyjensen/vibelattice');

    const mobileOrder = await page.evaluate(() => {
      const eigen = document.querySelector('#eigenPanel');
      const mobile = document.querySelector('#githubStarMobile');
      if (!eigen || !mobile || !eigen.parentElement) return null;
      const children = Array.from(eigen.parentElement.children);
      return {
        eigenIdx: children.indexOf(eigen),
        mobileIdx: children.indexOf(mobile),
      };
    });
    expect(mobileOrder).toBeTruthy();
    expect(mobileOrder.mobileIdx).toBeGreaterThan(mobileOrder.eigenIdx);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

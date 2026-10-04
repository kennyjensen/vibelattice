import { test, expect } from '@playwright/test';
import http from 'node:http';
import fs from 'node:fs/promises';
import path from 'node:path';

const apps = [
  { name: 'Vibefoil', symbol: 'Vf', url: 'https://www.vibefoil.com' },
  { name: 'VibeSim', symbol: 'Vs', url: 'https://sim.vibefoil.com' },
  { name: 'Alula', symbol: 'Al', url: 'https://alula.vibefoil.com' },
];
const contentTypes = {
  '.html': 'text/html',
  '.js': 'application/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.wasm': 'application/wasm',
};
let server;
let baseURL;

test.beforeAll(async () => {
  const root = path.resolve('.');
  server = http.createServer(async (req, res) => {
    const pathname = new URL(req.url, 'http://localhost').pathname;
    const filePath = path.join(root, pathname === '/' ? 'index.html' : decodeURIComponent(pathname));
    try {
      const data = await fs.readFile(filePath);
      res.writeHead(200, { 'Content-Type': contentTypes[path.extname(filePath)] || 'application/octet-stream' });
      res.end(data);
    } catch {
      res.writeHead(404);
      res.end('Not found');
    }
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  baseURL = `http://127.0.0.1:${server.address().port}`;
});

test.afterAll(async () => {
  await new Promise((resolve) => server.close(resolve));
});

test.beforeEach(async ({ context, page }) => {
  // Keep the real app and worker requests; isolate third-party widgets and destinations.
  await context.route(/^https?:\/\//, (route) => {
    if (route.request().url().startsWith(`${baseURL}/`)) return route.continue();
    return route.fulfill({ status: 200, contentType: 'text/html', body: '' });
  });
  await page.goto(`${baseURL}/index.html`, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState));
});

test('title bar links show element symbols and open the related apps with keyboard navigation', async ({ page }) => {
  const nav = page.getByRole('navigation', { name: 'More airfoil and flight tools' });
  await expect(nav).toBeVisible();
  await expect(nav.getByRole('link')).toHaveCount(3);
  expect(await nav.evaluate((el) => Boolean(el.closest('.title-bar')))).toBe(true);

  await page.locator('.title-sub a').focus();
  for (const app of apps) {
    const link = nav.getByRole('link', { name: `${app.name} (opens in a new tab)`, exact: true });
    await expect(link).toHaveAttribute('href', app.url);
    await expect(link).toHaveAttribute('target', '_blank');
    await expect(link).toHaveAttribute('rel', /\bnoopener\b/);
    await expect(link).toHaveAttribute('rel', /\bnoreferrer\b/);
    await expect(link.locator('.element-symbol')).toHaveText(app.symbol);
    await expect(link.locator('.element-symbol')).toHaveAttribute('aria-hidden', 'true');
    await expect(link.locator('.element-name')).toHaveText(app.name);

    await page.keyboard.press('Tab');
    await expect(link).toBeFocused();
    await expect(link).toHaveCSS('outline-style', 'solid');
    const popupPromise = page.waitForEvent('popup');
    await page.keyboard.press('Enter');
    const popup = await popupPromise;
    await expect(popup).toHaveURL(`${app.url}/`);
    await popup.close();
    await expect(page).toHaveURL(`${baseURL}/index.html`);
  }
});

test('element tiles fit beside the title on desktop and narrow mobile screens', async ({ page }) => {
  const nav = page.getByRole('navigation', { name: 'More airfoil and flight tools' });
  for (const width of [1280, 901, 900, 430, 375, 320]) {
    await page.setViewportSize({ width, height: 900 });
    await expect(nav).toBeVisible();
    const title = await page.locator('.title-text').boundingBox();
    const header = await page.locator('.app-header').boundingBox();
    const navigation = await nav.boundingBox();
    expect(title.x + title.width, `title should not overlap links at ${width}px`).toBeLessThanOrEqual(navigation.x);
    expect(navigation.x + navigation.width).toBeLessThanOrEqual(width);
    expect(navigation.y).toBeGreaterThanOrEqual(header.y);
    expect(navigation.y + navigation.height).toBeLessThanOrEqual(header.y + header.height);

    let previousRight = navigation.x;
    for (const app of apps) {
      const link = nav.getByRole('link', { name: `${app.name} (opens in a new tab)`, exact: true });
      await expect(link).toBeInViewport();
      const tile = await link.boundingBox();
      expect(tile.width).toBe(width <= 900 ? 44 : 54);
      expect(tile.height).toBe(tile.width);
      expect(tile.x).toBeGreaterThanOrEqual(previousRight);
      previousRight = tile.x + tile.width;
      const label = await link.locator('.element-name').boundingBox();
      expect(label.x).toBeGreaterThanOrEqual(tile.x);
      expect(label.x + label.width).toBeLessThanOrEqual(tile.x + tile.width);
    }

    if (width > 900) {
      await expect(page.locator('#githubStarDesktop')).toBeVisible();
      const star = await page.locator('#githubStarDesktop').boundingBox();
      expect(navigation.x + navigation.width).toBeLessThanOrEqual(star.x);
    } else {
      await expect(page.locator('#githubStarDesktop')).toBeHidden();
    }
    const titleFits = await page.locator('.title').evaluate((el) => el.scrollWidth <= el.clientWidth);
    expect(titleFits, `title should fit at ${width}px`).toBe(true);
  }
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('github star link and note are positioned correctly on desktop and mobile', async ({ page }) => {
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
    for (const entry of ['/index.html']) {
      await page.setViewportSize({ width: 1280, height: 900 });
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });

      await expect(page.locator('#githubStarDesktop')).toBeVisible();
      await expect(page.locator('#githubStarDesktop .github-star-note')).toHaveText('XROTOR port at 100 stars!');
      await expect(page.locator('#githubStarMobile')).toBeHidden();

      const desktopHasStarLink = await page.evaluate(() => {
        const scope = document.querySelector('#githubStarDesktop');
        if (!scope) return false;
        const anchor = scope.querySelector('a.github-button[href]');
        if (anchor && anchor.href.includes('github.com/kennyjensen/vibelattice')) return true;
        const iframe = scope.querySelector('iframe[src]');
        return Boolean(iframe && String(iframe.getAttribute('src') || '').includes('github.com/kennyjensen/vibelattice'));
      });
      expect(desktopHasStarLink).toBeTruthy();

      const desktopPlacement = await page.evaluate(() => {
        const desktop = document.querySelector('#githubStarDesktop');
        const button = desktop?.querySelector('.github-button');
        const note = desktop?.querySelector('.github-star-note');
        if (!desktop || !button || !note) return null;
        const buttonRect = button.getBoundingClientRect();
        const noteRect = note.getBoundingClientRect();
        return {
          inTitleBar: Boolean(desktop.closest('.title-bar')),
          noteBelowButton: noteRect.top >= (buttonRect.bottom - 1),
        };
      });
      expect(desktopPlacement).toBeTruthy();
      expect(desktopPlacement.inTitleBar).toBeTruthy();
      expect(desktopPlacement.noteBelowButton).toBeTruthy();

      await page.setViewportSize({ width: 430, height: 932 });
      await page.reload({ waitUntil: 'domcontentloaded' });

      await expect(page.locator('#githubStarDesktop')).toBeHidden();
      await expect(page.locator('#githubStarMobile')).toBeVisible();
      await expect(page.locator('#githubStarMobile .github-star-note')).toHaveText('XROTOR port at 100 stars!');

      const mobileHasStarLink = await page.evaluate(() => {
        const scope = document.querySelector('#githubStarMobile');
        if (!scope) return false;
        const anchor = scope.querySelector('a.github-button[href]');
        if (anchor && anchor.href.includes('github.com/kennyjensen/vibelattice')) return true;
        const iframe = scope.querySelector('iframe[src]');
        return Boolean(iframe && String(iframe.getAttribute('src') || '').includes('github.com/kennyjensen/vibelattice'));
      });

      const mobilePlacement = await page.evaluate(() => {
        const eigen = document.querySelector('#eigenPanel');
        const mobile = document.querySelector('#githubStarMobile');
        const note = mobile?.querySelector('.github-star-note');
        const starEl = mobile?.querySelector('a.github-button, iframe');
        if (!eigen || !mobile || !eigen.parentElement || !note) return null;
        const children = Array.from(eigen.parentElement.children);
        const noteRect = note.getBoundingClientRect();
        const buttonRect = starEl ? starEl.getBoundingClientRect() : noteRect;
        return {
          eigenIdx: children.indexOf(eigen),
          mobileIdx: children.indexOf(mobile),
          hasStar: Boolean(starEl),
          noteLeftOfStar: noteRect.left <= buttonRect.left,
        };
      });
      expect(mobilePlacement).toBeTruthy();
      expect(mobilePlacement.mobileIdx).toBeGreaterThan(mobilePlacement.eigenIdx);
      if (mobilePlacement.hasStar) {
        expect(mobilePlacement.noteLeftOfStar).toBeTruthy();
      } else {
        expect(mobileHasStarLink).toBe(false);
      }
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

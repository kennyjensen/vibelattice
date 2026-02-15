import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('mobile exposes 4 tabs with editor page, desktop keeps editor in plots column', async ({ page }) => {
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
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 15000 });

    const tabLabels = await page.locator('.page-indicator .nav-btn').allTextContents();
    expect(tabLabels.map((t) => t.trim())).toEqual(['Editor', 'Settings', 'Plots', 'Outputs']);

    const waitNearestColumn = async (columnId) => {
      await page.waitForFunction((targetId) => {
        const app = document.getElementById('appRoot');
        if (!app) return false;
        const columns = ['editorCol', 'settingsCol', 'plotsCol', 'outputsCol']
          .map((id) => document.getElementById(id))
          .filter((el) => el);
        if (!columns.length) return false;
        let nearest = columns[0];
        let best = Number.POSITIVE_INFINITY;
        columns.forEach((col) => {
          const d = Math.abs(app.scrollLeft - col.offsetLeft);
          if (d < best) {
            best = d;
            nearest = col;
          }
        });
        return nearest.id === targetId;
      }, columnId);
    };

    await waitNearestColumn('plotsCol');
    await expect(page.locator('#navPlots')).toHaveClass(/active/);

    const mobileOrder = await page.evaluate(() => ({
      editorLeft: document.getElementById('editorCol')?.offsetLeft ?? -1,
      settingsLeft: document.getElementById('settingsCol')?.offsetLeft ?? -1,
      plotsLeft: document.getElementById('plotsCol')?.offsetLeft ?? -1,
      outputsLeft: document.getElementById('outputsCol')?.offsetLeft ?? -1,
      appHeight: document.getElementById('appRoot')?.clientHeight ?? 0,
      editorColHeight: document.getElementById('editorCol')?.clientHeight ?? 0,
      editorPanelHeight: document.getElementById('editorPanel')?.clientHeight ?? 0,
      editorBodyHeight: document.querySelector('#editorPanel .panel-body')?.clientHeight ?? 0,
      fileEditorHeight: document.getElementById('fileEditor')?.clientHeight ?? 0,
      editorInEditorCol: Boolean(
        document.getElementById('editorPanel')
        && document.getElementById('editorCol')?.contains(document.getElementById('editorPanel')),
      ),
      editorInPlotsCol: Boolean(
        document.getElementById('editorPanel')
        && document.getElementById('plotsCol')?.contains(document.getElementById('editorPanel')),
      ),
    }));
    expect(mobileOrder.editorLeft).toBeGreaterThanOrEqual(0);
    expect(mobileOrder.plotsLeft).toBeGreaterThanOrEqual(0);
    expect(mobileOrder.settingsLeft).toBeGreaterThanOrEqual(0);
    expect(mobileOrder.outputsLeft).toBeGreaterThanOrEqual(0);
    expect(mobileOrder.editorLeft).toBeLessThan(mobileOrder.settingsLeft);
    expect(mobileOrder.settingsLeft).toBeLessThan(mobileOrder.plotsLeft);
    expect(mobileOrder.plotsLeft).toBeLessThan(mobileOrder.outputsLeft);
    expect(mobileOrder.editorInEditorCol).toBeTruthy();
    expect(mobileOrder.editorInPlotsCol).toBeFalsy();
    expect(mobileOrder.editorPanelHeight).toBeGreaterThan(mobileOrder.appHeight * 0.88);
    expect(mobileOrder.fileEditorHeight).toBeGreaterThan(mobileOrder.editorBodyHeight * 0.75);

    await page.click('#navEditor');
    await waitNearestColumn('editorCol');

    await page.click('#navSettings');
    await waitNearestColumn('settingsCol');

    await page.click('#navPlots');
    await waitNearestColumn('plotsCol');

    await page.click('#navOutputs');
    await waitNearestColumn('outputsCol');

    await page.setViewportSize({ width: 1280, height: 900 });
    await page.waitForFunction(() => !window.matchMedia('(max-width: 900px)').matches);
    const desktopOrder = await page.evaluate(() => ({
      settingsLeft: document.getElementById('settingsCol')?.getBoundingClientRect().left ?? -1,
      plotsLeft: document.getElementById('plotsCol')?.getBoundingClientRect().left ?? -1,
      editorColDisplay: window.getComputedStyle(document.getElementById('editorCol')).display,
      editorInPlotsCol: Boolean(
        document.getElementById('editorPanel')
        && document.getElementById('plotsCol')?.contains(document.getElementById('editorPanel')),
      ),
    }));
    expect(desktopOrder.settingsLeft).toBeLessThan(desktopOrder.plotsLeft);
    expect(desktopOrder.editorColDisplay).toBe('none');
    expect(desktopOrder.editorInPlotsCol).toBeTruthy();
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

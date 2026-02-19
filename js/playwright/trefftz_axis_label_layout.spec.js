import { test, expect } from '@playwright/test';
import path from 'node:path';
import http from 'node:http';
import fs from 'node:fs';

test('trefftz axis labels are outside tick label bounds', async ({ page }) => {
  const root = process.cwd();
  const server = http.createServer((req, res) => {
    const urlPath = decodeURIComponent(req.url || '/');
    const safePath = urlPath.split('?')[0].replace(/^\/+/, '');
    const filePath = path.join(root, safePath || 'index.html');
    if (!filePath.startsWith(root)) {
      res.writeHead(403);
      res.end('Forbidden');
      return;
    }
    try {
      const data = fs.readFileSync(filePath);
      const ext = path.extname(filePath);
      const type = ext === '.html' ? 'text/html'
        : ext === '.js' ? 'text/javascript'
        : ext === '.css' ? 'text/css'
        : ext === '.wasm' ? 'application/wasm'
        : 'application/octet-stream';
      res.writeHead(200, { 'Content-Type': type });
      res.end(data);
    } catch {
      res.writeHead(404);
      res.end('Not found');
    }
  });
  await new Promise((resolve) => server.listen(0, resolve));
  const { port } = server.address();

  await page.setViewportSize({ width: 1200, height: 800 });
  await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });

  await page.waitForFunction(() => {
    const log = document.getElementById('debugLog')?.textContent || '';
    return log.includes('App module loaded.') || log.includes('App module failed:');
  });
  const debugText = await page.evaluate(() => document.getElementById('debugLog')?.textContent || '');
  expect(debugText).toContain('App module loaded.');
  await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');
  const trefData = {
    strips: [
      [-7.0, 0.0, 0.1, 0.2, 0.15, -0.01, 1],
      [0.0, 0.0, 0.15, 0.25, 0.2, -0.015, 1],
      [7.0, 0.0, 0.1, 0.2, 0.15, -0.01, 1],
    ],
    surfaces: [{ start: 0, count: 3 }],
    cref: 1.0,
  };
  await page.evaluate((data) => {
    window.__trefftzTestHook.setTrefftzData(data);
  }, trefData);
  await page.evaluate(async () => {
    if (document.fonts) {
      await document.fonts.ready;
    }
  });
  await page.evaluate((data) => {
    window.__trefftzTestHook.setTrefftzData(data);
  }, trefData);
  await page.evaluate(() => new Promise(requestAnimationFrame));
  await page.waitForFunction(() => window.__trefftzTestHook?.layoutReady === true);

  await page.waitForSelector('.trefftz-tick.left');

  const measure = async () => page.evaluate(() => {
    const leftLabel = document.querySelector('.trefftz-axis-label.y-left')?.getBoundingClientRect();
    const rightLabel = document.querySelector('.trefftz-axis-label.y-right')?.getBoundingClientRect();
    const xLabel = document.querySelector('.trefftz-axis-label.x')?.getBoundingClientRect();
    const leftTick = document.querySelector('.trefftz-tick.left')?.getBoundingClientRect();
    const rightTick = document.querySelector('.trefftz-tick.right')?.getBoundingClientRect();
    const bottomTick = document.querySelector('.trefftz-tick.bottom')?.getBoundingClientRect();
    const axisLeft = document.querySelector('.trefftz-axis-line.left')?.getBoundingClientRect();
    const axisRight = document.querySelector('.trefftz-axis-line.right')?.getBoundingClientRect();
    const axisBottom = document.querySelector('.trefftz-axis-line.bottom')?.getBoundingClientRect();
    const gridH = Array.from(document.querySelectorAll('.trefftz-grid-line.h')).map((el) => el.getBoundingClientRect());
    const gridV = Array.from(document.querySelectorAll('.trefftz-grid-line.v')).map((el) => el.getBoundingClientRect());
    const ticksLeft = Array.from(document.querySelectorAll('.trefftz-tick.left')).map((el) => el.getBoundingClientRect());
    const ticksBottom = Array.from(document.querySelectorAll('.trefftz-tick.bottom')).map((el) => el.getBoundingClientRect());
    return {
      leftLabel,
      rightLabel,
      xLabel,
      leftTick,
      rightTick,
      bottomTick,
      axisLeft,
      axisRight,
      axisBottom,
      gridH,
      gridV,
      ticksLeft,
      ticksBottom,
    };
  });
  const bounds = await measure();

  expect(bounds.leftLabel).toBeTruthy();
  expect(bounds.rightLabel).toBeTruthy();
  expect(bounds.xLabel).toBeTruthy();
  expect(bounds.leftTick).toBeTruthy();
  expect(bounds.rightTick).toBeTruthy();
  expect(bounds.bottomTick).toBeTruthy();

  expect(bounds.leftLabel.right).toBeLessThan(bounds.leftTick.left);
  expect(bounds.rightLabel.left).toBeGreaterThan(bounds.rightTick.right);
  expect(bounds.xLabel.top).toBeGreaterThan(bounds.bottomTick.bottom);

  const panelBounds = await page.evaluate(() => {
    const panel = document.querySelector('#trefftz')?.parentElement;
    return panel?.getBoundingClientRect();
  });
  expect(panelBounds).toBeTruthy();
  const leftOffset = Math.abs(bounds.leftLabel.left - panelBounds.left);
  const rightOffset = Math.abs(panelBounds.right - bounds.rightLabel.right);
  const bottomOffset = Math.abs(panelBounds.bottom - bounds.xLabel.bottom);
  expect(Math.abs(leftOffset - 3)).toBeLessThanOrEqual(1);
  expect(Math.abs(rightOffset - 3)).toBeLessThanOrEqual(1);
  expect(Math.abs(bottomOffset - 3)).toBeLessThanOrEqual(1);

  expect(bounds.axisLeft).toBeTruthy();
  expect(bounds.axisRight).toBeTruthy();
  expect(bounds.axisBottom).toBeTruthy();

  const labelGapLeft = bounds.leftTick.left - bounds.leftLabel.right;
  const labelGapRight = bounds.rightLabel.left - bounds.rightTick.right;
  const bottomTickBottoms = bounds.ticksBottom.map((tick) => tick.bottom);
  const labelGapBottom = bounds.xLabel.top - Math.max(...bottomTickBottoms);
  expect(Math.abs(labelGapLeft - 3)).toBeLessThanOrEqual(1);
  expect(Math.abs(labelGapRight - 3)).toBeLessThanOrEqual(1);
  expect(labelGapBottom).toBeLessThanOrEqual(2);

  const tickGapLeft = bounds.axisLeft.left - bounds.leftTick.right;
  const tickGapRight = bounds.rightTick.left - bounds.axisRight.right;
  const tickGapBottom = bounds.bottomTick.top - bounds.axisBottom.bottom;
  
  expect(Math.abs(tickGapLeft - 3)).toBeLessThanOrEqual(1);
  expect(Math.abs(tickGapRight - 3)).toBeLessThanOrEqual(1);
  expect(Math.abs(tickGapBottom - 3)).toBeLessThanOrEqual(1);

  expect(bounds.gridH.length).toBeGreaterThan(0);
  expect(bounds.gridV.length).toBeGreaterThan(0);
  expect(bounds.ticksLeft.length).toBeGreaterThan(0);
  expect(bounds.ticksBottom.length).toBeGreaterThan(0);

  const countH = Math.min(bounds.gridH.length, bounds.ticksLeft.length);
  for (let i = 0; i < countH; i += 1) {
    const tickCenter = bounds.ticksLeft[i].top + bounds.ticksLeft[i].height / 2;
    const gridY = bounds.gridH[i].top;
    expect(Math.abs(tickCenter - gridY)).toBeLessThanOrEqual(1);
  }

  const countV = Math.min(bounds.gridV.length, bounds.ticksBottom.length);
  for (let i = 0; i < countV; i += 1) {
    const tickCenter = bounds.ticksBottom[i].left + bounds.ticksBottom[i].width / 2;
    const gridX = bounds.gridV[i].left;
    expect(Math.abs(tickCenter - gridX)).toBeLessThanOrEqual(1);
  }

  for (const line of bounds.gridH) {
    expect(line.left).toBeGreaterThanOrEqual(bounds.axisLeft.left - 1);
    expect(line.right).toBeLessThanOrEqual(bounds.axisRight.right + 1);
    expect(line.top).toBeGreaterThanOrEqual(bounds.axisLeft.top - 1);
    expect(line.bottom).toBeLessThanOrEqual(bounds.axisBottom.bottom + 1);
  }

  for (const line of bounds.gridV) {
    expect(line.left).toBeGreaterThanOrEqual(bounds.axisLeft.left - 1);
    expect(line.right).toBeLessThanOrEqual(bounds.axisRight.right + 1);
    expect(line.top).toBeGreaterThanOrEqual(bounds.axisLeft.top - 1);
    expect(line.bottom).toBeLessThanOrEqual(bounds.axisBottom.bottom + 1);
  }

  await page.evaluate(() => {
    const panel = document.querySelector('#trefftz')?.parentElement;
    if (!panel) return;
    const current = panel.getBoundingClientRect().height;
    panel.style.height = `${current + 120}px`;
  });
  await page.evaluate(() => new Promise(requestAnimationFrame));
  const resized = await measure();
  const resizedBottoms = resized.ticksBottom.map((tick) => tick.bottom);
  const resizedGap = resized.xLabel.top - Math.max(...resizedBottoms);
  expect(resizedGap).toBeLessThanOrEqual(2);

  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate((data) => {
    window.__trefftzTestHook.setTrefftzData(data);
  }, trefData);
  await page.evaluate(() => new Promise(requestAnimationFrame));
  await page.waitForFunction(() => window.__trefftzTestHook?.layoutReady === true);

  const mobileBounds = await measure();
  const mobileBottomTickBottoms = mobileBounds.ticksBottom.map((tick) => tick.bottom);
  const mobileGap = mobileBounds.xLabel.top - Math.max(...mobileBottomTickBottoms);
  expect(mobileGap).toBeLessThanOrEqual(2);

  server.close();
});

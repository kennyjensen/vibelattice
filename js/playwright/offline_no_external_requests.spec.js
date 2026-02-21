import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

test('startup does not request external network resources', async ({ page }) => {
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
      else if (filePath.endsWith('.wasm')) res.setHeader('Content-Type', 'application/wasm');
      else if (filePath.endsWith('.woff2')) res.setHeader('Content-Type', 'font/woff2');
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
  const base = `http://127.0.0.1:${port}`;
  const external = [];
  const vendorFailures = [];

  page.on('request', (req) => {
    const url = String(req.url() || '');
    const isLocalHttp = url.startsWith(`${base}/`);
    const isLocalSocket = url.startsWith(`ws://127.0.0.1:${port}`) || url.startsWith(`wss://127.0.0.1:${port}`);
    const isBrowserLocal = url.startsWith('about:') || url.startsWith('data:') || url.startsWith('blob:') || url.startsWith('devtools:') || url.startsWith('chrome-extension:');
    if (isLocalHttp || isLocalSocket || isBrowserLocal) return;
    external.push(url);
  });
  page.on('requestfailed', (req) => {
    const url = String(req.url() || '');
    if (url.includes('/third_party/vendor/')) {
      vendorFailures.push(`requestfailed: ${url}`);
    }
  });
  page.on('response', (res) => {
    const url = String(res.url() || '');
    if (url.includes('/third_party/vendor/') && res.status() >= 400) {
      vendorFailures.push(`http${res.status()}: ${url}`);
    }
  });

  try {
    await page.goto(`${base}/index.html`, { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getViewerOverlayState), null, { timeout: 30000 });
    await expect(page.locator('#viewer canvas')).toHaveCount(1);
    await page.waitForTimeout(800);
    expect(external).toEqual([]);
    expect(vendorFailures).toEqual([]);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

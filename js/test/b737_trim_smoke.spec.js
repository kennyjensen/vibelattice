import { test, expect } from '@playwright/test';
import path from 'node:path';
import http from 'node:http';
import fs from 'node:fs';

test('b737.avl loads and trim completes', async ({ page }) => {
  const root = process.cwd();
  const server = http.createServer((req, res) => {
    const urlPath = decodeURIComponent(req.url || '/');
    const safePath = urlPath.split('?')[0].replace(/^\/+/, '');
    const filePath = path.join(root, safePath || 'js/dist/index.html');
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
  await page.goto(`http://127.0.0.1:${port}/js/dist/index.html`, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => {
    const log = document.getElementById('debugLog')?.textContent || '';
    return log.includes('App module loaded.') || log.includes('App module failed:');
  });
  const debugText = await page.evaluate(() => document.getElementById('debugLog')?.textContent || '');
  expect(debugText).toContain('App module loaded.');

  const avlPath = path.join(root, 'third_party', 'avl', 'runs', 'b737.avl');
  await page.setInputFiles('#fileInput', avlPath);
  await page.waitForFunction(() => {
    const log = document.getElementById('debugLog')?.textContent || '';
    return log.includes('Loaded file: b737.avl');
  });

  await page.click('#trimBtn');
  await page.waitForFunction(() => {
    const log = document.getElementById('debugLog')?.textContent || '';
    return log.includes('Worker EXEC done');
  }, { timeout: 60000 });

  const finalLog = await page.evaluate(() => document.getElementById('debugLog')?.textContent || '');
  expect(finalLog).not.toContain('Error:');
  expect(finalLog).not.toContain('Promise rejection');
  expect(finalLog).not.toContain('EXEC worker error');

  server.close();
});

import { test, expect } from '@playwright/test';
import http from 'node:http';
import fs from 'node:fs/promises';
import path from 'node:path';

const root = path.resolve('.');

function contentTypeFor(filePath) {
  if (filePath.endsWith('.html')) return 'text/html; charset=utf-8';
  if (filePath.endsWith('.js')) return 'application/javascript; charset=utf-8';
  if (filePath.endsWith('.css')) return 'text/css; charset=utf-8';
  if (filePath.endsWith('.wasm')) return 'application/wasm';
  if (filePath.endsWith('.json')) return 'application/json; charset=utf-8';
  if (filePath.endsWith('.ico')) return 'image/x-icon';
  return 'application/octet-stream';
}

async function createStaticServer() {
  const server = http.createServer(async (req, res) => {
    const reqPath = (req.url || '/').split('?')[0];
    const clean = decodeURIComponent(reqPath === '/' ? '/index.html' : reqPath);
    const safePath = clean.replace(/^\/+/, '');
    const filePath = path.join(root, safePath || 'index.html');
    try {
      const data = await fs.readFile(filePath);
      res.writeHead(200, { 'Content-Type': contentTypeFor(filePath) });
      res.end(data);
    } catch {
      res.writeHead(404, { 'Content-Type': 'text/plain; charset=utf-8' });
      res.end('not found');
    }
  });

  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const address = server.address();
  if (!address || typeof address === 'string') throw new Error('Failed to bind test server');
  return { server, port: address.port };
}

test('only root index.html is served as app entrypoint', async ({ page }) => {
  const { server, port } = await createStaticServer();
  try {
    const rootResp = await page.request.get(`http://127.0.0.1:${port}/index.html`);
    expect(rootResp.status(), '/index.html should be served').toBe(200);

    const distResp = await page.request.get(`http://127.0.0.1:${port}/js/dist/index.html`);
    expect(distResp.status(), '/js/dist/index.html should not exist').toBe(404);

    await page.goto(`http://127.0.0.1:${port}/index.html`, { waitUntil: 'domcontentloaded' });
    const hasApp = await page.locator('#appRoot').count();
    expect(hasApp, 'root index should render app root').toBeGreaterThanOrEqual(1);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

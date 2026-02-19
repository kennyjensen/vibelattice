import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

async function withStaticServer(run) {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
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
    await run(`http://127.0.0.1:${port}`);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
}

test('debug panel is hidden by default and shown by debug flag on root entrypoint', async ({ page }) => {
  await withStaticServer(async (baseUrl) => {
    await page.goto(`${baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugPanel')).toBeHidden();

    await page.goto(`${baseUrl}/index.html?debug=1`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugPanel')).toBeVisible();

    await page.goto(`${baseUrl}/index.html`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugPanel')).toBeHidden();

    await page.goto(`${baseUrl}/index.html?debug=true`, { waitUntil: 'domcontentloaded' });
    await expect(page.locator('#debugPanel')).toBeVisible();
  });
});

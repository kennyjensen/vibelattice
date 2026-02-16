import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function parseElevatorDeflection(text) {
  const match = String(text || '').replace(/\u00a0/g, ' ').match(/elevator\s*=\s*([+-]?\d+(?:\.\d+)?)/i);
  return match ? Number(match[1]) : Number.NaN;
}

test('trimmed elevator horseshoe wake legs use the same downstream direction on left/right sides', async ({ page }) => {
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
      else if (filePath.endsWith('.run') || filePath.endsWith('.avl') || filePath.endsWith('.mass')) {
        res.setHeader('Content-Type', 'text/plain; charset=utf-8');
      }
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
    for (const entry of ['/index.html', '/js/dist/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => Boolean(window.__trefftzTestHook?.getVortexLegSymmetryStats));

      await page.click('#trimBtn');
      await page.waitForFunction(() => {
        const txt = String(document.getElementById('outDef')?.textContent || '');
        const stats = window.__trefftzTestHook?.getVortexLegSymmetryStats?.();
        return txt.includes('elevator =') && stats?.left?.n > 0 && stats?.right?.n > 0;
      }, null, { timeout: 30000 });

      const values = await page.evaluate(() => ({
        defText: String(document.getElementById('outDef')?.textContent || ''),
        stats: window.__trefftzTestHook?.getVortexLegSymmetryStats?.() || null,
      }));
      const elevator = parseElevatorDeflection(values.defText);

      expect(Number.isFinite(elevator)).toBeTruthy();
      expect(Math.abs(elevator)).toBeGreaterThan(0.1);
      expect(Number(values.stats?.left?.n || 0)).toBeGreaterThan(0);
      expect(Number(values.stats?.right?.n || 0)).toBeGreaterThan(0);
      expect(Number(values.stats?.angleDeg)).toBeLessThan(0.2);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

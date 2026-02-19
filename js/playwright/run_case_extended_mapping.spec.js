import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';

function parseNum(text) {
  const match = String(text || '').replace(/\u00a0/g, ' ').match(/[-+]?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/);
  return match ? Number(match[0]) : Number.NaN;
}

const runPayload = `
 ---------------------------------------------
 Run case  1:   Mapping Case

 alpha        ->  CL          =  0.420000
 beta         ->  beta        =   0.00000
 pb/2V        ->  pb/2V       =   0.00000
 qc/2V        ->  qc/2V       =   0.00000
 rb/2V        ->  rb/2V       =   0.00000

 alpha     =   1.25000     deg
 beta      =   0.00000     deg
 pb/2V     =   0.00000
 qc/2V     =   0.00000
 rb/2V     =   0.00000
 CL        =   0.420000
 CDo       =   0.012300
 bank      =   0.00000     deg
 Mach      =   0.23000
 velocity  =  52.5000
 density   =   1.18000
 grav.acc. =   9.81000
 turn_rad. =  55.0000
 load_fac. =   1.30000
 X_cg      =   0.11110
 Y_cg      =   0.22220
 Z_cg      =   0.33330
 mass      =   4.40000
 Ixx       =   5.50000
 Iyy       =   6.60000
 Izz       =   7.70000
 Ixy       =   0.01000
 Iyz       =   0.02000
 Izx       =   0.03000
`.trimStart();

test('AVL .run maps Mach, loop mode, and mass/inertia values into EXEC and UI', async ({ page }) => {
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
    await page.setViewportSize({ width: 1400, height: 900 });
    for (const entry of ['/index.html']) {
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect(page.locator('#debugLog')).toContainText('App ready', { timeout: 30000 });
      await page.waitForFunction(() => typeof window.__trefftzTestHook !== 'undefined');

      await page.setInputFiles('#runCasesInput', [{
        name: 'mapping.run',
        mimeType: 'text/plain',
        buffer: Buffer.from(runPayload, 'utf8'),
      }]);
      await expect(page.locator('#runCasesMeta')).toContainText('mapping.run');
      const runDebug = await page.evaluate(() => ({
        selected: window.__trefftzTestHook?.getSelectedRunCaseIndex?.(),
        runCases: window.__trefftzTestHook?.getRunCases?.(),
      }));
      expect(runDebug.selected).toBe(0);
      expect(Array.isArray(runDebug.runCases)).toBeTruthy();
      expect(runDebug.runCases.length).toBe(1);
      expect(Number(runDebug.runCases[0]?.inputs?.mach)).toBeCloseTo(0.23, 6);

      await expect(page.locator('#flightMode')).toHaveValue('looping');
      const radLoop = Number(await page.locator('#radLoop').inputValue());
      expect(Number.isFinite(radLoop)).toBeTruthy();
      expect(radLoop).toBeGreaterThan(0);

      await expect(page.locator('#massTotal')).toHaveValue('4.4000');
      await expect(page.locator('#massXcg')).toHaveValue('0.1111');
      await expect(page.locator('#massYcg')).toHaveValue('0.2222');
      await expect(page.locator('#massZcg')).toHaveValue('0.3333');
      await expect(page.locator('#massIxx')).toHaveValue('5.5000');
      await expect(page.locator('#massIyy')).toHaveValue('6.6000');
      await expect(page.locator('#massIzz')).toHaveValue('7.7000');
      await expect(page.locator('#massIxy')).toHaveValue('0.0100');
      await expect(page.locator('#massIyz')).toHaveValue('0.0200');
      await expect(page.locator('#massIxz')).toHaveValue('0.0300');

      await page.evaluate(() => { document.getElementById('trimBtn')?.click(); });
      await page.waitForFunction(() => {
        const t = String(document.getElementById('outMach')?.textContent || '').trim();
        const s = window.__trefftzTestHook?.getLastExecSummary?.();
        return t && t !== '-' && s && typeof s === 'object';
      });

      const execSummary = await page.evaluate(() => window.__trefftzTestHook?.getLastExecSummary?.() || null);
      expect(execSummary).toBeTruthy();
      expect(Number(execSummary.machPar)).toBeCloseTo(0.23, 6);

      const machText = await page.locator('#outMach').textContent();
      const mach = parseNum(machText);
      expect(Number.isFinite(mach)).toBeTruthy();
      expect(Math.abs(mach - 0.23)).toBeLessThan(0.01);
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
});

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html'];

for (const entry of entrypoints) {
  test(`hinge moments renders Chinge and Moment columns on ${entry}`, async ({ page }) => {
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
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });
      await expect.poll(async () => page.evaluate(() => (
        Boolean(window.__trefftzTestHook && typeof window.__trefftzTestHook.renderHingeForTest === 'function')
      ))).toBe(true);

      const report = await page.evaluate(() => {
        const idx2 = (i, j, dim1) => i + dim1 * j;
        const parval = new Array(120).fill(0);
        const IR = 1;
        const IPVEE = 12;
        const IPRHO = 13;
        parval[idx2(IPVEE, IR, 30)] = 10.0;
        parval[idx2(IPRHO, IR, 30)] = 1.2;

        const result = {
          PARVAL: parval,
          SREF: 12.0,
          CREF: 1.0,
          CHINGE: [0.0, 0.1, -0.2],
        };
        window.__trefftzTestHook?.renderHingeForTest?.(result);

        const toNum = (text) => {
          const t = String(text || '').replace(/\u00a0/g, ' ').replace(/−/g, '-').trim();
          if (!t || t === '-') return Number.NaN;
          return Number(t);
        };
        const near = (a, b, tol) => Number.isFinite(a) && Number.isFinite(b) && Math.abs(a - b) <= tol;
        const mismatches = [];

        const cells = Array.from(document.querySelectorAll('#outHinge .stability-cell')).map((el) => String(el.textContent || '').trim());
        if (cells.length < 9) return { mismatches: ['hinge grid did not render expected columns/rows'] };
        const gridColsRaw = String(getComputedStyle(document.querySelector('#outHinge')).gridTemplateColumns || '').trim();
        const gridColsCount = gridColsRaw ? gridColsRaw.split(/\s+/).length : 0;
        if (gridColsCount !== 3) mismatches.push(`expected 3 hinge grid columns, got ${gridColsCount} (${gridColsRaw})`);

        if (cells[1] !== 'Chinge') mismatches.push(`expected Chinge header, got: ${cells[1]}`);
        if (cells[2] !== 'Moment (N-m)') mismatches.push(`expected Moment (N-m) header, got: ${cells[2]}`);

        const qdyn = 0.5 * 1.2 * 10.0 * 10.0;
        const exp1 = 0.1 * qdyn * 12.0 * 1.0;
        const exp2 = -0.2 * qdyn * 12.0 * 1.0;
        const gotCh1 = toNum(cells[4]);
        const gotM1 = toNum(cells[5]);
        const gotCh2 = toNum(cells[7]);
        const gotM2 = toNum(cells[8]);
        if (!near(gotCh1, 0.1, 1e-6)) mismatches.push(`row1 chinge mismatch: ${gotCh1}`);
        if (!near(gotM1, exp1, 1e-6)) mismatches.push(`row1 moment mismatch: ${gotM1} vs ${exp1}`);
        if (!near(gotCh2, -0.2, 1e-6)) mismatches.push(`row2 chinge mismatch: ${gotCh2}`);
        if (!near(gotM2, exp2, 1e-6)) mismatches.push(`row2 moment mismatch: ${gotM2} vs ${exp2}`);

        return { mismatches };
      });

      expect(report.mismatches, report.mismatches.join('\n')).toEqual([]);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`plane run/mass all outputs parity on ${entry}`, async ({ page }) => {
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
        else if (filePath.endsWith('.run') || filePath.endsWith('.mass') || filePath.endsWith('.avl') || filePath.endsWith('.dat')) {
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
      await page.goto(`http://127.0.0.1:${port}${entry}`, { waitUntil: 'domcontentloaded' });

      await page.setInputFiles('#runCasesInput', path.join(root, 'third_party', 'avl', 'runs', 'plane.run'));
      await page.setInputFiles('#massPropsInput', path.join(root, 'third_party', 'avl', 'runs', 'plane.mass'));
      await expect(page.locator('#runCasesMeta')).toContainText('plane.run');
      await expect(page.locator('#massPropsMeta')).toContainText('plane.mass');

      const avlText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'plane.avl'), 'utf8');
      await page.fill('#fileText', avlText);
      await page.dispatchEvent('#fileText', 'input');

      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      await expect.poll(async () => page.evaluate(() => {
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        const domReady = document.querySelectorAll('#outBodyDeriv .stability-cell[title] .stability-num').length > 0
          && document.querySelectorAll('#outForcesSurface .stability-cell .stability-num').length > 0
          && document.querySelectorAll('#outStability .stability-cell[title] .stability-num').length > 0;
        return Boolean(
          r?.CFTOT && r?.CMTOT && r?.CLTOT_U && r?.CYTOT_U && r?.CFTOT_U
          && r?.CMSURF && r?.CLSURF && r?.CDSURF
        ) && domReady;
      }), { timeout: 30000 }).toBe(true);

      const report = await page.evaluate(() => {
        const toNum = (text) => {
          const t = String(text || '').replace(/\u00a0/g, ' ').replace(/−/g, '-').trim();
          if (t === '-' || t === '') return 0;
          return Number(t);
        };
        const near = (a, b, tol) => Number.isFinite(a) && Number.isFinite(b) && Math.abs(a - b) <= tol;
        const mismatches = [];
        const result = window.__trefftzTestHook?.getLastExecResult?.();
        if (!result) return { mismatches: ['missing exec result'] };

        const dir = (typeof result.LNASA_SA === 'boolean') ? (result.LNASA_SA ? -1.0 : 1.0) : -1.0;
        const ca = Math.cos(Number(result.ALFA || 0));
        const sa = Math.sin(Number(result.ALFA || 0));
        const w0 = Number(result.WROT?.[0] || 0);
        const w2 = Number(result.WROT?.[2] || 0);
        const RX = (w0 * ca + w2 * sa) * dir;
        const RZ = (w2 * ca - w0 * sa) * dir;
        const WROT_RX = [ca * dir, 0.0, sa * dir];
        const WROT_RZ = [-sa * dir, 0.0, ca * dir];
        const WROT_A = [-RX * sa - RZ * ca, 0.0, -RZ * sa + RX * ca];

        const vinfA = [Number(result.VINF_A?.[0] || 0), Number(result.VINF_A?.[1] || 0), Number(result.VINF_A?.[2] || 0)];
        const vinfB = [Number(result.VINF_B?.[0] || 0), Number(result.VINF_B?.[1] || 0), Number(result.VINF_B?.[2] || 0)];
        const dot3 = (a, b) => Number(a[0] || 0) * b[0] + Number(a[1] || 0) * b[1] + Number(a[2] || 0) * b[2];
        const dA = (arr, add = 0) => (
          Number(arr[0] || 0) * vinfA[0] + Number(arr[1] || 0) * vinfA[1] + Number(arr[2] || 0) * vinfA[2]
          + Number(arr[3] || 0) * WROT_A[0] + Number(arr[4] || 0) * WROT_A[1] + Number(arr[5] || 0) * WROT_A[2]
          + add
        );
        const dB = (arr) => dot3(arr, vinfB);
        const dRX = (arr) => Number(arr[3] || 0) * WROT_RX[0] + Number(arr[5] || 0) * WROT_RX[2];
        const dRY = (arr) => Number(arr[4] || 0);
        const dRZ = (arr) => Number(arr[5] || 0) * WROT_RZ[2] + Number(arr[3] || 0) * WROT_RZ[0];

        const bref = Number(result.BREF || 1);
        const cref = Number(result.CREF || 1);
        const pScale = Math.abs(bref) > 1e-12 ? (2 / bref) : 0;
        const qScale = Math.abs(cref) > 1e-12 ? (2 / cref) : 0;
        const rScale = pScale;

        const stableActual = {};
        document.querySelectorAll('#outStability .stability-cell[title] .stability-num').forEach((n) => {
          const title = n.closest('.stability-cell')?.getAttribute('title');
          stableActual[title] = toNum(n.textContent);
        });

        const clU = result.CLTOT_U || [];
        const cyU = result.CYTOT_U || [];
        const cmU = result.CMTOT_U || [[], [], []];
        const clxU = cmU[0] || [];
        const cmyU = cmU[1] || [];
        const cnzU = cmU[2] || [];
        const crsaxU = Array.from({ length: 6 }, (_, i) => Number(clxU[i] || 0) * ca + Number(cnzU[i] || 0) * sa);
        const cmsaxU = Array.from({ length: 6 }, (_, i) => Number(cmyU[i] || 0));
        const cnsaxU = Array.from({ length: 6 }, (_, i) => Number(cnzU[i] || 0) * ca - Number(clxU[i] || 0) * sa);
        const crsaxA = -Number(result.CMTOT?.[0] || 0) * sa + Number(result.CMTOT?.[2] || 0) * ca;
        const cnsaxA = -Number(result.CMTOT?.[2] || 0) * sa - Number(result.CMTOT?.[0] || 0) * ca;

        const stableExpected = {
          CLa: dA(clU, Number(result.CLTOT_A || 0)),
          CYa: dA(cyU),
          Cla: dir * dA(crsaxU, crsaxA),
          Cma: dA(cmsaxU),
          Cna: dir * dA(cnsaxU, cnsaxA),
          CLb: dB(clU),
          CYb: dB(cyU),
          Clb: dir * dB(crsaxU),
          Cmb: dB(cmsaxU),
          Cnb: dir * dB(cnsaxU),
          CLp: dRX(clU) * pScale,
          CYp: dRX(cyU) * pScale,
          Clp: dir * dRX(crsaxU) * pScale,
          Cmp: dRX(cmsaxU) * pScale,
          Cnp: dir * dRX(cnsaxU) * pScale,
          CLq: dRY(clU) * qScale,
          CYq: dRY(cyU) * qScale,
          Clq: dir * dRY(crsaxU) * qScale,
          Cmq: dRY(cmsaxU) * qScale,
          Cnq: dir * dRY(cnsaxU) * qScale,
          CLr: dRZ(clU) * rScale,
          CYr: dRZ(cyU) * rScale,
          Clr: dir * dRZ(crsaxU) * rScale,
          Cmr: dRZ(cmsaxU) * rScale,
          Cnr: dir * dRZ(cnsaxU) * rScale,
        };
        for (const [k, v] of Object.entries(stableExpected)) {
          if (!near(stableActual[k], v, 1.5e-6)) mismatches.push(`stability ${k}: actual=${stableActual[k]} expected=${v}`);
        }

        const bodyActual = {};
        document.querySelectorAll('#outBodyDeriv .stability-cell[title] .stability-num').forEach((n) => {
          const title = n.closest('.stability-cell')?.getAttribute('title');
          bodyActual[title] = toNum(n.textContent);
        });
        const cfU = result.CFTOT_U || [[], [], []];
        const bodyExpected = {
          CXu: -Number(cfU[0]?.[0] || 0),
          CYu: -(dir * Number(cfU[1]?.[0] || 0)),
          CZu: -Number(cfU[2]?.[0] || 0),
          Clu: -Number(cmU[0]?.[0] || 0),
          Cmu: -(dir * Number(cmU[1]?.[0] || 0)),
          Cnu: -Number(cmU[2]?.[0] || 0),
          CXv: -(dir * Number(cfU[0]?.[1] || 0)),
          CYv: -Number(cfU[1]?.[1] || 0),
          CZv: -(dir * Number(cfU[2]?.[1] || 0)),
          Clv: -(dir * Number(cmU[0]?.[1] || 0)),
          Cmv: -Number(cmU[1]?.[1] || 0),
          Cnv: -(dir * Number(cmU[2]?.[1] || 0)),
          CXw: -Number(cfU[0]?.[2] || 0),
          CYw: -(dir * Number(cfU[1]?.[2] || 0)),
          CZw: -Number(cfU[2]?.[2] || 0),
          Clw: -Number(cmU[0]?.[2] || 0),
          Cmw: -(dir * Number(cmU[1]?.[2] || 0)),
          Cnw: -Number(cmU[2]?.[2] || 0),
          CXp: Number(cfU[0]?.[3] || 0) * pScale,
          CYp: dir * Number(cfU[1]?.[3] || 0) * pScale,
          CZp: Number(cfU[2]?.[3] || 0) * pScale,
          Clp: Number(cmU[0]?.[3] || 0) * pScale,
          Cmp: dir * Number(cmU[1]?.[3] || 0) * pScale,
          Cnp: Number(cmU[2]?.[3] || 0) * pScale,
          CXq: dir * Number(cfU[0]?.[4] || 0) * qScale,
          CYq: Number(cfU[1]?.[4] || 0) * qScale,
          CZq: dir * Number(cfU[2]?.[4] || 0) * qScale,
          Clq: dir * Number(cmU[0]?.[4] || 0) * qScale,
          Cmq: Number(cmU[1]?.[4] || 0) * qScale,
          Cnq: dir * Number(cmU[2]?.[4] || 0) * qScale,
          CXr: Number(cfU[0]?.[5] || 0) * rScale,
          CYr: dir * Number(cfU[1]?.[5] || 0) * rScale,
          CZr: Number(cfU[2]?.[5] || 0) * rScale,
          Clr: Number(cmU[0]?.[5] || 0) * rScale,
          Cmr: dir * Number(cmU[1]?.[5] || 0) * rScale,
          Cnr: Number(cmU[2]?.[5] || 0) * rScale,
        };
        for (const [k, v] of Object.entries(bodyExpected)) {
          if (!near(bodyActual[k], v, 1.5e-6)) mismatches.push(`body ${k}: actual=${bodyActual[k]} expected=${v}`);
        }

        const surfCells = Array.from(document.querySelectorAll('#outForcesSurface .stability-cell')).map((el) => toNum(el.textContent));
        const surfRows = Number(result.CLSURF?.length || 0) - 1;
        for (let i = 1; i <= surfRows; i += 1) {
          const base = i * 7;
          const exp = [
            Number(result.CLSURF?.[i] || 0),
            Number(result.CDSURF?.[i] || 0),
            Number(result.CYSURF?.[i] || 0),
            dir * Number(result.CMSURF?.[i]?.[0] || 0),
            Number(result.CMSURF?.[i]?.[1] || 0),
            dir * Number(result.CMSURF?.[i]?.[2] || 0),
          ];
          const got = [surfCells[base + 1], surfCells[base + 2], surfCells[base + 3], surfCells[base + 4], surfCells[base + 5], surfCells[base + 6]];
          ['CL', 'CD', 'CY', 'Cl', 'Cm', 'Cn'].forEach((name, k) => {
            if (!near(got[k], exp[k], 5e-4)) mismatches.push(`surface row ${i} ${name}: actual=${got[k]} expected=${exp[k]}`);
          });
        }

        return { mismatches };
      });

      expect(report.mismatches, report.mismatches.slice(0, 20).join('\n')).toEqual([]);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

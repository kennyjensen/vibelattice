import { test, expect } from '@playwright/test';
import path from 'node:path';
import fs from 'node:fs/promises';
import http from 'node:http';
import { fileURLToPath } from 'node:url';

const entrypoints = ['/index.html', '/js/dist/index.html'];

for (const entry of entrypoints) {
  test(`hershey all outputs parity on ${entry}`, async ({ page }) => {
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

      const avlText = await fs.readFile(path.join(root, 'third_party', 'avl', 'runs', 'hershey.avl'), 'utf8');
      await page.fill('#fileText', avlText);
      await page.dispatchEvent('#fileText', 'input');

      await page.fill('#mass', '1.0');
      await page.fill('#gee', '1.0');
      await page.fill('#rho', '1.0');
      await page.fill('#vel', '1.0');
      await page.fill('#cl', '0.0');

      await page.selectOption('.constraint-row[data-var="alpha"] .constraint-select', 'alpha');
      await page.selectOption('.constraint-row[data-var="beta"] .constraint-select', 'beta');
      await page.selectOption('.constraint-row[data-var="p"] .constraint-select', 'p');
      await page.selectOption('.constraint-row[data-var="q"] .constraint-select', 'q');
      await page.selectOption('.constraint-row[data-var="r"] .constraint-select', 'r');
      await page.fill('.constraint-row[data-var="alpha"] .constraint-value', '1.0');
      await page.fill('.constraint-row[data-var="beta"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="p"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="q"] .constraint-value', '0.0');
      await page.fill('.constraint-row[data-var="r"] .constraint-value', '0.0');

      await page.check('#useWasmExec');
      await page.click('#trimBtn');

      await expect.poll(async () => page.evaluate(() => {
        const r = window.__trefftzTestHook?.getLastExecResult?.();
        const ready = Boolean(
          r?.CFTOT && r?.CMTOT && r?.CLTOT_U && r?.CYTOT_U && r?.CFTOT_U
          && r?.CMSURF && r?.CLSURF && r?.CDSURF && r?.CHINGE
        );
        const domReady = document.querySelectorAll('#outStability .stability-num').length > 0
          && document.querySelectorAll('#outBodyDeriv .stability-num').length > 0
          && document.querySelectorAll('#outForcesSurface .stability-num').length > 0;
        return ready && domReady;
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
        const idx2 = (i, j, dim1) => i + dim1 * j;

        const checkId = (sel, expected, tol, label) => {
          const actual = toNum(document.querySelector(sel)?.textContent);
          if (!near(actual, expected, tol)) mismatches.push(`${label}: actual=${actual} expected=${expected} tol=${tol}`);
        };

        checkId('#outAlpha', Number(result.ALFA || 0) * (180 / Math.PI), 1e-3, 'outAlpha');
        checkId('#outBeta', Number(result.BETA || 0) * (180 / Math.PI), 1e-3, 'outBeta');
        checkId('#outMach', Number(result.PARVAL?.[idx2(11, 1, 30)] ?? 0), 1e-3, 'outMach');
        checkId('#outPb2v', dir * Number(result.WROT?.[0] || 0), 1e-3, 'outPb2v');
        checkId('#outQc2v', Number(result.WROT?.[1] || 0), 1e-3, 'outQc2v');
        checkId('#outRb2v', dir * Number(result.WROT?.[2] || 0), 1e-3, 'outRb2v');
        checkId('#outCD', Number(result.CDTOT || 0), 1e-4, 'outCD');
        checkId('#outCYtot', Number(result.CFTOT?.[1] || 0), 1e-5, 'outCYtot');
        checkId('#outCltot', dir * Number(result.CMTOT?.[0] || 0), 1e-5, 'outCltot');
        checkId('#outCmtot', Number(result.CMTOT?.[1] || 0), 1e-4, 'outCmtot');
        checkId('#outCntot', dir * Number(result.CMTOT?.[2] || 0), 1e-5, 'outCntot');
        checkId('#outCDvis', Number(result.CDVTOT || 0), 1e-4, 'outCDvis');
        checkId('#outCDind', Number(result.CDTOT || 0) - Number(result.CDVTOT || 0), 1e-4, 'outCDind');
        if (Number.isFinite(result.SPANEF)) checkId('#outEff', Number(result.SPANEF), 1e-4, 'outEff');

        const collectGrid = (rootSel) => {
          const out = {};
          document.querySelectorAll(`${rootSel} .stability-cell[title] .stability-num`).forEach((n) => {
            const title = n.closest('.stability-cell')?.getAttribute('title');
            out[title] = toNum(n.textContent);
          });
          return out;
        };

        const vinfA = [Number(result.VINF_A?.[0] || 0), Number(result.VINF_A?.[1] || 0), Number(result.VINF_A?.[2] || 0)];
        const vinfB = [Number(result.VINF_B?.[0] || 0), Number(result.VINF_B?.[1] || 0), Number(result.VINF_B?.[2] || 0)];
        const dot3 = (a, b) => Number(a[0] || 0) * b[0] + Number(a[1] || 0) * b[1] + Number(a[2] || 0) * b[2];

        const clU = result.CLTOT_U || [];
        const cyU = result.CYTOT_U || [];
        const cmU = result.CMTOT_U || [[], [], []];
        const clD = result.CLTOT_D || [];
        const cyD = result.CYTOT_D || [];
        const cmD = result.CMTOT_D || [[], [], []];

        const bref = Number(result.BREF || 1);
        const cref = Number(result.CREF || 1);
        const pScale = Math.abs(bref) > 1e-12 ? (2 / bref) : 0;
        const qScale = Math.abs(cref) > 1e-12 ? (2 / cref) : 0;
        const rScale = pScale;

        const dA = (arr, add = 0) => dot3(arr, vinfA) + add;
        const dB = (arr) => dot3(arr, vinfB);
        const stableActual = collectGrid('#outStability');
        const bodyActual = collectGrid('#outBodyDeriv');
        const actualDerivKeys = [...Object.keys(stableActual), ...Object.keys(bodyActual)];
        const maxD = actualDerivKeys.reduce((m, key) => {
          const hit = /^C[LYlmn][d](\d+)$/.exec(key) || /^C[XYZlmn][d](\d+)$/.exec(key);
          if (!hit) return m;
          return Math.max(m, Number(hit[1] || 0));
        }, 0);

        const stableExpected = {
          CLa: dA(clU, Number(result.CLTOT_A || 0)),
          CYa: dA(cyU),
          Cla: dA(cmU[0] || []),
          Cma: dA(cmU[1] || []),
          Cna: dA(cmU[2] || []),
          CLb: dB(clU),
          CYb: dB(cyU),
          Clb: dB(cmU[0] || []),
          Cmb: dB(cmU[1] || []),
          Cnb: dB(cmU[2] || []),
          CLp: Number(clU[3] || 0) * pScale,
          CYp: Number(cyU[3] || 0) * pScale,
          Clp: Number(cmU[0]?.[3] || 0) * pScale,
          Cmp: Number(cmU[1]?.[3] || 0) * pScale,
          Cnp: Number(cmU[2]?.[3] || 0) * pScale,
          CLq: Number(clU[4] || 0) * qScale,
          CYq: Number(cyU[4] || 0) * qScale,
          Clq: Number(cmU[0]?.[4] || 0) * qScale,
          Cmq: Number(cmU[1]?.[4] || 0) * qScale,
          Cnq: Number(cmU[2]?.[4] || 0) * qScale,
          CLr: Number(clU[5] || 0) * rScale,
          CYr: Number(cyU[5] || 0) * rScale,
          Clr: Number(cmU[0]?.[5] || 0) * rScale,
          Cmr: Number(cmU[1]?.[5] || 0) * rScale,
          Cnr: Number(cmU[2]?.[5] || 0) * rScale,
        };
        for (let i = 0; i < maxD; i += 1) {
          const key = `d${i + 1}`;
          stableExpected[`CL${key}`] = Number(clD[i] || 0);
          stableExpected[`CY${key}`] = Number(cyD[i] || 0);
          stableExpected[`Cl${key}`] = Number(cmD[0]?.[i] || 0);
          stableExpected[`Cm${key}`] = Number(cmD[1]?.[i] || 0);
          stableExpected[`Cn${key}`] = Number(cmD[2]?.[i] || 0);
        }
        for (const [k, v] of Object.entries(stableExpected)) {
          if (!near(stableActual[k], v, 1.5e-6)) mismatches.push(`stability ${k}: actual=${stableActual[k]} expected=${v}`);
        }

        const cfU = result.CFTOT_U || [[], [], []];
        const cfD = result.CFTOT_D || [[], [], []];
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
        for (let i = 0; i < maxD; i += 1) {
          const key = `d${i + 1}`;
          bodyExpected[`CX${key}`] = dir * Number(cfD[0]?.[i] || 0);
          bodyExpected[`CY${key}`] = Number(cfD[1]?.[i] || 0);
          bodyExpected[`CZ${key}`] = dir * Number(cfD[2]?.[i] || 0);
          bodyExpected[`Cl${key}`] = dir * Number(cmD[0]?.[i] || 0);
          bodyExpected[`Cm${key}`] = Number(cmD[1]?.[i] || 0);
          bodyExpected[`Cn${key}`] = dir * Number(cmD[2]?.[i] || 0);
        }
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

        const hingeRows = Array.from(document.querySelectorAll('#outHinge .stability-cell'));
        if (Array.isArray(result.CHINGE)) {
          for (let i = 1; i < result.CHINGE.length; i += 1) {
            const idx = i * 2 + 1;
            const got = toNum(hingeRows[idx]?.textContent);
            const exp = Number(result.CHINGE[i] || 0);
            if (!near(got, exp, 1.5e-6)) mismatches.push(`hinge d${i}: actual=${got} expected=${exp}`);
          }
        }

        return { mismatches };
      });

      expect(report.mismatches, report.mismatches.slice(0, 20).join('\n')).toEqual([]);
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
  });
}

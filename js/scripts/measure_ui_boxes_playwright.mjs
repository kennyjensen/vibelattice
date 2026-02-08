import path from 'node:path';
import { chromium } from 'playwright';

const chromePath = process.env.PLAYWRIGHT_CHROME_PATH || '/usr/bin/google-chrome';
const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '../..');
const siblingRoot = path.resolve(repoRoot, '..');

const vibefoilUrl = `file://${path.resolve(siblingRoot, 'vibefoil/index.html')}`;
const vibelatticeUrl = `file://${path.resolve(repoRoot, 'index.html')}`;

const browser = await chromium.launch({
  headless: true,
  executablePath: chromePath,
  args: ['--no-sandbox'],
});

const context = await browser.newContext({ viewport: { width: 1400, height: 1000 } });

async function measure(url, evaluateFn) {
  const page = await context.newPage();
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForTimeout(600);
  const data = await page.evaluate(evaluateFn);
  await page.close();
  return data;
}

const vibefoil = await measure(vibefoilUrl, () => {
  const b = (el) => el.getBoundingClientRect();
  const row = (labelFor, inputId) => {
    const label = document.querySelector(`label[for="${labelFor}"]`);
    const input = document.getElementById(inputId);
    if (!label || !input) return null;
    const lb = b(label);
    const ib = b(input);
    return {
      labelLeft: Number(lb.left.toFixed(2)),
      labelRight: Number(lb.right.toFixed(2)),
      inputLeft: Number(ib.left.toFixed(2)),
      inputWidth: Number(ib.width.toFixed(2)),
      gapFromLabelRight: Number((ib.left - lb.right).toFixed(2)),
    };
  };
  return {
    mach: row('mach', 'mach'),
    reynolds: row('reynolds', 'reynolds'),
  };
});

const vibelattice = await measure(vibelatticeUrl, () => {
  const ids = ['gee', 'rho', 'mass', 'vel', 'cl', 'flightMode', 'bank', 'radLoop', 'facLoop'];
  const out = {};
  const b = (el) => el.getBoundingClientRect();
  for (const id of ids) {
    const input = document.getElementById(id);
    if (!input) continue;
    const label = input.closest('label');
    if (!label) continue;
    const labelText = label.querySelector('.label-text') || label;
    const lb = b(label);
    const tb = b(labelText);
    const ib = b(input);
    out[id] = {
      labelLeft: Number(lb.left.toFixed(2)),
      labelTextRight: Number(tb.right.toFixed(2)),
      inputLeft: Number(ib.left.toFixed(2)),
      inputWidth: Number(ib.width.toFixed(2)),
      gapFromLabelText: Number((ib.left - tb.right).toFixed(2)),
    };
  }
  return out;
});

console.log(JSON.stringify({ vibefoil, vibelattice }, null, 2));
await browser.close();

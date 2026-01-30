import { parentPort, workerData, isMainThread } from 'node:worker_threads';
import { runExec } from '../src/exec_pipeline.js';

const { text, options } = workerData || {};

async function main() {
  if (isMainThread || !parentPort) {
    return;
  }
  try {
    const { state } = await runExec(text, options);
    let cncNan = 0;
    let dwNan = 0;
    let cltNan = 0;
    for (let j = 1; j <= state.NSTRIP; j += 1) {
      if (!Number.isFinite(state.CNC[j])) cncNan += 1;
      if (!Number.isFinite(state.DWWAKE[j])) dwNan += 1;
      if (!Number.isFinite(state.CLT_LSTRP[j])) cltNan += 1;
    }
    parentPort.postMessage({
      ok: true,
      CLTOT: state.CLTOT,
      NVOR: state.NVOR,
      NSTRIP: state.NSTRIP,
      cncNan,
      dwNan,
      cltNan,
    });
  } catch (err) {
    parentPort.postMessage({
      ok: false,
      error: err?.message ?? String(err),
    });
  }
}

main();

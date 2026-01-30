import { EXEC } from './aoper.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function log(message) {
  postMessage({ type: 'log', message });
}

onmessage = (evt) => {
  const { state } = evt.data || {};
  if (!state) {
    postMessage({ type: 'error', message: 'Missing state.' });
    return;
  }
  try {
    const t0 = Date.now();
    log('Worker EXEC start');
    EXEC(state, 8, 0, 1);
    const dt = Date.now() - t0;
    log(`Worker EXEC done (${dt} ms)`);
    const trefftz = {
      axis: 'Y',
      cref: state.CREF,
      strips: [],
      surfaces: [],
    };
    if (state.DWWAKE && state.RLE && state.CNC && state.CLA_LSTRP && state.CLT_LSTRP && state.JFRST && state.NJ) {
      for (let n = 1; n <= state.NSURF; n += 1) {
        const j1 = state.JFRST[n];
        const nj = state.NJ[n];
        if (!j1 || !nj) continue;
        const start = trefftz.strips.length;
        for (let jj = 0; jj < nj; jj += 1) {
          const j = j1 + jj;
          const y = state.RLE[idx2(2, j, 4)];
          const z = state.RLE[idx2(3, j, 4)];
          const cnc = state.CNC[j] ?? 0.0;
          const cl = state.CLA_LSTRP[j] ?? 0.0;
          const clPerp = state.CLT_LSTRP[j] ?? 0.0;
          const dw = state.DWWAKE[j] ?? 0.0;
          trefftz.strips.push([y, z, cnc, cl, clPerp, dw, n]);
        }
        trefftz.surfaces.push({ id: n, start, count: nj });
      }
    }
    const surfVec = (arr, is) => (arr ? [
      arr[idx2(0, is, 3)],
      arr[idx2(1, is, 3)],
      arr[idx2(2, is, 3)],
    ] : null);
    const rowU = (arr, row) => {
      if (!arr) return null;
      const out = [];
      for (let n = 0; n < 6; n += 1) out.push(arr[idx2(row, n, 3)]);
      return out;
    };
    const hinge = state.CHINGE ? (() => {
      const out = new Array(state.NCONTROL + 1).fill(0);
      for (let i = 1; i <= state.NCONTROL; i += 1) out[i] = state.CHINGE[i - 1] ?? 0.0;
      return out;
    })() : null;
    postMessage({
      type: 'result',
      ALFA: state.ALFA,
      BETA: state.BETA,
      CLTOT: state.CLTOT,
      CDTOT: state.CDTOT,
      CYTOT: state.CYTOT,
      CDVTOT: state.CDVTOT,
      CFTOT: state.CFTOT ? Array.from(state.CFTOT) : null,
      CMTOT: state.CMTOT ? Array.from(state.CMTOT) : null,
      CFTOT_U: state.CFTOT_U ? [rowU(state.CFTOT_U, 0), rowU(state.CFTOT_U, 1), rowU(state.CFTOT_U, 2)] : null,
      CMTOT_U: state.CMTOT_U ? [rowU(state.CMTOT_U, 0), rowU(state.CMTOT_U, 1), rowU(state.CMTOT_U, 2)] : null,
      CLTOT_U: state.CLTOT_U ? Array.from(state.CLTOT_U.slice(0, 6)) : null,
      CDTOT_U: state.CDTOT_U ? Array.from(state.CDTOT_U.slice(0, 6)) : null,
      CYTOT_U: state.CYTOT_U ? Array.from(state.CYTOT_U.slice(0, 6)) : null,
      CLTOT_D: state.CLTOT_D ? Array.from(state.CLTOT_D) : null,
      CDTOT_D: state.CDTOT_D ? Array.from(state.CDTOT_D) : null,
      CYTOT_D: state.CYTOT_D ? Array.from(state.CYTOT_D) : null,
      CLTOT_A: state.CLTOT_A ?? 0.0,
      CDTOT_A: state.CDTOT_A ?? 0.0,
      VINF_A: state.VINF_A ? Array.from(state.VINF_A) : null,
      VINF_B: state.VINF_B ? Array.from(state.VINF_B) : null,
      CDSURF: state.CDSURF ? Array.from(state.CDSURF) : null,
      CYSURF: state.CYSURF ? Array.from(state.CYSURF) : null,
      CLSURF: state.CLSURF ? Array.from(state.CLSURF) : null,
      CDVSURF: state.CDVSURF ? Array.from(state.CDVSURF) : null,
      CFSURF: state.CFSURF ? Array.from({ length: state.NSURF + 1 }, (_, is) => (is === 0 ? null : surfVec(state.CFSURF, is))) : null,
      CMSURF: state.CMSURF ? Array.from({ length: state.NSURF + 1 }, (_, is) => (is === 0 ? null : surfVec(state.CMSURF, is))) : null,
      CDSTRP: state.CDSTRP ? Array.from(state.CDSTRP) : null,
      CYSTRP: state.CYSTRP ? Array.from(state.CYSTRP) : null,
      CLSTRP: state.CLSTRP ? Array.from(state.CLSTRP) : null,
      CNC: state.CNC ? Array.from(state.CNC) : null,
      CLA_LSTRP: state.CLA_LSTRP ? Array.from(state.CLA_LSTRP) : null,
      CLT_LSTRP: state.CLT_LSTRP ? Array.from(state.CLT_LSTRP) : null,
      DWWAKE: state.DWWAKE ? Array.from(state.DWWAKE) : null,
      RLE: state.RLE ? Array.from(state.RLE) : null,
      DCP: state.DCP ? Array.from(state.DCP) : null,
      CDBDY: state.CDBDY ? Array.from(state.CDBDY) : null,
      CYBDY: state.CYBDY ? Array.from(state.CYBDY) : null,
      CLBDY: state.CLBDY ? Array.from(state.CLBDY) : null,
      CFBDY: state.CFBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CFBDY, ib)) : null,
      CMBDY: state.CMBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CMBDY, ib)) : null,
      CHINGE: hinge,
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
      TREFFTZ: trefftz,
    });
  } catch (err) {
    postMessage({ type: 'error', message: err?.message ?? String(err) });
  }
};

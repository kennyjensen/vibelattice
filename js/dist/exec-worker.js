import { EXEC, preloadAoperLinSolveWasm, preloadAsetupWasm, preloadAeroWasm, preloadAicWasmBridge, preloadAsetpLuWasmBridge } from './aoper.js';
import { TRMSET_CORE } from './atrim.js';
import { SYSMAT, EIGSOL } from '../src/amode.js';
import { APPGET } from '../src/amass.js';

let execWasm = null;
let execWasmReady = null;
let execWasmState = null;
let amodeWasm = null;
let amodeWasmReady = null;
let amodeWasmState = null;
let amodeWasmLast = null;

async function loadExecWasm() {
  if (execWasm) return execWasm;
  if (execWasmReady) return execWasmReady;
  execWasmReady = (async () => {
    const url = new URL('./aoper.wasm', import.meta.url);
    const bytes = await (await fetch(url)).arrayBuffer();
    const imports = {
      env: {
        exec_js: (niter, info, ir) => {
          if (!execWasmState) {
            throw new Error('EXEC wasm called without bound state');
          }
          EXEC(execWasmState, niter, info, ir);
        },
      },
    };
    const { instance } = await WebAssembly.instantiate(bytes, imports);
    execWasm = instance.exports.EXEC;
    return execWasm;
  })();
  return execWasmReady;
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function ensureAmodeState(state) {
  state.JEMAX = state.JEMAX || 12;
  state.JEU = state.JEU || 1;
  state.JEW = state.JEW || 2;
  state.JEQ = state.JEQ || 3;
  state.JETH = state.JETH || 4;
  state.JEV = state.JEV || 5;
  state.JEP = state.JEP || 6;
  state.JER = state.JER || 7;
  state.JEPH = state.JEPH || 8;
  state.JEX = state.JEX || 9;
  state.JEY = state.JEY || 10;
  state.JEZ = state.JEZ || 11;
  state.JEPS = state.JEPS || 12;
  if (!state.__amode) {
    const jemax = state.JEMAX + 1;
    const ndmax = (state.NDMAX || Math.max(1, state.NCONTROL || 1)) + 1;
    state.__amode = {
      ASYS: new Float32Array(jemax * jemax),
      BSYS: new Float32Array(jemax * ndmax),
      RSYS: new Float32Array(jemax),
    };
  }
}

async function loadAmodeWasm() {
  if (amodeWasm) return amodeWasm;
  if (amodeWasmReady) return amodeWasmReady;
  amodeWasmReady = (async () => {
    const url = new URL('./amode.wasm', import.meta.url);
    const bytes = await (await fetch(url)).arrayBuffer();
    const imports = {
      env: {
        runchk_js: () => {},
        sysmat_js: (ir) => {
          const { ASYS, BSYS, RSYS } = amodeWasmState.__amode;
          amodeWasmLast = SYSMAT(amodeWasmState, ir, ASYS, BSYS, RSYS);
        },
        appmat_js: () => {},
        syssho_js: () => {},
        eigsol_js: (ir, etol, nsys) => {
          const { ASYS } = amodeWasmState.__amode;
          amodeWasmLast = EIGSOL(amodeWasmState, ir, etol, ASYS, nsys);
        },
      },
    };
    const { instance } = await WebAssembly.instantiate(bytes, imports);
    amodeWasm = instance.exports;
    return amodeWasm;
  })();
  return amodeWasmReady;
}

function normalizeEigenVec(vr, vi, nsys) {
  let mag = 0;
  for (let i = 0; i < nsys; i += 1) {
    mag = Math.max(mag, Math.hypot(vr[i] || 0, vi[i] || 0));
  }
  const s = mag > 1e-8 ? 1 / mag : 1;
  return {
    re: Array.from({ length: nsys }, (_, i) => (vr[i] || 0) * s),
    im: Array.from({ length: nsys }, (_, i) => (vi[i] || 0) * s),
  };
}

async function computeEigenmodes(state, useWasm) {
  ensureAmodeState(state);
  const IR = 1;
  const ETOL = 1e-5;
  if (state.LMASS) {
    try {
      APPGET(state);
    } catch (err) {
      log(`APPGET failed: ${err?.message ?? err}`);
    }
  }
  let sysRes = null;
  let eigRes = null;
  if (useWasm) {
    const wasm = await loadAmodeWasm();
    amodeWasmState = state;
    wasm.SYSMAT(IR);
    sysRes = amodeWasmLast;
    const nsys = sysRes?.NSYS || 0;
    wasm.EIGSOL(IR, ETOL, nsys);
    eigRes = amodeWasmLast;
  } else {
    const { ASYS, BSYS, RSYS } = state.__amode;
    sysRes = SYSMAT(state, IR, ASYS, BSYS, RSYS);
    eigRes = EIGSOL(state, IR, ETOL, ASYS, sysRes?.NSYS || 0);
  }
  const nsys = sysRes?.NSYS || 0;
  const evals = eigRes?.EVAL || [];
  const evecs = eigRes?.EVEC || [];
  const names = ['u', 'w', 'q', 'theta', 'v', 'p', 'r', 'phi', 'x', 'y', 'z', 'psi'];
  const modes = evals.map((ev, idx) => {
    const vec = evecs[idx] ? normalizeEigenVec(evecs[idx].re, evecs[idx].im, nsys) : { re: [], im: [] };
    const v = vec.re;
    const roll = (v[5] || 0) + 0.8 * (v[7] || 0);
    const pitch = (v[2] || 0) + 0.8 * (v[3] || 0);
    const yaw = (v[6] || 0) + 0.8 * (v[11] || 0);
    const tx = (v[0] || 0) + 0.6 * (v[8] || 0);
    const ty = (v[4] || 0) + 0.6 * (v[9] || 0);
    const tz = (v[1] || 0) + 0.6 * (v[10] || 0);
    return {
      name: `Mode ${idx + 1}`,
      re: ev.re,
      im: ev.im,
      stateOrder: names.slice(0, nsys),
      eigenvector: vec,
      vec: {
        rx: roll,
        ry: pitch,
        rz: yaw,
        tx,
        ty,
        tz,
      },
    };
  });
  return { nsys, modes };
}

function log(message) {
  postMessage({ type: 'log', message });
}

onmessage = async (evt) => {
  const { state, type, requestId, useWasm } = evt.data || {};
  if (!state) {
    postMessage({ type: 'error', message: 'Missing state.' });
    return;
  }
  if (type === 'trim') {
    try {
      const IR = 1;
      TRMSET_CORE(state, 1, IR, IR, IR);
      postMessage({ type: 'trimResult', state, requestId });
    } catch (err) {
      postMessage({ type: 'error', message: err?.message ?? String(err) });
    }
    return;
  }
  try {
    const t0 = Date.now();
    log('Worker EXEC start');
    try {
      const IR = 1;
      const idx2 = (i, j, dim1) => i + dim1 * j;
      const ipcl = state.IPCL ?? 6;
      const ipvee = state.IPVEE ?? 12;
      const ipphi = state.IPPHI ?? 8;
      const iprho = state.IPRHO ?? 13;
      const ipgee = state.IPGEE ?? 14;
      const ipmass = state.IPMASS ?? 20;
      const par = state.PARVAL || [];
      log(`EXEC inputs: CL=${par[idx2(ipcl, IR, state.IPTOT || 30)]} V=${par[idx2(ipvee, IR, state.IPTOT || 30)]} PHI=${par[idx2(ipphi, IR, state.IPTOT || 30)]} RHO=${par[idx2(iprho, IR, state.IPTOT || 30)]} GEE=${par[idx2(ipgee, IR, state.IPTOT || 30)]} MASS=${par[idx2(ipmass, IR, state.IPTOT || 30)]}`);
      log(`EXEC refs: SREF=${state.SREF} CREF=${state.CREF} BREF=${state.BREF}`);
      const ivmax = state.IVMAX || 0;
      const icmax = state.ICMAX || 0;
      if (state.ICON && state.CONVAL && ivmax && icmax) {
        const iValfa = state.IVALFA || 1;
        const iVbeta = state.IVBETA || 2;
        const iVrotx = state.IVROTX || 3;
        const iVroty = state.IVROTY || 4;
        const iVrotz = state.IVROTZ || 5;
        const icMap = [
          ['IVALFA', iValfa],
          ['IVBETA', iVbeta],
          ['IVROTX', iVrotx],
          ['IVROTY', iVroty],
          ['IVROTZ', iVrotz],
        ];
        const iconVals = icMap.map(([label, iv]) => {
          const ic = state.ICON[idx2(iv, IR, ivmax)];
          const cv = state.CONVAL[idx2(ic, IR, icmax)];
          return `${label}=>IC${ic} CON=${cv}`;
        });
        log(`EXEC constraints: ${iconVals.join(' | ')}`);
      }
    } catch (err) {
      log(`EXEC precheck failed: ${err?.message ?? err}`);
    }
    state.USE_WASM_SOLVE = Boolean(useWasm);
    state.USE_WASM_GAM = Boolean(useWasm);
    state.USE_WASM_AERO = Boolean(useWasm);
    state.USE_WASM_AIC = Boolean(useWasm);
    state.USE_WASM_LU = Boolean(useWasm);
    if (useWasm) {
      try {
        await preloadAoperLinSolveWasm();
        await preloadAsetupWasm();
        await preloadAeroWasm();
        await preloadAicWasmBridge();
        await preloadAsetpLuWasmBridge();
      } catch (err) {
        log(`EXEC wasm solve preload failed: ${err?.message ?? err}`);
      }
    }
    EXEC(state, 20, 0, 1);
    const dt = Date.now() - t0;
    log(`Worker EXEC done (${dt} ms)`);
    log(`EXEC state: ALFA=${state.ALFA} BETA=${state.BETA} MACH=${state.MACH}`);
    if (state.VINF && state.WROT) {
      log(`EXEC VINF=[${state.VINF[0]}, ${state.VINF[1]}, ${state.VINF[2]}] WROT=[${state.WROT[0]}, ${state.WROT[1]}, ${state.WROT[2]}]`);
    }
    const trefftz = {
      axis: 'Y',
      cref: state.CREF,
      strips: [],
      surfaces: [],
    };
    let badCnc = 0;
    let badCl = 0;
    let badClt = 0;
    let badDw = 0;
    const toFinite = (v, kind) => {
      if (Number.isFinite(v)) return v;
      if (kind === 'cnc') badCnc += 1;
      if (kind === 'cl') badCl += 1;
      if (kind === 'clt') badClt += 1;
      if (kind === 'dw') badDw += 1;
      return 0.0;
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
          const cnc = toFinite(state.CNC[j], 'cnc');
          const cl = toFinite(state.CLA_LSTRP[j], 'cl');
          const clPerp = toFinite(state.CLT_LSTRP[j], 'clt');
          const dw = toFinite(state.DWWAKE[j], 'dw');
          trefftz.strips.push([y, z, cnc, cl, clPerp, dw, n]);
        }
        trefftz.surfaces.push({ id: n, start, count: nj });
      }
    }
    if (badCnc || badCl || badClt || badDw) {
      log(`Trefftz non-finite: cnc=${badCnc} cl=${badCl} clt=${badClt} dw=${badDw}`);
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
    const rowD = (arr, row) => {
      if (!arr) return null;
      const out = [];
      for (let n = 0; n <= state.NDMAX; n += 1) out.push(arr[idx2(row, n, 3)]);
      return out;
    };
    const hinge = state.CHINGE ? (() => {
      const out = new Array(state.NCONTROL + 1).fill(0);
      for (let i = 1; i <= state.NCONTROL; i += 1) out[i] = state.CHINGE[i - 1] ?? 0.0;
      return out;
    })() : null;
    const eigen = await computeEigenmodes(state, Boolean(useWasm));
    postMessage({
      type: 'result',
      requestId,
      SREF: state.SREF,
      CREF: state.CREF,
      BREF: state.BREF,
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
      CFTOT_D: state.CFTOT_D ? [rowD(state.CFTOT_D, 0), rowD(state.CFTOT_D, 1), rowD(state.CFTOT_D, 2)] : null,
      CLTOT_U: state.CLTOT_U ? Array.from(state.CLTOT_U.slice(0, 6)) : null,
      CDTOT_U: state.CDTOT_U ? Array.from(state.CDTOT_U.slice(0, 6)) : null,
      CYTOT_U: state.CYTOT_U ? Array.from(state.CYTOT_U.slice(0, 6)) : null,
      CLTOT_D: state.CLTOT_D ? Array.from(state.CLTOT_D) : null,
      CDTOT_D: state.CDTOT_D ? Array.from(state.CDTOT_D) : null,
      CYTOT_D: state.CYTOT_D ? Array.from(state.CYTOT_D) : null,
      CMTOT_D: state.CMTOT_D ? [rowD(state.CMTOT_D, 0), rowD(state.CMTOT_D, 1), rowD(state.CMTOT_D, 2)] : null,
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
      RV: state.RV ? Array.from(state.RV) : null,
      ENSY: state.ENSY ? Array.from(state.ENSY) : null,
      ENSZ: state.ENSZ ? Array.from(state.ENSZ) : null,
      IJFRST: state.IJFRST ? Array.from(state.IJFRST) : null,
      NVSTRP: state.NVSTRP ? Array.from(state.NVSTRP) : null,
      JFRST: state.JFRST ? Array.from(state.JFRST) : null,
      NJ: state.NJ ? Array.from(state.NJ) : null,
      LFLOAD: state.LFLOAD ? Array.from(state.LFLOAD) : null,
      CDBDY: state.CDBDY ? Array.from(state.CDBDY) : null,
      CYBDY: state.CYBDY ? Array.from(state.CYBDY) : null,
      CLBDY: state.CLBDY ? Array.from(state.CLBDY) : null,
      CFBDY: state.CFBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CFBDY, ib)) : null,
      CMBDY: state.CMBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CMBDY, ib)) : null,
      CHINGE: hinge,
      JFRST: state.JFRST ? Array.from(state.JFRST) : null,
      NJ: state.NJ ? Array.from(state.NJ) : null,
      BETM: state.BETM,
      IYSYM: state.IYSYM,
      IZSYM: state.IZSYM,
      YSYM: state.YSYM,
      ZSYM: state.ZSYM,
      NVOR: state.NVOR,
      GAM: state.GAM ? Array.from(state.GAM.slice(0, state.NVOR + 1)) : null,
      RV1: state.RV1 ? Array.from(state.RV1.slice(0, 4 * (state.NVOR + 1))) : null,
      RV2: state.RV2 ? Array.from(state.RV2.slice(0, 4 * (state.NVOR + 1))) : null,
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
      SPANEF: state.SPANEF,
      TREFFTZ: trefftz,
      EIGEN: eigen,
    });
  } catch (err) {
    postMessage({ type: 'error', message: err?.message ?? String(err) });
  }
};
